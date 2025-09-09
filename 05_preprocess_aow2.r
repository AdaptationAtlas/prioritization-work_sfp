library(terra)
library(nanoparquet)
library(dplyr)

a0_vect <- vect("data/bounds_a0.parquet")
a0 <- rast("data/results/bounds_a0.tif")
country_area <- read_parquet("data/results/mdata_area-lookup.parquet") |>
  subset(
    level == "admin0"
  )

# --- Shock Exposure ---
## Rain Variability
chirps_cv <- rast(
  "data/aow2_shocks/precip_cv_historical_mean.tif"
) |>
  project(a0)
chirps_cv_a0 <- zonal(chirps_cv, a0, fun = "mean", na.rm = TRUE)
names(chirps_cv_a0) <- c("gaul0_code", "chirps_cv")

## Drought
spei6 <- rast("data/aow2_shocks/spei6_severity.tif") |> # unit = yr where spei < -1
  project(a0)
spei6_a0 <- zonal(spei6, a0, fun = "mean", na.rm = TRUE)
names(spei6_a0) <- c("gaul0_code", "spei6")

## Flood
flood <- rast("data/aow2_shocks/floodMapGL_rp20y.tif") |> # unit = meter
  project(a0)
flood_a0 <- zonal(flood, a0, fun = "mean", na.rm = TRUE)
names(flood_a0) <- c("gaul0_code", "flood")

## Pest and Disease
rhor_files <- list.files(
  "data/aow2_shocks/ssp370_2100_pest_yield_impact_pct",
  full.names = TRUE
)
rhor_rast <- rast(rhor_files) |>
  project(a0)
crops <- lapply(strsplit(names(rhor_rast), "_"), `[`, 3)
names(rhor_rast) <- crops

rhor_a0 <- zonal(rhor_rast, a0, fun = "mean", na.rm = TRUE)
rhor_a0$avg_pest_impact <- rowMeans(rhor_a0[unlist(crops)], na.rm = TRUE)

aow2_shocks <- Reduce(
  \(x, y) merge(x, y, by = "gaul0_code", all.x = TRUE),
  list(
    rhor_a0,
    flood_a0,
    spei6_a0,
    chirps_cv_a0
  )
)
aow2_shocks$iso3c <- a0_vect[
  match(aow2_shocks$gaul0_code, a0_vect$gaul0_code),
]$iso3_code

aow2_shocks_idx <- aow2_shocks |>
  mutate(across(
    -c(gaul0_code, iso3c),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(shock_idx = rowMeans(across(-c(gaul0_code, iso3c)), na.rm = TRUE))


# --- Resilience Index  ---

## Internet
ookla_rast <- rast("data/aow1_digi-advisory/ookla_mobileInternet.tif") |>
  project(a0)
ookla_a0 <- zonal(ookla_rast, a0, fun = "mean", na.rm = TRUE)
names(ookla_a0) <- c("gaul0_code", "ookla_d_speed")

## Poverty
grdi_rast <- rast("data/aow2_shocks/grdi_povmap.tif") |>
  project(a0)
grdi_a0 <- zonal(grdi_rast, a0, fun = "mean", na.rm = TRUE)
names(grdi_a0) <- c("gaul0_code", "grdi")
# Invert to match the other indicators where high = positive resiliance
grdi_a0$grdi <- with(
  grdi_a0,
  (max(grdi, na.rm = TRUE) + min(grdi, na.rm = TRUE)) - grdi
)

## Finance services
wb_findex_accounts <- read.csv(
  "data/aow2_shocks/WB_FINDEX_ACCOUNT-OWNERSHIP.csv"
) |>
  subset(
    # REF_AREA %in%
    # initial_countries &
    SEX == "_T" &
      AGE == "Y_GE15" &
      URBANISATION == "RUR" &
      TOTAL == "_T",
    select = c(REF_AREA, X2024)
  )
names(wb_findex_accounts) <- c("iso3c", "pct_rpop_bank_accounts")
wb_findex_accounts$gaul0_code <- a0_vect$gaul0_code[
  match(wb_findex_accounts$iso3c, a0_vect$iso3_code)
]
## CIS ?????

aow2_resilience <- Reduce(
  \(x, y) merge(x, y, by = "gaul0_code", all.x = TRUE),
  list(
    ookla_a0,
    grdi_a0,
    wb_findex_accounts[, -1]
  )
)
aow2_resilience$iso3c <- a0_vect[
  match(aow2_resilience$gaul0_code, a0_vect$gaul0_code),
]$iso3_code

aow2_resilience_idx <- aow2_resilience |>
  mutate(across(
    -c(gaul0_code, iso3c),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(resilience_idx = rowMeans(across(-c(gaul0_code, iso3c)), na.rm = TRUE))

# --- Results ---
write_parquet(aow2_shocks_idx, "data/results/aow2_shocks.parquet")
write_parquet(aow2_resilience_idx, "data/results/aow2_resilience.parquet")
