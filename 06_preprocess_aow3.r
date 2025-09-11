library(terra)
library(dplyr)
library(nanoparquet)

# --- Setup ---
a0_vect <- vect("data/bounds_a0.parquet")
a0 <- rast("data/results/bounds_a0.tif")
ag_area <- rast("data/results/bounds_ag-area.tif")
a0_areas <- read_parquet("data/results/bounds_country-area.parquet")

plt_yr <- 2019

COUNTRY_SUBSET <- TRUE
initial_countries <- read.csv("data/initial_prioritization.csv")$ISO3
countries <- if (COUNTRY_SUBSET) initial_countries else a0_vect$iso3_code

# --- Constraint Burden ---

## Soil PH
ph_threshols <- c(5, 7.5) # unit = pH - below 5 is poor, above 7.5 is poor
ph_files <- list.files(
  "data/aow3_constraints",
  full.names = TRUE,
  pattern = "phh2o"
)
ph_rast <- sprc(ph_files) |>
  merge() |>
  project(a0) # Saved in 2 tiles so merge together

ph_mask <- ph_rast < ph_threshols[1] | ph_rast > ph_threshols[2]

ph_area <- mask(ag_area, ph_mask, maskvalue = c(FALSE, NA))
ph_area[is.na(ph_area)] <- 0
ph_a0 <- zonal(ph_area, a0, fun = "sum", na.rm = TRUE)

names(ph_a0) <- c("gaul0_code", "bad_ph_area")

## Soil Health (SOC)
soc_files <- list.files(
  "data/aow3_constraints",
  full.names = TRUE,
  pattern = "soc"
)
soc_rast <- sprc(soc_files) |>
  merge() |>
  project(a0)
mnmx <- minmax(soc_rast, compute = TRUE)
soc_rast <- mnmx[2, ] + mnmx[1, ] - soc_rast

soc_a0 <- zonal(soc_rast, a0, fun = "mean", na.rm = TRUE)
names(soc_a0) <- c("gaul0_code", "soc_invserse")

## Crop Diversity (shannon diversity)
cropgrid_diversity <- rast("data/aow3_constraints/shannon_diversity.tif") |>
  project(a0)
diversity_a0 <- zonal(cropgrid_diversity, a0, fun = "mean", na.rm = TRUE)
names(diversity_a0) <- c("gaul0_code", "diversity")
diversity_a0$crop_diversity <- with(
  # inverse as more diversity = positive
  diversity_a0,
  (max(diversity, na.rm = TRUE) + min(diversity, na.rm = TRUE)) - diversity
)

## Soil Degradation

## Gren Water Scarcity
gws_rast <- rast("data/aow3_constraints/green-water-scarcity_baseline.tif") |>
  project(a0)
gws_a0 <- zonal(gws_rast, a0, fun = "mean", na.rm = TRUE)
names(gws_a0) <- c("gaul0_code", "gws")

## aow3 - constraints index
aow3_constraints <- Reduce(
  \(x, y) merge(x, y, by = "gaul0_code", all.x = TRUE),
  list(
    ph_a0,
    soc_a0,
    diversity_a0,
    gws_a0
  )
)

aow3_constraints$iso3c <- a0_vect[
  match(aow3_constraints$gaul0_code, a0_vect$gaul0_code),
]$iso3_code

aow3_constraints_idx <- aow3_constraints |>
  mutate(across(
    -c(gaul0_code, iso3c),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(
    constraint_idx = rowMeans(across(-c(gaul0_code, iso3c)), na.rm = TRUE)
  )

# --- Enabling Environment ---

## Market Access
market_rast <- rast("data/aow3_constraints/nelson_marketTime.tif") |>
  project(a0)

market_a0 <- zonal(market_rast, a0, fun = "mean", na.rm = TRUE)
names(market_a0) <- c("gaul0_code", "market_access")

## Mechanization
mech_df <- read.csv(
  "data/aow3_constraints/machinery-per-agricultural-land.csv"
)
west_af_val <- mech_df[
  mech_df$Entity == "West Africa" & mech_df$Year == plt_yr,
  "machinery_per_ag_land"
]
civ_mech_df <- data.frame(Code = "CIV", machinery_per_ag_land = west_af_val)
mech_df <- mech_df |>
  subset(
    Year == plt_yr & Code %in% countries,
    select = c("Code", "machinery_per_ag_land")
  )
mech_df <- rbind(mech_df, civ_mech_df)

mech_df$gaul0_code <- a0_vect$gaul0_code[
  match(mech_df$Code, a0_vect$iso3_code)
]
names(mech_df) <- c("iso3c", "machinery_per_ag_land", "gaul0_code")

rpop_density <- rast("data/urgency/wpop_rural_pop_density.tif") |>
  project(a0)

rpop_density_a0 <- zonal(rpop_density, a0, fun = "mean", na.rm = TRUE)
names(rpop_density_a0) <- c("gaul0_code", "rpop_density")

## Finance services
wb_findex_accounts <- read.csv(
  "data/aow2_shocks/WB_FINDEX_ACCOUNT-OWNERSHIP.csv"
) |>
  subset(
    REF_AREA %in%
      countries &
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

## Index

aow3_capacity <- Reduce(
  \(x, y) merge(x, y, by = "gaul0_code", all.x = TRUE),
  list(
    rpop_density_a0,
    mech_df,
    market_a0,
    wb_findex_accounts[, -1]
  )
)

aow3_capacity_idx <- aow3_capacity |>
  mutate(across(
    -c(gaul0_code, iso3c),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(capacity_idx = rowMeans(across(-c(gaul0_code, iso3c)), na.rm = TRUE))

# --- Results ---

write_parquet(aow3_constraints_idx, "data/results/aow3_constraints.parquet")
write_parquet(aow3_capacity_idx, "data/results/aow3_capacity.parquet")
