library(terra)
library(countrycode)
library(nanoparquet)
library(dplyr)
library(readxl)

# --- Setup ---
a0_vect <- vect("data/bounds_a0.parquet")
a0 <- rast("data/results/bounds_a0.tif")
country_area <- read_parquet("data/results/mdata_area-lookup.parquet") |>
  subset(
    level == "admin0"
  )

plt_yr <- 2019

# --- Advisory Need  ---
inputs <- read.csv("data/aow1_digi-advisory/FAOSTAT_fert-inputs.csv")
inputs$iso3 <- countrycode(inputs$Area, "country.name", "iso3c")
inputs <- subset(inputs, Year == plt_yr)
input_use <- aggregate(data = inputs, Value ~ iso3, FUN = sum)
input_use$gaul0_code <- a0_vect$gaul0_code[
  match(input_use$iso3, a0_vect$iso3_code)
]
names(input_use) <- c("iso3", "input_use", "gaul0_code")

## Production Value
vop <- rast("data/aow1_digi-advisory/spam2010V2r0_global_total-vop_A.tif") |>
  project(a0)
vop_a0 <- zonal(vop, a0, fun = "sum", na.rm = TRUE)
names(vop_a0) <- c("gaul0_code", "vop")
vop_a0$vop <- vop_a0$vop / country_area$area_km2_ag

## Yield Gap
yg_files <- list.files(
  "data/aow1_digi-advisory/yieldgap/",
  full.names = TRUE,
  pattern = ".tif$"
)

gap_rast <- rast(yg_files) |>
  project(a0)

# gap_rast_norm <- mean(normalize_rast(gap_rast), na.rm = TRUE)
a0_yg_gaps <- zonal(gap_rast, a0, fun = "mean", na.rm = TRUE)
a0_yg_gaps <- a0_yg_gaps |>
  mutate(across(
    -gaul0_code,
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(total_gap = rowMeans(across(-gaul0_code), na.rm = TRUE))
# a0_yg_norm <- zonal(gap_rast_norm, a0, fun = "mean", na.rm = TRUE)
# a0_yg_gaps$gap_index <- a0_yg_norm$mean

## merge indices
aow1_need <- merge(
  vop_a0,
  a0_yg_gaps[c("gaul0_code", "total_gap")],
  by = "gaul0_code"
)

aow1_need <- merge(input_use, aow1_need, by = "gaul0_code")

aow1_need <- aow1_need |>
  mutate(across(
    -c(gaul0_code, iso3),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(
    need_idx = rowMeans(across(-c(gaul0_code, iso3)))
  )


# --- Delivery Capacity  ---

## Weather Station Density
wstation_rast <- rast(
  "data/aow1_digi-advisory/weather_station_density_k10.tif"
) |>
  project(a0)
wstation_a0 <- zonal(wstation_rast, a0, fun = "mean", na.rm = TRUE)
names(wstation_a0) <- c("gaul0_code", "wstation_density")

## Education Attainment
education_rast <- rast("data/urgency/IHME_eduAttain.tif") |>
  project(a0)
education_a0 <- zonal(education_rast, a0, fun = "mean", na.rm = TRUE)
names(education_a0) <- c("gaul0_code", "edu_attain")

## Internet Speed
ookla_rast <- rast("data/aow1_digi-advisory/ookla_mobileInternet.tif") |>
  project(a0)
ookla_a0 <- zonal(ookla_rast, a0, fun = "mean", na.rm = TRUE)
names(ookla_a0) <- c("gaul0_code", "ookla_d_speed")

## EBA/Business Ease
eba_df <- read.csv(
  "data/aow1_digi-advisory/EBA-enablebusiness.csv",
  skip = 1
)[c("Economy", "Overall.Score")]
names(eba_df) <- c("country", "EBA_score")
eba_df$iso3c <- countrycode(eba_df$country, "country.name", "iso3c")
eba_df$gaul0_code <- a0_vect$gaul0_code[
  match(eba_df$iso3c, a0_vect$iso3_code)
]

## Network Readiness
net_ready_df <- read_excel(
  "data/aow1_digi-advisory/network_ready_index.xlsx",
  col_names = FALSE
)
names(net_ready_df) <- as.character(unlist(net_ready_df[136, ])) # 2nd to last row is col names...
net_ready_df <- net_ready_df[1:135, c("ISO3Code", "NRI.score")]
# subset(ISO3Code %in% initial_countries, select = c("ISO3Code", "NRI.score"))
net_ready_df$NRI.score <- as.numeric(net_ready_df$NRI.score)
names(net_ready_df) <- c("iso3c", "NRI_score")
net_ready_df$gaul0_code <- a0_vect$gaul0_code[
  match(net_ready_df$iso3c, a0_vect$iso3_code)
]
## merge indices
eba_df <- eba_df[c("gaul0_code", "EBA_score")]
net_ready_df <- net_ready_df[c("gaul0_code", "NRI_score")]

aow1_digi <- Reduce(
  function(x, y) merge(x, y, by = "gaul0_code", all.x = TRUE),
  list(
    wstation_a0,
    education_a0,
    ookla_a0,
    eba_df,
    net_ready_df
  )
)
aow1_digi$iso3c <- a0_vect$iso3_code[match(
  aow1_digi$gaul0_code,
  a0_vect$gaul0_code
)]
aow1_digi <- aow1_digi |>
  mutate(across(
    -c(gaul0_code, iso3c),
    ~ {
      rng <- range(.x, na.rm = TRUE)
      if (diff(rng) == 0) 0 else (.x - rng[1]) / diff(rng)
    }
  )) |>
  mutate(capacity_idx = rowMeans(across(-c(gaul0_code, iso3c)), na.rm = TRUE))

# --- Final Results ---

write_parquet(aow1_need, "data/results/aow1_need.parquet")
write_parquet(aow1_digi, "data/results/aow1_digi.parquet")
