library(terra)
library(nanoparquet)

# --- Constants  ---
TLU_THRESHOLD <- 0.4 # TLU threshold for livestock areas
CROPLAND_THRESHOLD <- 10 # Minimum % cropland per cell

# --- Boundarys and zonal rasters  ---
a0_vect <- vect("data/bounds_a0.parquet")
a0_vect_simple <- vect("data/a0_vect_vsimple.topojson")
tlu_mask <- rast("data/tlu.tif") > TLU_THRESHOLD
cropland_rast <- rast("data/Global_cropland_3km_2019.tif")
cropland_mask <- project(cropland_rast, tlu_mask) > CROPLAND_THRESHOLD
ag_cells <- cropland_mask | tlu_mask # keep cells with either or both

# Dixon
farm_sys <- rast("data/bounds_farmSys.tif")
names(levels(farm_sys)[[1]]) <- c("value", "land_use")

farm_sys <- crop(farm_sys, a0_vect, mask = TRUE) # mask global rast to get region area
ag_cells <- crop(ag_cells, a0_vect, mask = TRUE)
farm_sys_lvl <- levels(farm_sys)[[1]]

cell_area <- cellSize(farm_sys, unit = "km")
cell_area_ag <- mask(cell_area, ag_cells, maskvalue = FALSE)
# a0_vect <- subset(
#   a0_vect,
#   iso3_code %in% initial_countries,
#   NSE = TRUE # required by terra for non quoted columns
# )

a0 <- rasterize(a0_vect, farm_sys, field = "gaul0_code")


# --- Zonal Extractions for Area an Ag Area  ---
country_area <- zonal(cell_area, a0, fun = "sum", na.rm = TRUE)
colnames(country_area) <- c("country_id", "area_km2")
country_area_ag <- zonal(cell_area_ag, a0, fun = "sum", na.rm = TRUE)
colnames(country_area_ag) <- c("country_id", "area_km2_ag")
country_area <- merge(country_area, country_area_ag, by = "country_id")
country_area$level <- "admin0"

farm_sys_area <- zonal(cell_area, farm_sys, fun = "sum", na.rm = TRUE)
colnames(farm_sys_area) <- c("farm_sys_id", "area_km2")
farm_sys_area_ag <- zonal(cell_area_ag, farm_sys, fun = "sum", na.rm = TRUE)
colnames(farm_sys_area_ag) <- c("farm_sys_id", "area_km2_ag")
farm_sys_area <- merge(farm_sys_area, farm_sys_area_ag, by = "farm_sys_id")
farm_sys_area$level <- "farmSys"


a0_sys <- a0 * 1e4 + farm_sys # numeric encode much faster than categorical
names(a0_sys) <- "a0_sys_id"
a0_sys_area <- zonal(cell_area, a0_sys, fun = "sum", na.rm = TRUE)
colnames(a0_sys_area) <- c("a0_sys_id", "area_km2")
a0_sys_area_ag <- zonal(cell_area_ag, a0_sys, fun = "sum", na.rm = TRUE)
colnames(a0_sys_area_ag) <- c("a0_sys_id", "area_km2_ag")
a0_sys_area <- merge(a0_sys_area, a0_sys_area_ag, by = "a0_sys_id")
a0_sys_area$level <- "a0_farmSys"

area_lookup <- do.call(
  rbind,
  lapply(list(country_area, farm_sys_area, a0_sys_area), \(df) {
    names(df)[1] <- "ID"
    df
  })
)

a0_sys_area$pct_area_ag <- with(a0_sys_area, (area_km2_ag / area_km2) * 100)

# Decode country-system combinations
a0_sys_area$a0 <- a0_sys_area$a0_sys_id %/% 1e4 # integer division to get a0 gaul code
a0_sys_area$sys <- as.character(a0_sys_area$a0_sys_id %% 1e4) # modulo to reverse sys

a0_sys_area$sys_label <- farm_sys_lvl[
  match(a0_sys_area$sys, farm_sys_lvl$value),
  "land_use"
]

a0_sys_area$a0_label <- a0_vect$gaul0_name[match(
  a0_sys_area$a0,
  a0_vect$gaul0_code
)]
#
# sys_labels <- setNames(
#   gsub("-|.(?=\\()", " ", farm_sys_lvl$land_use, perl = TRUE),
#   as.character(farm_sys_lvl$value)
# )

# --- Lookup DF  ---
a0_lookup <- as.data.frame(a0_vect[c("gaul0_code", "iso3_code", "gaul0_name")])

# farm_sys_lvl

# --- Write Results ---

writeRaster(a0, "data/results/bounds_a0.tif", overwrite = TRUE)
writeRaster(farm_sys, "data/results/bounds_farmsys.tif", overwrite = TRUE)
writeRaster(a0_sys, "data/results/bounds_a0-farmsys.tif", overwrite = TRUE)
writeRaster(ag_cells, "data/results/bounds_ag-cells.tif", overwrite = TRUE)
writeRaster(cell_area_ag, "data/results/bounds_ag-area.tif", overwrite = TRUE)
write_parquet(a0_sys_area, "data/results/bounds_area.parquet")
write_parquet(area_lookup, "data/results/mdata_area-lookup.parquet")
