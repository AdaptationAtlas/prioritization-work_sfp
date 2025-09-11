---
title: "sfp_report"
---

```{r, setup = true}
library(terra)
library(tidyr)
library(ggplot2)
library(jsonlite)
```

```{r}
a0_vect <- vect("bounds_a0.parquet")
tlu_mask <- rast("tlu.tif") > 0.4 # tlu per cell
cropland_rast <- rast("Global_cropland_3km_2019.tif")
cropland_mask <- project(cropland_rast, tlu_mask) > 10 # 10% cell = crop
ag_cells <- cropland_mask | tlu_mask # keep cells with either or both

# Ag system dataset 1:
# https://data.apps.fao.org/catalog/iso/4e463d70-7593-11db-b9b2-000d939bc5d8
# farm_sys <- rast("fao_fggd_farmsys.tif")
# names(levels(farm_sys)[[1]]) <- c("value", "land_use")
# farm_sys <- subst(farm_sys, c(-997, 8), NA, raw = TRUE) # remove waterbodies
# farm_sys <- droplevels(farm_sys)

# Ag system dataset 2:
# https://data.apps.fao.org/catalog/iso/c9be830e-daf5-4926-bbf6-0051ad057c53
# farm_sys <- rast("fao_farmsys.tif")

# Ag system dataset 3:
# Dixon
farm_sys <- rast("bounds_farmSys.tif")
names(levels(farm_sys)[[1]]) <- c("value", "land_use")

farm_sys <- crop(farm_sys, a0_vect, mask = TRUE) # mask global rast to get region area
ag_cells <- crop(ag_cells, a0_vect, mask = TRUE)
farm_sys_lvl <- levels(farm_sys)

cell_area <- cellSize(farm_sys, unit = "km")
cell_area_ag <- mask(cell_area, ag_cells, maskvalue = FALSE)
initial_countries <- read.csv("initial_prioritization.csv")$ISO3 # from Jan & Oscar
a0_prioritized <- subset(
  a0_vect,
  iso3_code %in% initial_countries,
  NSE = TRUE # required by terra for non quoted columns
)

a0 <- rasterize(a0_prioritized, farm_sys, field = "gaul0_code")
```

# Farm Sys X Country Area

```{r}
country_area <- zonal(cell_area, a0, fun = "sum", na.rm = TRUE)
colnames(country_area) <- c("country_id", "area_km2")
country_area_ag <- zonal(cell_area_ag, a0, fun = "sum", na.rm = TRUE)
colnames(country_area_ag) <- c("country_id", "area_km2_ag")
country_area <- merge(country_area, country_area_ag, by = "country_id")

farm_sys_area <- zonal(cell_area, farm_sys, fun = "sum", na.rm = TRUE)
colnames(farm_sys_area) <- c("farm_sys_id", "area_km2")
farm_sys_area_ag <- zonal(cell_area_ag, farm_sys, fun = "sum", na.rm = TRUE)
colnames(farm_sys_area_ag) <- c("farm_sys_id", "area_km2_ag")
farm_sys_area <- merge(farm_sys_area, farm_sys_area_ag, by = "farm_sys_id")

a0_sys <- a0 * 1e4 + farm_sys # numeric encode faster than categories
names(a0_sys) <- "a0_sys_id"
a0_sys_area <- zonal(cell_area, a0_sys, fun = "sum", na.rm = TRUE)
colnames(a0_sys_area) <- c("a0_sys_id", "area_km2")
a0_sys_area_ag <- zonal(cell_area_ag, a0_sys, fun = "sum", na.rm = TRUE)
colnames(a0_sys_area_ag) <- c("a0_sys_id", "area_km2_ag")
a0_sys_area <- merge(a0_sys_area, a0_sys_area_ag, by = "a0_sys_id")

area_lookup <- do.call(
  rbind,
  lapply(list(country_area, farm_sys_area, a0_sys_area), \(df) {
    names(df)[1] <- "ID"
    df
  })
)

a0_sys_area$pct_area_ag <- with(a0_sys_area, (area_km2_ag / area_km2) * 100)
a0_sys_area$a0 <- a0_sys_area$a0_sys_id %/% 1e4 # integer division to get a0
a0_sys_area$sys <- as.character(a0_sys_area$a0_sys_id %% 1e4) # modulo to reverse sys

sys_labels <- setNames(
  farm_sys_lvl[[1]]$land_use,
  as.character(farm_sys_lvl[[1]]$value)
)

a0_labels <- setNames(a0_prioritized$gaul0_name, a0_prioritized$gaul0_code)

# Get top systems
sys_totals <- aggregate(
  area_km2_ag ~ sys,
  data = a0_sys_area,
  sum,
  na.rm = TRUE
)
top_sys <- head(sys_totals[order(-sys_totals$area_km2_ag), "sys"], 15)
top_sys_names <- sys_labels[top_sys]

area_plot_data <- subset(a0_sys_area, sys %in% top_sys)

area_plot_data |>
  complete(
    a0 = unique(area_plot_data$a0), # fill missing values for pretty plot
    sys = unique(area_plot_data$sys)
  ) |>
  ggplot(aes(x = factor(sys), y = factor(a0), fill = area_km2_ag)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  scale_fill_viridis_c(name = "Area (km²)", na.value = "#eeee") +
  labs(
    x = "System code (sys)",
    y = "Country code (a0)",
    title = "Area by Country and System"
  ) +
  scale_x_discrete(labels = scales::label_wrap(10)(sys_labels)) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal()
```

```{r, setup = true}

prepare_rast <- function(
    obj,
    base_rast,
    data_dir = "urgency",
    norm = FALSE,
    classify = FALSE,
    mask = NULL) {
  path <- file.path(data_dir, obj$path)
  var_rast <- rast(path)
  var_name <- obj$var_name
  var_rast <- project(var_rast, base_rast)
  if (!is.null(mask)) {
    var_rast <- mask(var_rast, mask, maskvalue = FALSE)
  }
  if (!is.null(obj$thresholds) && classify) {
    thresholds <- as.numeric(unlist(obj$thresholds))
    var_rast <- classify(var_rast, c(thresholds, Inf))
    if (norm) {
      x_min <- 0 # Normalize between thresholds to not loose range
      x_max <- length(obj$thresholds) - 1 # index starting from 0
      var_rast <- (var_rast - x_min) / (x_max - x_min)
    }
  } else if (norm) {
    mnmx <- terra::minmax(var_rast, compute = TRUE)
    x_min <- mnmx[1]
    x_max <- mnmx[2]
    var_rast <- (var_rast - x_min) / (x_max - x_min)
  }
  if (obj$inverse) {
    var_rast <- max(var_rast, na.rm = T) + min(var_rast, na.rm = T) - var_rast
  }
  if (length(names(var_rast)) == 1) {
    names(var_rast) <- var_name
    varnames(var_rast) <- var_name
  } else {
    names(var_rast) <- paste0(var_name, "_", names(var_rast))
    varnames(var_rast) <- names(var_rast)
  }
  return(var_rast)
}

zonal_extract <- function(
    data_rast,
    zone_rasts,
    stat_fn = "mean") {
  admin_results <- lapply(seq_along(zone_rasts), function(i) {
    zone_rast <- zone_rasts[[i]]
    df <- terra::zonal(data_rast, zone_rast, fun = stat_fn, na.rm = TRUE)
    df$boundary_id <- names(zone_rast)
    df$id <- df[, 1]
    return(df[2:4])
  })
  admin_df <- do.call(rbind, admin_results)
  return(admin_df)
}

df_classify_norm <- function(df, obj, norm = FALSE, classify = FALSE) {
  var_name <- obj$var_name
  if (!is.null(obj$thresholds) && classify) {
    thresholds <- as.numeric(unlist(obj$thresholds))
    var_classed <- paste0(var_name, "_classed")
    df[var_classed] <- cut(
      df[[var_name]],
      c(thresholds, Inf)
    )
    if (norm) {
      var_norm <- paste0(var_name, "_norm")
      x_min <- 1
      x_max <- length(thresholds)
      df[var_norm] <- (as.numeric(df[[var_classed]]) - x_min) /
        (x_max - x_min)
    }
  } else if (norm) {
    var_norm <- paste0(var_name, "_norm")
    x_min <- min(df[[var_name]], na.rm = TRUE)
    x_max <- max(df[[var_name]], na.rm = TRUE)
    df[var_norm] <- (as.numeric(df[[var_name]]) - x_min) /
      (x_max - x_min)
  }
  if (obj$inverse && norm && obj$agg_stat != "pixel_area") {
    var_norm <- paste0(var_name, "_norm")
    df[[var_norm]] <- max(df[[var_norm]], na.rm = TRUE) +
      min(df[[var_norm]], na.rm = TRUE) -
      df[[var_norm]]
  }
  return(df)
}

add_geographic_info <- function(df) {
  df$gaul0_code <- ifelse(df$boundary_id == "gaul0_code", df$id, NA)
  df$land_use <- ifelse(df$boundary_id == "land_use", df$id, NA)

  # Handle a0_sys_id boundary
  a0_sys_mask <- df$boundary_id == "a0_sys_id"
  df$gaul0_code[a0_sys_mask] <- as.numeric(df$id[a0_sys_mask]) %/% 1e4
  df$land_use[a0_sys_mask] <- sys_labels[as.character(
    as.numeric(df$id[a0_sys_mask]) %% 1e4
  )]

  return(df)
}

process_urgency_data <- function(vars, area_based = TRUE, classify_raster = TRUE, classify_df = FALSE) {
  # Set classify parameter based on area_based unless overridden
  index_tables <- lapply(vars, function(x) {
    rast_data <- prepare_rast(x, farm_sys, mask = ag_cells, classify = classify_raster)

    # Set aggregation statistic
    if (!area_based) {
      x$agg_stat <- "mean"
    }
    extract_fn <- x$agg_stat

    # Handle pixel area calculations
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      level <- ifelse(x$inverse, 0, length(x$thresholds) - 1)
      high_mask <- rast_data == level
      rast_data <- mask(cell_area_ag, high_mask, maskvalue = FALSE)
      names(rast_data) <- x$var_name
      extract_fn <- "sum"
    }

    # Extract and process data
    extract <- zonal_extract(rast_data, list(a0, farm_sys, a0_sys), extract_fn)

    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      extract$total_area_ag <- area_lookup[
        match(extract$id, area_lookup$ID),
        "area_km2"
      ]
      extract[[x$var_name]] <- extract[[x$var_name]] / extract$total_area_ag
      extract$total_area_ag <- NULL
    }

    df_classify_norm(extract, x, norm = TRUE, classify = classify_df)
  })

  # Merge all index tables
  merged_df <- Reduce(
    function(x, y) {
      merge(x, y, by = c("boundary_id", "id"))
    },
    index_tables
  )

  # Calculate composite index
  norm_cols <- grep("_norm$", names(merged_df), value = TRUE)
  merged_df$composite_index <- rowMeans(merged_df[, norm_cols], na.rm = TRUE)

  # Add geographic and land use information
  merged_df <- add_geographic_info(merged_df)

  return(merged_df)
}
```


```{r}
urgency_data <- read_json("urgency.json")
vars <- urgency_data$variables
area_based <- TRUE

merged_df <- process_urgency_data(vars, area_based = TRUE, classify_raster = TRUE, classify_df = FALSE)
index_tables <- lapply(
  vars,
  \(x) {
    rast_data <- prepare_rast(
      x,
      farm_sys,
      mask = ag_cells,
      classify = area_based
    )
    if (!area_based) {
      x$agg_stat <- "mean"
    }
    extract_fn <- x$agg_stat
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      level <- ifelse(x$inverse, 0, length(x$thresholds) - 1)
      high_mask <- rast_data == level
      rast_data <- mask(cell_area_ag, high_mask, maskvalue = FALSE)
      names(rast_data) <- x$var_name
      extract_fn <- "sum"
    }
    extract <- zonal_extract(rast_data, list(a0, farm_sys, a0_sys), extract_fn)
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      extract$total_area_ag <- area_lookup[
        match(extract$id, area_lookup$ID),
        "area_km2"
      ]
      extract[[x$var_name]] <- extract[[x$var_name]] / extract$total_area_ag
      extract$total_area_ag <- NULL
    }
    pre_idx <- df_classify_norm(extract, x, norm = TRUE, classify = !area_based)
  }
)

merged_df <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = c("boundary_id", "id"),
    )
  },
  index_tables
)

norm_cols <- grep("_norm$", names(merged_df), value = TRUE)

merged_df$composite_index <- rowMeans(merged_df[, norm_cols], na.rm = TRUE)

merged_df$gaul0_code <- ifelse(
  merged_df$boundary_id == "gaul0_code",
  merged_df$id,
  NA
)

merged_df$land_use <- ifelse(
  merged_df$boundary_id == "land_use",
  merged_df$id,
  NA
)

merged_df$gaul0_code <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  as.numeric(merged_df$id) %/% 1e4,
  merged_df$gaul0_code
)

merged_df$land_use <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  sys_labels[as.character(as.numeric(merged_df$id) %% 1e4)], # conversion due to factor index at 0
  merged_df$land_use
)

plot_data <- subset(
  merged_df,
  boundary_id == "a0_sys_id" & land_use %in% top_sys_names
)

plot_data |>
  complete(
    gaul0_code = unique(plot_data$gaul0_code), # fill missing values for pretty plot
    land_use = unique(plot_data$land_use)
  ) |>
  ggplot(aes(x = land_use, y = gaul0_code, fill = composite_index)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  scale_fill_viridis_c(name = "Urgency (0-1)", na.value = "#eeee") +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System"
  ) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal()

plot_data$area <- area_lookup[
  match(plot_data$id, area_lookup$ID),
  "area_km2_ag"
]

plot_data |>
  complete(
    gaul0_code = unique(plot_data$gaul0_code),
    land_use = unique(plot_data$land_use)
  ) |>
  ggplot(aes(
    x = land_use,
    y = gaul0_code,
    size = area,
    fill = composite_index
  )) +
  geom_point(shape = 21, color = "white", stroke = 1.2) +
  scale_fill_viridis_c(
    name = "Urgency (0–1)",
    na.value = "#eeee",
    direction = -1
  ) +
  scale_size_continuous(name = "Area", range = c(3, 12)) +
  guides(
    size = guide_legend(
      override.aes = list(
        shape = 21,
        fill = "grey70"
      )
    ),
    fill = guide_colorbar()
  ) +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System (Bubble Size = Area)"
  ) +
  scale_y_discrete(labels = a0_labels) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  theme_minimal() +
  theme(legend.box = "vertical")
```


## Non-area based approach

```{r}
area_based <- FALSE
index_tables2 <- lapply(
  vars,
  \(x) {
    rast_data <- prepare_rast(
      x,
      farm_sys,
      mask = ag_cells,
      classify = area_based
    )
    if (!area_based) {
      x$agg_stat <- "mean"
    }
    extract_fn <- x$agg_stat
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      level <- ifelse(x$inverse, 0, length(x$thresholds) - 1)
      high_mask <- rast_data == level
      rast_data <- mask(cell_area_ag, high_mask, maskvalue = FALSE)
      names(rast_data) <- x$var_name
      extract_fn <- "sum"
    }
    extract <- zonal_extract(rast_data, list(a0, farm_sys, a0_sys), extract_fn)
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      extract$total_area_ag <- area_lookup[
        match(extract$id, area_lookup$ID),
        "area_km2"
      ]
      extract[[x$var_name]] <- extract[[x$var_name]] / extract$total_area_ag
      extract$total_area_ag <- NULL
    }
    pre_idx <- df_classify_norm(extract, x, norm = TRUE, classify = !area_based)
  }
)


merged_df2 <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = c("boundary_id", "id"),
    )
  },
  index_tables2
)

norm_cols2 <- grep("_norm$", names(merged_df2), value = TRUE)

merged_df2$composite_index <- rowMeans(merged_df2[, norm_cols2], na.rm = TRUE)

merged_df2$gaul0_code <- ifelse(
  merged_df$boundary_id == "gaul0_code",
  merged_df$id,
  NA
)

merged_df2$land_use <- ifelse(
  merged_df$boundary_id == "land_use",
  merged_df$id,
  NA
)

merged_df2$gaul0_code <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  as.numeric(merged_df2$id) %/% 1e4,
  merged_df$gaul0_code
)

merged_df2$land_use <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  sys_labels[as.numeric(merged_df2$id) %% 1e4],
  merged_df$land_use
)

plot_data2 <- subset(merged_df2, boundary_id == "a0_sys_id")

plot_data2 |>
  complete(
    gaul0_code = unique(plot_data2$gaul0_code), # fill missing values for pretty plot
    land_use = unique(plot_data2$land_use)
  ) |>
  ggplot(aes(x = land_use, y = gaul0_code, fill = composite_index)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  scale_fill_viridis_c(name = "Urgency (0-1)", na.value = "#eeee") +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System"
  ) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal()


plot_data2$area <- area_lookup[
  match(plot_data3$id, area_lookup$ID),
  "area_km2_ag"
]

plot_data2 |>
  complete(
    gaul0_code = unique(plot_data3$gaul0_code),
    land_use = unique(plot_data3$land_use)
  ) |>
  ggplot(aes(
    x = land_use,
    y = gaul0_code,
    size = area,
    fill = composite_index
  )) +
  geom_point(shape = 21, color = "white", stroke = 1.2) +
  scale_fill_viridis_c(name = "Urgency (0–1)", na.value = "#eeee") +
  scale_size_continuous(name = "Area", range = c(3, 12)) +
  guides(
    size = guide_legend(
      override.aes = list(
        shape = 21,
        fill = "grey70"
      )
    ),
    fill = guide_colorbar()
  ) +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System (Bubble Size = Area)"
  ) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal() +
  theme(legend.box = "vertical")
```

## No threshold, just distribution
```{r}

area_based <- FALSE
index_tables3 <- lapply(
  vars,
  \(x) {
    rast_data <- prepare_rast(
      x,
      farm_sys,
      mask = ag_cells,
      classify = area_based
    )
    if (!area_based) {
      x$agg_stat <- "mean"
    }
    extract_fn <- x$agg_stat
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      level <- ifelse(x$inverse, 0, length(x$thresholds) - 1)
      high_mask <- rast_data == level
      rast_data <- mask(cell_area_ag, high_mask, maskvalue = FALSE)
      names(rast_data) <- x$var_name
      extract_fn <- "sum"
    }
    extract <- zonal_extract(rast_data, list(a0, farm_sys, a0_sys), extract_fn)
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      extract$total_area_ag <- area_lookup[
        match(extract$id, area_lookup$ID),
        "area_km2"
      ]
      extract[[x$var_name]] <- extract[[x$var_name]] / extract$total_area_ag
      extract$total_area_ag <- NULL
    }
    pre_idx <- df_classify_norm(extract, x, norm = TRUE, classify = FALSE)
  }
)


merged_df3 <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = c("boundary_id", "id"),
    )
  },
  index_tables3
)

norm_cols3 <- grep("_norm$", names(merged_df3), value = TRUE)

merged_df3$composite_index <- rowMeans(merged_df3[, norm_cols3], na.rm = TRUE)

merged_df3$gaul0_code <- ifelse(
  merged_df$boundary_id == "gaul0_code",
  merged_df$id,
  NA
)

merged_df3$land_use <- ifelse(
  merged_df$boundary_id == "land_use",
  merged_df$id,
  NA
)

merged_df3$gaul0_code <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  as.numeric(merged_df3$id) %/% 1e4,
  merged_df$gaul0_code
)

merged_df3$land_use <- ifelse(
  merged_df$boundary_id == "a0_sys_id",
  sys_labels[as.numeric(merged_df3$id) %% 1e4],
  merged_df$land_use
)

plot_data3 <- subset(merged_df3, boundary_id == "a0_sys_id")

plot_data3 |>
  complete(
    gaul0_code = unique(plot_data3$gaul0_code), # fill missing values for pretty plot
    land_use = unique(plot_data3$land_use)
  ) |>
  ggplot(aes(x = land_use, y = gaul0_code, fill = composite_index)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1) +
  scale_fill_viridis_c(name = "Urgency (0-1)", na.value = "#eeee") +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System"
  ) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal()

plot_data3$area <- area_lookup[match(plot_data3$id, area_lookup$ID), "area_km2"]

plot_data3 |>
  complete(
    gaul0_code = unique(plot_data3$gaul0_code),
    land_use = unique(plot_data3$land_use)
  ) |>
  ggplot(aes(
    x = land_use,
    y = gaul0_code,
    size = area,
    fill = composite_index
  )) +
  geom_point(shape = 21, color = "white", stroke = 1.2) + # shape 21 = filled circle
  scale_fill_viridis_c(name = "Urgency (0–1)", na.value = "#eeee") +
  scale_size_continuous(name = "Area", guide = "legend") +
  guides(
    size = guide_legend(
      override.aes = list(
        shape = 21,
        fill = "grey70"
      )
    ),
    fill = guide_colorbar()
  ) +
  labs(
    x = "System code (sys)",
    y = "Country",
    title = "Urgency by Country and System (Bubble Size = Area)"
  ) +
  scale_y_discrete(labels = a0_labels) +
  theme_minimal() +
  theme(legend.box = "vertical")
```
