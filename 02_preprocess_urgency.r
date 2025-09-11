library(terra)
library(jsonlite)
library(nanoparquet)

a0 <- rast("data/results/bounds_a0.tif")
farm_sys <- rast("data/results/bounds_farmsys.tif")
farm_sys_lvl <- levels(farm_sys)[[1]]
a0_sys <- rast("data/results/bounds_a0-farmsys.tif")
ag_cells <- rast("data/results/bounds_ag-cells.tif")
cell_area_ag <- rast("data/results/bounds_ag-area.tif")
area_lookup <- read_parquet("data/results/mdata_area-lookup.parquet")

min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

prepare_rast <- function(
  obj,
  base_rast,
  data_dir = "data/urgency",
  norm = FALSE,
  classify = FALSE,
  area_mask = NULL
) {
  path <- file.path(data_dir, obj$path)
  var_rast <- rast(path)
  var_name <- obj$var_name
  var_rast <- project(var_rast, base_rast)
  if (!is.null(area_mask)) {
    var_rast <- mask(var_rast, area_mask, maskvalue = FALSE)
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
  # if (obj$inverse) {
  #   mnmx <- terra::minmax(var_rast, compute = TRUE)
  #   var_rast <- mnmx[1, ] + mnmx[2, ] - var_rast
  # }
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
  stat_fn = "mean"
) {
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
  sys_values <- as.character(as.numeric(df$id[a0_sys_mask]) %% 1e4)

  df$land_use[a0_sys_mask] <- farm_sys_lvl[
    match(sys_values, farm_sys_lvl$value),
    "land_use"
  ]
  #
  # df$land_use[a0_sys_mask] <- sys_labels[as.character(
  #   as.numeric(df$id[a0_sys_mask]) %% 1e4
  # )]

  return(df)
}


rowGeometricMeans <- function(df, na.rm = TRUE, eps = 1e-9) {
  exp(rowMeans(log(df + eps), na.rm = na.rm)) # avoid log(0)
}

process_urgency_data <- function(
  vars,
  area_based = TRUE,
  classify_raster = TRUE,
  classify_df = FALSE
) {
  # Set classify parameter based on area_based unless overridden
  index_tables <- lapply(vars, function(x) {
    rast_data <- prepare_rast(
      x,
      farm_sys,
      area_mask = ag_cells,
      classify = classify_raster
    )

    # Set aggregation statistic
    if (!area_based) {
      x$agg_stat <- "mean"
    }
    extract_fn <- x$agg_stat

    # Handle pixel area calculations
    if (!is.null(x$thresholds) && x$agg_stat == "pixel_area") {
      level <- ifelse(x$inverse, 0, length(x$thresholds) - 1)
      high_mask <- rast_data == level
      rast_data <- mask(
        cell_area_ag,
        high_mask,
        maskvalue = c(FALSE, NA),
        # mask values = 0 bc 0 km is in high concern, NA would make index mean incorrect
        updatevalue = 0
      )
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
  group_vec <- unlist(lapply(vars, \(x) setNames(x$group, x$var_name)))

  # Get normalized columns and their base names
  norm_cols <- grep("_norm$", names(merged_df), value = TRUE)
  var_names <- sub("_norm$", "", norm_cols)

  # Assign groups to norm columns
  groups <- group_vec[var_names]
  names(groups) <- norm_cols

  # merged_df$vuln_index <- rowMeans(
  #   merged_df[, names(groups[groups == "vulnerability"])],
  #   na.rm = TRUE
  # )

  merged_df$vuln_index <- min_max_norm(rowGeometricMeans(
    merged_df[, names(groups[groups == "vulnerability"])],
    na.rm = TRUE
  ))

  merged_df$vuln_index_norm <- with(
    merged_df,
    ave(vuln_index, boundary_id, FUN = min_max_norm)
  )

  # merged_df$expo_index <- rowMeans(
  #   merged_df[, names(groups[groups == "exposure"])],
  #   na.rm = TRUE
  # )

  merged_df$expo_index <- rowGeometricMeans(
    merged_df[, names(groups[groups == "exposure"])],
    na.rm = TRUE
  )

  merged_df$expo_index_norm <- with(
    merged_df,
    ave(expo_index, boundary_id, FUN = min_max_norm)
  )

  # merged_df$composite_index <- rowMeans(
  #   merged_df[, c("vuln_index", "expo_index")],
  #   na.rm = TRUE
  # )

  merged_df$composite_index <- rowMeans(
    merged_df[, c("vuln_index_norm", "expo_index_norm")],
    na.rm = TRUE
  )

  # Add geographic and land use information
  merged_df <- add_geographic_info(merged_df)

  return(merged_df)
}


# --- Main  ---
urgency_mdata <- read_json("data/urgency.json")

urgency_df <- process_urgency_data(
  urgency_mdata$variables,
  area_based = TRUE, # When thresolds are provided, use pixel area in high category for idx
  classify_raster = TRUE, # When thresolds are provided, classify cells into low, mid, high
  classify_df = FALSE # Normalize and classify df post extraction into low, mid, high
)
urgency_df |> write_parquet("data/results/urgency.parquet")
#
# # --- A0 Tests  ---
# a0_vect <- vect("data/bounds_a0.parquet")
# urgency_a0 <- subset(urgency_df, boundary_id == "gaul0_code")
# urgency_a0$iso3c <- a0_vect$iso3_code[match(
#   urgency_a0$gaul0_code,
#   a0_vect$gaul0_code
# )]
# urgency_a0 |> head()
# #
# DT::datatable(
#   urgency_a0,
#   options = list(scrollY = TRUE, autoWidth = FALSE, scrollX = TRUE)
# )
