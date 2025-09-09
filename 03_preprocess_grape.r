library(readxl)
library(nanoparquet)
library(terra)

plt_yr <- 2019

grape_df <- readxl::read_xlsx("data/fin-cap/grape_v1.0.0.xlsx")
supp_data <- readxl::read_xlsx("data/fin-cap/grape_macro_db_v1.0.0.xlsx")
urgency_df <- read_parquet("data/results/urgency.parquet")
a0_df <- subset(urgency_df, boundary_id == "gaul0_code")
a0_vect <- vect("data/bounds_a0.parquet")
a0_df$iso3c <- a0_vect$iso3_code[match(a0_df$gaul0_code, a0_vect$gaul0_code)]

# grape_df <- subset(grape_df, iso3c %in% initial_countries)
supp_data <- supp_data[c(
  # subset(supp_data, iso3c %in% initial_countries)[c(
  "iso3c",
  "year",
  "ag_gdp_ppp",
  "population",
  "rural_pop_share"
)]
supp_data$rural_pop <- supp_data$rural_pop_share * supp_data$population
grape_df <- merge(grape_df, supp_data, by = c("iso3c", "year"))

grape_hr <- subset(grape_df, variable == "HR")
grape_hr$hr_per_rpop <- grape_hr$value / grape_hr$rural_pop # high = better

grape_rd <- subset(grape_df, variable == "RD")
grape_rd$rd_pct_gdp <- (grape_rd$value / grape_rd$ag_gdp_ppp) * 100 # high = better

merge_cols <- c("iso3c", "year")
grape_plt <- merge(
  grape_hr[c(merge_cols, "hr_per_rpop")],
  grape_rd[c(merge_cols, "rd_pct_gdp")],
  by = merge_cols
)

min_max <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

grape_plt$fincap_index <- ((min_max(grape_plt$hr_per_rpop) +
  min_max(grape_plt$rd_pct_gdp)) /
  2)

grape_plt <- subset(grape_plt, year == plt_yr & iso3c %in% a0_vect$iso3_code)

grape_plt$urgency <- a0_df[
  match(grape_plt$iso3c, a0_df$iso3c),
]$composite_index

urgency_order <- order(grape_plt$urgency, decreasing = TRUE)
grape_plt$iso3_factor <- factor(
  grape_plt$iso3c,
  levels = grape_plt$iso3c[order(grape_plt$urgency, decreasing = TRUE)]
)

write_parquet(grape_plt, "data/results/grape.parquet")
