# ============================================================
# SFP Program Prioritization – Composite Ranking & Quadrants
# - Composite = 0.5 * Urgency + 0.5 * Opportunity
# - Primary = Top 15 by Composite; Secondary = Bottom 15
# - Quadrants at 60th percentile for Urgency & Opportunity
# - Consistent colors; all world boundaries shown in maps
# ============================================================

suppressPackageStartupMessages({
  library(arrow)      # read_parquet()
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(ggrepel)
  library(ggforce)
  library(scales)
})

# -------------------------
# CONFIG
# -------------------------
CONFIG <- list(
  out_dir            = "outputs",
  world_path         = "data/a0_vect_vsimple.topojson",
  reach_weight       = 0.5,    # Opportunity = 0.5 Reach + 0.5 Readiness
  readiness_weight   = 0.5,
  readiness_weights  = c(      # must sum to 1.0
    aow1_cap = 0.25,           # digital/advisory capacity
    aow2_res = 0.25,           # resilience capacity
    aow3_cap = 0.25,           # constraints capacity
    grape_hr = 0.125,          # researchers per rural pop
    grape_spd= 0.125           # ag R&D spend / AgGDP
  ),
  high_pct_cutoff    = 0.60    # 60th percentile for High/Low classification
)
dir.create(CONFIG$out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------
# HELPERS
# -------------------------
rescale01 <- function(x) {
  if (all(is.na(x))) return(x)
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(0.5, length(x)))
  scales::rescale(x, to = c(0,1), from = rng)
}
fix_code_type <- function(df) {
  if ("gaul0_code" %in% names(df)) df$gaul0_code <- as.character(df$gaul0_code)
  df
}

# -------------------------
# LOAD WORLD BOUNDARIES
# -------------------------
a0_sf <- sf::st_read(CONFIG$world_path, quiet = TRUE)
stopifnot(all(c("gaul0_code","iso3_code","gaul0_name") %in% names(a0_sf)))
a0_sf$gaul0_code <- as.character(a0_sf$gaul0_code)

# -------------------------
# LOAD PROGRAM-LEVEL INPUTS
# -------------------------
urg <- arrow::read_parquet("data/results/urgency.parquet")
if ("boundary_id" %in% names(urg)) urg <- subset(urg, boundary_id == "gaul0_code")
urg <- fix_code_type(urg)
urg$iso3c <- a0_sf$iso3_code[match(urg$gaul0_code, a0_sf$gaul0_code)]
urg <- urg[urg$iso3c != "LBN", ]

aow1_cap <- arrow::read_parquet("data/results/aow1_digi.parquet") %>%
  select(gaul0_code, aow1_cap = capacity_idx) %>% fix_code_type()
aow1_need <- arrow::read_parquet("data/results/aow1_need.parquet") %>%
  select(gaul0_code, aow1_need = need_idx) %>% fix_code_type()
aow2_res  <- arrow::read_parquet("data/results/aow2_resilience.parquet") %>%
  select(gaul0_code, resilience_idx) %>% fix_code_type()
aow2_shk  <- arrow::read_parquet("data/results/aow2_shocks.parquet") %>%
  select(gaul0_code, shock_idx) %>% fix_code_type()
aow3_cap  <- arrow::read_parquet("data/results/aow3_capacity.parquet") %>%
  select(gaul0_code, aow3_cap = capacity_idx) %>% fix_code_type()
aow3_con  <- arrow::read_parquet("data/results/aow3_constraints.parquet") %>%
  select(gaul0_code, constraint_idx) %>% fix_code_type()
grape     <- arrow::read_parquet("data/results/grape.parquet")
if (!"gaul0_code" %in% names(grape)) {
  grape <- grape %>% mutate(gaul0_code = a0_sf$gaul0_code[match(iso3c, a0_sf$iso3_code)])
}
grape <- grape %>% select(gaul0_code, iso3c, hr_per_rpop, rd_pct_gdp) %>% fix_code_type()

# -------------------------
# BUILD URGENCY, READINESS, OPPORTUNITY
# -------------------------
urg <- urg %>%
  mutate(
    vuln_norm   = if ("vuln_index_norm" %in% names(.)) vuln_index_norm else rescale01(vuln_index),
    expo_norm   = if ("expo_index_norm" %in% names(.)) expo_index_norm else rescale01(expo_index),
    urg_index   = if ("composite_index" %in% names(.)) composite_index else (vuln_norm + expo_norm)/2
  )

df <- urg %>%
  select(gaul0_code, iso3c, urg_index, vuln_norm, expo_norm) %>%
  left_join(aow1_cap, by = "gaul0_code") %>%
  left_join(aow1_need, by = "gaul0_code") %>%
  left_join(aow2_shk,  by = "gaul0_code") %>%
  left_join(aow2_res,  by = "gaul0_code") %>%
  left_join(aow3_con,  by = "gaul0_code") %>%
  left_join(aow3_cap,  by = "gaul0_code") %>%
  left_join(grape,     by = "gaul0_code") %>%
  mutate(country = a0_sf$gaul0_name[match(gaul0_code, a0_sf$gaul0_code)])

df <- df %>%
  rename(iso3c = iso3c.x)

# Readiness components (normalize then weight)
df <- df %>%
  mutate(
    aow1_cap_n = rescale01(aow1_cap),
    aow2_res_n = rescale01(resilience_idx),
    aow3_cap_n = rescale01(aow3_cap),
    grape_hr_n = rescale01(hr_per_rpop),
    grape_spd_n= rescale01(rd_pct_gdp)
  )

rw <- CONFIG$readiness_weights
df <- df %>%
  mutate(
    readiness_idx = rw["aow1_cap"]*aow1_cap_n +
      rw["aow2_res"]*aow2_res_n +
      rw["aow3_cap"]*aow3_cap_n +
      rw["grape_hr"]*grape_hr_n +
      rw["grape_spd"]*grape_spd_n,
    reach_idx       = expo_norm,
    opportunity_idx = CONFIG$reach_weight*reach_idx + CONFIG$readiness_weight*readiness_idx
  )

# -------------------------
# COMPOSITE SCORE & RANKING (Primary/Secondary)
# -------------------------
# Assign by composite rank
program30 <- df %>%
  mutate(
    composite_score = 0.7*urg_index + 0.3*opportunity_idx
  ) %>%
  arrange(desc(composite_score)) %>%
  mutate(rank_overall = row_number()) %>%
  # Keep only the top 30
  slice_head(n = 30) %>%
  mutate(
    program_tier = ifelse(rank_overall <= 15, "Primary", "Secondary")
  )


# ---- Enforce exactly 15/15 ----
n_primary   <- sum(program30$program_tier == "Primary")
n_secondary <- sum(program30$program_tier == "Secondary")

if (n_primary != 15 | n_secondary != 15) {
  # Force top 15 to Primary, rest to Secondary
  program30 <- program30 %>%
    mutate(program_tier = ifelse(rank_overall <= 15, "Primary", "Secondary"))
}

# Assert again
stopifnot(sum(program30$program_tier == "Primary") == 15,
          sum(program30$program_tier == "Secondary") == 15)

# -------------------------
# QUADRANTS @ 60th PERCENTILE
# -------------------------
# K-Means Quadrant Classification
# set.seed(123)  # for reproducibility

# Run K-means on urgency and opportunity
# km <- kmeans(scale(program30[, c("urg_index", "opportunity_idx")]), centers = 4, nstart = 25)
# 
# program30$cluster <- km$cluster
# 
# # Calculate cluster means to map to quadrant labels
# cluster_means <- program30 %>%
#   group_by(cluster) %>%
#   summarise(mean_urg = mean(urg_index, na.rm = TRUE),
#             mean_opp = mean(opportunity_idx, na.rm = TRUE))
# 
# print(cluster_means)

# # Relabel clusters into quadrants
# program30 <- program30 %>%
#   left_join(cluster_means, by = "cluster") %>%
#   mutate(Quadrant = case_when(
#     mean_urg >= median(program30$urg_index) & mean_opp >= median(program30$opportunity_idx) ~ "High Urgency & High Opportunity",
#     mean_urg >= median(program30$urg_index) & mean_opp <  median(program30$opportunity_idx) ~ "High Urgency & Low Opportunity",
#     mean_urg <  median(program30$urg_index) & mean_opp >= median(program30$opportunity_idx) ~ "Low Urgency & High Opportunity",
#     TRUE ~ "Low Urgency & Low Opportunity"
#   ))

# Calculate median cutoffs
urg_cutoff <- quantile(program30$urg_index, 0.4, na.rm = TRUE)
opp_cutoff <- quantile(program30$opportunity_idx, 0.4, na.rm = TRUE)

program30 <- program30 %>%
  mutate(
    Urgency     = ifelse(urg_index >= urg_cutoff, "High Urgency", "Low Urgency"),
    Opportunity = ifelse(opportunity_idx >= opp_cutoff, "High Opportunity", "Low Opportunity"),
    Quadrant    = paste(Urgency, "&", Opportunity)
  )


program30 <- program30 %>%
  select(
    gaul0_code, iso3c, country,
    urg_index, opportunity_idx, readiness_idx, reach_idx,
    composite_score, rank_overall, program_tier,
    Quadrant
  )

# Save tidy table
out_csv <- file.path(CONFIG$out_dir, "sfp_program30_composite_ranking.csv")
write.csv(program30, out_csv, row.names = FALSE)
message("Saved table: ", normalizePath(out_csv))
message("Cutoffs (60th pct): Urgency = ", round(urg_cutoff, 3), 
        " | Opportunity = ", round(opp_cutoff, 3))

# -------------------------
# CONSISTENT COLORS
# -------------------------
tier_cols <- c("Primary" = "#2E7D32", "Secondary" = "#9CCC65")  # tier palette
quadrant_cols <- c(                                             # used everywhere for quadrants
  "High Urgency & High Opportunity" = "#2A9D8F",   # teal-green
  "High Urgency & Low Opportunity"  = "#E76F51",   # soft orange-red
  "Low Urgency & High Opportunity"  = "#3A86FF",   # blue
  "Low Urgency & Low Opportunity"   = "#9E9E9E"    # grey
)

# -------------------------
# MAPS (all boundaries visible)
# -------------------------
a0_sf$gaul0_code <- as.character(a0_sf$gaul0_code)
program30$gaul0_code <- as.character(program30$gaul0_code)

map_df <- a0_sf %>%
  left_join(program30 %>% select(gaul0_code, program_tier, Quadrant), by = "gaul0_code")

# 1) Primary vs Secondary (single map)
p_tiers <- ggplot() +
  geom_sf(data = a0_sf, fill = "white", color = "grey80", size = 0.12) +
  geom_sf(data = map_df, aes(fill = program_tier), color = "grey50", size = 0.15) +
  scale_fill_manual(values = tier_cols, na.value = "white") +
  labs(title = "SFP Program Countries – Primary vs Secondary (Composite Ranking)",
       fill = "Program Tier") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave(file.path(CONFIG$out_dir, "map_program30_tiers.png"), p_tiers, width = 12, height = 6.5, dpi = 300)

# Map: All program countries colored by Quadrant
p_quadrants_all <- ggplot() +
  # Base: all world boundaries in white
  geom_sf(data = a0_sf, fill = "white", color = "grey80", size = 0.1) +
  
  # Overlay program countries (Primary + Secondary) colored by Quadrant
  geom_sf(data = map_df, aes(fill = Quadrant), color = "grey50", size = 0.2) +
  
  scale_fill_manual(
    values = quadrant_cols,   # same consistent palette
    drop   = FALSE,
    na.value = "white"
  ) +
  labs(
    title    = "SFP Program Countries by Urgency & Opportunity Quadrants",
    subtitle = "All 30 program countries (Primary + Secondary)",
    fill     = "Quadrant"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(CONFIG$out_dir, "map_program30_quadrants_all.png"),
       p_quadrants_all, width = 12, height = 6.5, dpi = 300)



# 2) Primary only – by Quadrant
p_primary_quads <- ggplot() +
  geom_sf(data = a0_sf, fill = "white", color = "grey80", size = 0.12) +
  geom_sf(data = subset(map_df, program_tier == "Primary"),
          aes(fill = Quadrant), color = "grey50", size = 0.15) +
  scale_fill_manual(values = quadrant_cols, na.value = "white", drop = FALSE) +
  labs(title = "Primary Countries Categories",
       fill = "Quadrant") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave(file.path(CONFIG$out_dir, "map_primary_quadrants.png"),
       p_primary_quads, width = 12, height = 6.5, dpi = 300)

# 3) Secondary only – by Quadrant
p_secondary_quads <- ggplot() +
  geom_sf(data = a0_sf, fill = "white", color = "grey80", size = 0.12) +
  geom_sf(data = subset(map_df, program_tier == "Secondary"),
          aes(fill = Quadrant), color = "grey50", size = 0.15) +
  scale_fill_manual(values = quadrant_cols, na.value = "white", drop = FALSE) +
  labs(title = "Secondary Countries  Categories",
       fill = "Quadrant") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
ggsave(file.path(CONFIG$out_dir, "map_secondary_quadrants.png"),
       p_secondary_quads, width = 12, height = 6.5, dpi = 300)

# -------------------------
# SCATTER (faceted by tier; same quadrant colors)
# -------------------------
scatter_df <- program30 %>%
  mutate(program_tier = factor(program_tier, levels = c("Primary","Secondary")))

p_scatter <- ggplot(scatter_df, aes(x = opportunity_idx, y = urg_index)) +
  geom_vline(xintercept = opp_cutoff, linetype = "dashed", color = "grey60", alpha = 0.6) +
  geom_hline(yintercept = urg_cutoff, linetype = "dashed", color = "grey60", alpha = 0.6) +
  geom_point(aes(color = Quadrant), size = 3.8, alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = country),
    size = 3, max.overlaps = 100, box.padding = 0.25, point.padding = 0.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = quadrant_cols, drop = FALSE) +
  facet_wrap(~ program_tier) +
  labs(
    title = "Urgency vs Opportunity (Composite Ranking shown by Tier)",
    x = "Opportunity Index", y = "Urgency Index", color = "Quadrant"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave(file.path(CONFIG$out_dir, "scatter_urgency_opportunity_faceted.png"),
       p_scatter, width = 12, height = 7, dpi = 300)


# -------------------------
# DONE
# -------------------------
message("All outputs saved in: ", normalizePath(CONFIG$out_dir))
