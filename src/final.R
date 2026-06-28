install.packages("xtsum")
library(xtsum)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)

#####データセットと可視化と記述統計量####
df <- read_excel("data/final.xlsx")

analysis_df <- df %>%
  mutate(
    Prefecture = trimws(Prefecture),
    Year = as.integer(Year),
    jp_school_density_area = jp_school_num / Inhabitable_area,
    ln_min_salary = log(min_salary)
  ) %>%
  arrange(Prefecture, Year)

base_df <- analysis_df %>%
  filter(Year == 2020) %>%
  select(
    Prefecture,
    base_disappear_per1000 = disappear_per1000,
    base_trainees_total = trainees_total
  )

index_df <- analysis_df %>%
  left_join(base_df, by = "Prefecture") %>%
  mutate(
    disappear_per1000_index = disappear_per1000 / base_disappear_per1000 * 100,
    trainees_total_index = trainees_total / base_trainees_total * 100
  ) %>%
  select(
    Prefecture,
    Year,
    disappear_per1000_index,
    trainees_total_index
  ) %>%
  pivot_longer(
    cols = c(disappear_per1000_index, trainees_total_index),
    names_to = "series",
    values_to = "index_value"
  ) %>%
  mutate(
    series = recode(
      series,
      disappear_per1000_index = "Per 1,000",
      trainees_total_index = "Trainees"
    )
  )

p_rq <- ggplot(index_df, aes(x = Year, y = index_value,
                             color = series, linetype = series)) +
  geom_hline(yintercept = 100, color = "grey70", linewidth = 0.25) +
  geom_line(linewidth = 0.45) +
  geom_point(size = 0.6) +
  facet_wrap(~ Prefecture, ncol = 7) +
  scale_color_manual(
    values = c(
      "Per 1,000" = "red",
      "Trainees" = "deepskyblue3"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Per 1,000" = "solid",
      "Trainees" = "dashed"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(index_df$Year))) +
  labs(
    title = "Per 1,000 vs Trainees (Indexed to 2020 = 100) by Prefecture",
    x = NULL,
    y = "Index, 2020 = 100",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_family = "Yu Gothic") +
  theme(
    plot.title = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 5),
    axis.title.y = element_text(size = 7),
    strip.text = element_text(size = 6),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )
p_rq

xtsum_vars <- c(
  "disappear_ratio",
  "foreigner_ratio",
  "job_offer_ratio_avg",
  "share_primary",
  "share_construction",
  "share_manufacturing",
  "ln_min_salary",
  "jp_school_density_area",
  "violation_count"
)

xtsum_table <- xtsum(
  data = analysis_df,
  variables = xtsum_vars,
  id = "Prefecture",
  t = "Year",
  na.rm = TRUE,
  return.data.frame = FALSE,
  dec = 3
)

xtsum_table

#####ＳＤＭ####

library(geosphere)
library(spdep)
library(sf)
library(fixest)
library(splm)


coord_df <- read_excel("data/Citylatlongi.xlsx") %>%
  mutate(Prefecture = trimws(as.character(Prefecture))) %>%
  arrange(Prefecture)

master_order <- coord_df$Prefecture

spatial_df <- analysis_df %>%
  mutate(
    Prefecture = factor(Prefecture, levels = master_order),
    Year = as.integer(Year)
  ) %>%
  arrange(Prefecture, Year)

coords <- as.matrix(coord_df[, c("City_longitude", "City_latitude")])
rownames(coords) <- master_order

# ============================================================
# 2. W1: k近傍W k = 4
# ============================================================

knn_nb <- knearneigh(coords, k = 4)
knn_nb <- knn2nb(knn_nb, row.names = master_order)

listw_knn <- nb2listw(knn_nb, style = "W", zero.policy = FALSE)

W_knn <- listw2mat(listw_knn)
rownames(W_knn) <- master_order
colnames(W_knn) <- master_order

knn_weights <- list(
  label = "knn_k4",
  listw = listw_knn,
  W = W_knn
)

# ============================================================
# 3. W2: Queen型W
#    孤立県だけ最近傍県にbridgeしてSDMで使う
# ============================================================

bridge_islands <- function(nb, coords, region_names) {
  zero_ids <- which(card(nb) == 0)
  
  dist_mat <- geosphere::distm(coords, fun = geosphere::distHaversine)
  diag(dist_mat) <- Inf
  
  for (i in zero_ids) {
    nearest <- which.min(dist_mat[i, ])
    nb[[i]] <- as.integer(nearest)
    nb[[nearest]] <- sort(as.integer(unique(c(nb[[nearest]][nb[[nearest]] != 0L], i))))
  }
  
  attr(nb, "region.id") <- region_names
  attr(nb, "sym") <- FALSE
  
  nb
}

suppressMessages(sf_use_s2(FALSE))

n03 <- st_read(
  "N03-20260101_GML/N03-20260101_prefecture.shp",
  quiet = TRUE,
  options = "ENCODING=UTF-8"
)

pref_polygons <- n03 %>%
  st_make_valid() %>%
  st_transform(3857) %>%
  group_by(N03_001) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_simplify(dTolerance = 1000, preserveTopology = TRUE) %>%
  st_make_valid()

pref_polygons <- pref_polygons[match(master_order, pref_polygons$N03_001), ]

queen_nb <- poly2nb(
  pref_polygons,
  queen = TRUE,
  row.names = pref_polygons$N03_001,
  snap = 100
)

queen_nb <- bridge_islands(
  nb = queen_nb,
  coords = coords,
  region_names = master_order
)

listw_queen <- nb2listw(queen_nb, style = "W", zero.policy = FALSE)

W_queen <- listw2mat(listw_queen)
rownames(W_queen) <- master_order
colnames(W_queen) <- master_order

queen_weights <- list(
  label = "queen_bridge",
  listw = listw_queen,
  W = W_queen
)

# ============================================================
# 4. SDM変数とWX作成
# ============================================================

sdm_x <- c(
  "share_manufacturing",
  "share_construction",
  "share_primary",
  "ln_min_salary",
  "jp_school_density_area",
  "violation_count",
  "foreigner_ratio",
  "job_offer_ratio_avg"
)

calc_lag <- function(x, W) {
  as.vector(W %*% x)
}

add_WX <- function(data, W) {
  data %>%
    group_by(Year) %>%
    arrange(Prefecture, .by_group = TRUE) %>%
    mutate(
      W_share_manufacturing = calc_lag(share_manufacturing, W),
      W_share_construction = calc_lag(share_construction, W),
      W_share_primary = calc_lag(share_primary, W),
      W_ln_min_salary = calc_lag(ln_min_salary, W),
      W_jp_school_density_area = calc_lag(jp_school_density_area, W),
      W_violation_count = calc_lag(violation_count, W),
      W_foreigner_ratio = calc_lag(foreigner_ratio, W),
      W_job_offer_ratio_avg = calc_lag(job_offer_ratio_avg, W)
    ) %>%
    ungroup() %>%
    arrange(Prefecture, Year)
}

sdm_formula <- disappear_ratio ~
  share_manufacturing + share_construction + share_primary +
  ln_min_salary + jp_school_density_area + violation_count +
  foreigner_ratio + job_offer_ratio_avg +
  W_share_manufacturing + W_share_construction + W_share_primary +
  W_ln_min_salary + W_jp_school_density_area + W_violation_count +
  W_foreigner_ratio + W_job_offer_ratio_avg

# ============================================================
# 5. impacts() の結果を表にする関数
# ============================================================

extract_impacts_table <- function(impact_summary, w_label) {
  bind_rows(lapply(c("Direct", "Indirect", "Total"), function(effect_name) {
    effect_key <- tolower(effect_name)
    
    impact_names <- attr(impact_summary, "bnames")
    
    if (is.null(impact_names)) {
      impact_names <- names(impact_summary$res[[effect_key]])
    }
    
    if (is.null(impact_names)) {
      impact_names <- rownames(impact_summary$semat)
    }
    
    data.frame(
      spatial_weights = w_label,
      fixed_effects = "Year",
      effect = effect_key,
      variable = impact_names,
      estimate = as.numeric(impact_summary$res[[effect_key]]),
      std_error = as.numeric(impact_summary$semat[, effect_name]),
      z_value = as.numeric(impact_summary$zmat[, effect_name]),
      p_value = as.numeric(impact_summary$pzmat[, effect_name]),
      row.names = NULL
    )
  }))
}

# ============================================================
# 6. SDM実行：Year固定効果のみ
# ============================================================

run_sdm <- function(weight_obj) {
  df_sdm <- add_WX(
    data = spatial_df,
    W = weight_obj$W
  )
  
  sdm_model <- spml(
    formula = sdm_formula,
    data = as.data.frame(df_sdm),
    index = c("Prefecture", "Year"),
    listw = weight_obj$listw,
    model = "within",
    effect = "time",
    spatial.error = "none",
    lag = TRUE
  )
  
  attr(sdm_model, "have_factor_preds") <- FALSE
  
  sdm_impacts <- impacts(
    sdm_model,
    listw = weight_obj$listw,
    time = length(unique(df_sdm$Year)),
    R = 1000
  )
  
  sdm_impacts_summary <- summary(
    sdm_impacts,
    zstats = TRUE,
    short = TRUE
  )
  
  sdm_impacts_table <- extract_impacts_table(
    impact_summary = sdm_impacts_summary,
    w_label = weight_obj$label
  )
  
  list(
    model = sdm_model,
    model_summary = summary(sdm_model),
    impacts = sdm_impacts,
    impacts_summary = sdm_impacts_summary,
    impacts_table = sdm_impacts_table
  )
}

# ============================================================
# 7. k近傍WとQueen型Wで実行
# ============================================================

sdm_results <- list(
  knn_k4 = run_sdm(knn_weights),
  queen_bridge = run_sdm(queen_weights)
)

# ============================================================
# 8. 結果確認・効果分解比較
# ============================================================

sdm_summary_knn <- sdm_results$knn_k4$model_summary
sdm_summary_queen <- sdm_results$queen_bridge$model_summary

sdm_impacts_summary_knn <- sdm_results$knn_k4$impacts_summary
sdm_impacts_summary_queen <- sdm_results$queen_bridge$impacts_summary

sdm_impacts_all <- bind_rows(
  sdm_results$knn_k4$impacts_table,
  sdm_results$queen_bridge$impacts_table
)

sdm_impacts_comparison <- sdm_impacts_all %>%
  select(
    effect,
    variable,
    spatial_weights,
    estimate,
    std_error,
    z_value,
    p_value
  ) %>%
  pivot_wider(
    names_from = spatial_weights,
    values_from = c(
      estimate,
      std_error,
      z_value,
      p_value
    )
  ) %>%
  arrange(
    factor(effect, levels = c("direct", "indirect", "total")),
    variable
  )

sdm_summary_knn
sdm_summary_queen

sdm_impacts_summary_knn
sdm_impacts_summary_queen

sdm_impacts_all
sdm_impacts_comparison