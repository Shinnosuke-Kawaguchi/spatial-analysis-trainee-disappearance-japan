source(file.path("src", "04_spatial_weights_and_moran.R"))

load_required_packages(c("splm", "dplyr"))
ensure_output_dirs()

calc_lag <- function(x, w) {
  as.vector(w %*% x)
}

df_sdm <- spatial_df %>%
  dplyr::group_by(.data$Year) %>%
  dplyr::arrange(.data$Prefecture, .by_group = TRUE) %>%
  dplyr::mutate(
    W_share_manufacturing = calc_lag(.data$share_manufacturing, W_mat),
    W_share_construction = calc_lag(.data$share_construction, W_mat),
    W_share_primary = calc_lag(.data$share_primary, W_mat),
    W_ln_min_salary = calc_lag(.data$ln_min_salary, W_mat),
    W_jp_school_density_area = calc_lag(.data$jp_school_density_area, W_mat),
    W_violation_count = calc_lag(.data$violation_count, W_mat),
    W_job_offer_ratio_avg = calc_lag(.data$job_offer_ratio_avg, W_mat),
    W_foreigner_ratio = calc_lag(.data$foreigner_ratio, W_mat)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.data$Prefecture, .data$Year)

sdm_formula <- disappear_ratio ~
  share_manufacturing + share_construction + share_primary +
  ln_min_salary + jp_school_density_area + violation_count +
  foreigner_ratio + job_offer_ratio_avg +
  W_share_manufacturing + W_share_construction + W_share_primary +
  W_ln_min_salary + W_jp_school_density_area + W_violation_count +
  W_job_offer_ratio_avg + W_foreigner_ratio

sdm_result <- splm::spml(
  formula = sdm_formula,
  data = df_sdm,
  listw = listw_knn,
  model = "within",
  effect = "twoways",
  spatial.error = "none",
  lag = TRUE
)

utils::capture.output(
  summary(sdm_result),
  file = project_file("results", "sdm_summary.txt")
)

attr(sdm_result, "have_factor_preds") <- FALSE

imp_sdm <- impacts(
  sdm_result,
  listw = listw_knn,
  time = length(unique(df_sdm$Year)),
  R = 1000
)

utils::capture.output(
  summary(imp_sdm, zstats = TRUE, short = TRUE),
  file = project_file("results", "sdm_impacts_summary.txt")
)
