source(file.path("src", "01_prepare_analysis_data.R"))

load_required_packages(c("estimatr", "car", "dplyr"))
ensure_output_dirs()

model_formula <- disappear_ratio ~
  foreigner_ratio + job_offer_ratio_avg +
  share_primary + share_construction +
  share_manufacturing + min_salary +
  jp_school_density_area + violation_count

m1 <- estimatr::lm_robust(
  formula = model_formula,
  data = analysis_df,
  clusters = Prefecture,
  se_type = "CR2"
)

m2 <- estimatr::lm_robust(
  formula = model_formula,
  data = analysis_df,
  fixed_effects = ~ Prefecture,
  clusters = Prefecture,
  se_type = "CR2"
)

m3 <- estimatr::lm_robust(
  formula = model_formula,
  data = analysis_df,
  fixed_effects = ~ Year,
  clusters = Prefecture,
  se_type = "CR2"
)

m4 <- estimatr::lm_robust(
  formula = model_formula,
  data = analysis_df,
  fixed_effects = ~ Prefecture + Year,
  clusters = Prefecture,
  se_type = "CR2"
)

models <- list(
  "(1) Pooled OLS" = m1,
  "(2) Prefecture FE" = m2,
  "(3) Year FE" = m3,
  "(4) Two-way FE" = m4
)

lm_for_vif <- stats::lm(model_formula, data = analysis_df)
vif_values <- car::vif(lm_for_vif)

utils::write.csv(
  data.frame(term = names(vif_values), vif = as.numeric(vif_values)),
  project_file("results", "vif.csv"),
  row.names = FALSE
)

X_main <- analysis_df %>%
  dplyr::select(
    foreigner_ratio, job_offer_ratio_avg,
    share_primary, share_construction, share_manufacturing,
    min_salary, jp_school_density_area, violation_count
  ) %>%
  as.matrix()

FE_mat <- stats::model.matrix(~ Prefecture + factor(Year) - 1, data = analysis_df)
X_all <- cbind(X_main, FE_mat)

rank_diagnostics <- data.frame(
  matrix = c("main_covariates", "main_covariates_plus_fixed_effects"),
  qr_rank = c(qr(X_main)$rank, qr(X_all)$rank),
  n_columns = c(ncol(X_main), ncol(X_all))
)
rank_diagnostics$full_rank <- rank_diagnostics$qr_rank == rank_diagnostics$n_columns

utils::write.csv(
  rank_diagnostics,
  project_file("results", "rank_diagnostics.csv"),
  row.names = FALSE
)

utils::capture.output(
  summary(m4),
  file = project_file("results", "twfe_summary.txt")
)
