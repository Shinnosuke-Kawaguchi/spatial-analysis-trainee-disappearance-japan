source(file.path("src", "02_regression_models.R"))

load_required_packages(c("modelsummary", "gt", "webshot2", "dplyr", "tibble"))
ensure_output_dirs()

gof_map <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "Observations", 0,
  "r.squared", "R-squared", 3,
  "adj.r.squared", "Adj. R2", 3
)

modelsummary::modelsummary(
  models,
  fmt = 3,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map,
  coef_rename = c(
    "foreigner_ratio" = "Foreigner Ratio",
    "job_offer_ratio_avg" = "Job Offer Ratio",
    "share_manufacturing" = "Share of Manufacturing",
    "share_construction" = "Share of Construction",
    "share_primary" = "Share of Primary Sector",
    "min_salary" = "Minimum Wage",
    "jp_school_density_area" = "Japanese School Density",
    "violation_count" = "Labor Violations"
  ),
  title = "Regression Results of Disappearance Rate",
  output = project_file("figures", "regression_results.png")
)

descriptive_df <- analysis_df %>%
  dplyr::select(
    disappear_ratio, foreigner_ratio, job_offer_ratio_avg,
    share_manufacturing, share_construction, share_primary,
    min_salary, jp_school_density_area, violation_count
  )

names(descriptive_df) <- c(
  "Disappearance Rate",
  "Foreigner Ratio",
  "Job Offer Ratio",
  "Share of Manufacturing",
  "Share of Construction",
  "Share of Primary Sector",
  "Minimum Wage",
  "Japanese School Density",
  "Labor Violations"
)

modelsummary::datasummary_skim(
  descriptive_df,
  fmt = 2,
  title = "Descriptive Statistics",
  output = project_file("figures", "descriptive_statistics.png")
)
