source(file.path("src", "01_prepare_analysis_data.R"))

load_required_packages(c("readxl", "dplyr", "fixest", "ggplot2", "ggrepel"))
ensure_output_dirs()

density_path <- project_file("data", "pref_density.xlsx")

if (!file.exists(density_path)) {
  stop(
    "Optional input data/pref_density.xlsx was not found. ",
    "This file is intentionally not tracked in the public repository.",
    call. = FALSE
  )
}

density_df <- readxl::read_excel(density_path)
check_columns(density_df, c("Prefecture", "train_density"), "data/pref_density.xlsx")

pref_fe_model <- fixest::feols(
  disappear_ratio ~
    foreigner_ratio + job_offer_ratio_avg +
    share_primary + share_construction + share_manufacturing +
    min_salary + jp_school_density_area + violation_count |
    Prefecture,
  data = analysis_df
)

fixed_effects <- fixest::fixef(pref_fe_model)$Prefecture

df_fe <- data.frame(
  Prefecture = names(fixed_effects),
  FE_Value = as.numeric(fixed_effects)
)

plot_data <- df_fe %>%
  dplyr::inner_join(density_df, by = "Prefecture")

p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = train_density, y = FE_Value, label = Prefecture)) +
  ggplot2::geom_point(color = "steelblue", alpha = 0.8, size = 2) +
  ggplot2::geom_smooth(method = "lm", color = "darkred", se = FALSE, linetype = "dashed") +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  ggplot2::labs(
    title = "Railway Density vs Prefecture Fixed Effects",
    subtitle = "Exploratory check of market-access-related unobserved risk",
    x = "Railway Density",
    y = "Prefecture Fixed Effect"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = project_file("figures", "railway_density_fixed_effects.png"),
  plot = p,
  width = 9,
  height = 6,
  dpi = 300
)

cor_result <- stats::cor.test(plot_data$train_density, plot_data$FE_Value)

utils::capture.output(
  cor_result,
  file = project_file("results", "railway_density_fixed_effect_correlation.txt")
)
