source(file.path("src", "00_paths.R"))

load_required_packages(c("readxl", "dplyr"))
ensure_output_dirs()

prepare_analysis_data <- function(input_path = project_file("data", "final.xlsx")) {
  df <- readxl::read_excel(input_path)

  required_columns <- c(
    "Prefecture", "Year", "foreigner_ratio", "job_offer_ratio_avg",
    "disappear_ratio", "trainees_total", "disappear_per1000", "disappeared",
    "total_total", "all_total", "share_primary", "share_construction",
    "share_manufacturing", "min_salary", "jp_school_num", "Inhabitable_area",
    "violation_count"
  )

  check_columns(df, required_columns, "data/final.xlsx")

  df %>%
    dplyr::mutate(
      Prefecture = trimws(.data$Prefecture),
      Year = as.integer(.data$Year),
      jp_school_density_area = .data$jp_school_num / .data$Inhabitable_area,
      ln_min_salary = log(.data$min_salary)
    ) %>%
    dplyr::arrange(.data$Prefecture, .data$Year)
}

analysis_df <- prepare_analysis_data()

utils::write.csv(
  analysis_df,
  project_file("results", "analysis_data.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
