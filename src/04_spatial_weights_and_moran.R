source(file.path("src", "01_prepare_analysis_data.R"))

load_required_packages(c("readxl", "dplyr", "geosphere", "spdep", "fixest", "maps", "mapdata"))
ensure_output_dirs()

coord_df <- readxl::read_excel(project_file("data", "Citylatlongi.xlsx"))

check_columns(
  coord_df,
  c("Prefecture", "City", "City_latitude", "City_longitude"),
  "data/Citylatlongi.xlsx"
)

coord_df <- coord_df %>%
  dplyr::mutate(Prefecture = trimws(.data$Prefecture)) %>%
  dplyr::arrange(.data$Prefecture)

master_order <- coord_df$Prefecture

spatial_df <- analysis_df %>%
  dplyr::mutate(Prefecture = factor(.data$Prefecture, levels = master_order)) %>%
  dplyr::arrange(.data$Prefecture, .data$Year)

coords <- as.matrix(coord_df[, c("City_longitude", "City_latitude")])
rownames(coords) <- coord_df$Prefecture

dist_mat <- geosphere::distm(coords, fun = geosphere::distHaversine)
rownames(dist_mat) <- coord_df$Prefecture
colnames(dist_mat) <- coord_df$Prefecture
diag(dist_mat) <- NA

W_inverse_distance <- 1 / dist_mat
W_inverse_distance[is.na(W_inverse_distance)] <- 0
row_sums <- rowSums(W_inverse_distance)
W_inverse_distance[row_sums > 0, ] <- W_inverse_distance[row_sums > 0, ] / row_sums[row_sums > 0]

utils::write.csv(
  W_inverse_distance,
  project_file("results", "W_inverse_distance_rowstandardized.csv"),
  row.names = TRUE,
  fileEncoding = "UTF-8"
)

k_nn <- spdep::knearneigh(coords, k = 4)
knn_nb <- spdep::knn2nb(k_nn, row.names = coord_df$Prefecture)
listw_knn <- spdep::nb2listw(knn_nb, style = "W")

W_mat <- spdep::listw2mat(listw_knn)
rownames(W_mat) <- coord_df$Prefecture
colnames(W_mat) <- coord_df$Prefecture

utils::write.csv(
  W_mat,
  project_file("results", "W_knn_k4_rowstandardized.csv"),
  row.names = TRUE,
  fileEncoding = "UTF-8"
)

years <- sort(unique(spatial_df$Year))

moran_table <- dplyr::bind_rows(lapply(years, function(yr) {
  df_year <- spatial_df[spatial_df$Year == yr, ] %>%
    dplyr::arrange(.data$Prefecture)

  mi <- spdep::moran.test(df_year$disappear_ratio, listw_knn)

  data.frame(
    Year = yr,
    Moran_I = unname(mi$estimate["Moran I statistic"]),
    Expected = unname(mi$estimate["E.I"]),
    Variance = unname(mi$estimate["Var.I"]),
    Z_value = unname(mi$statistic),
    P_value = mi$p.value
  )
}))

utils::write.csv(
  moran_table,
  project_file("results", "moran_disappear_ratio.csv"),
  row.names = FALSE
)

pref_fe_model <- fixest::feols(
  disappear_ratio ~
    foreigner_ratio + job_offer_ratio_avg +
    share_primary + share_construction + share_manufacturing +
    min_salary + jp_school_density_area + violation_count |
    Prefecture,
  data = spatial_df
)

spatial_df$resid_pref_fe <- stats::resid(pref_fe_model)

moran_resid_table <- dplyr::bind_rows(lapply(years, function(yr) {
  df_year <- spatial_df[spatial_df$Year == yr, ] %>%
    dplyr::arrange(.data$Prefecture)

  mi <- spdep::moran.test(df_year$resid_pref_fe, listw_knn)

  data.frame(
    Year = yr,
    Moran_I = unname(mi$estimate["Moran I statistic"]),
    Expected = unname(mi$estimate["E.I"]),
    Variance = unname(mi$estimate["Var.I"]),
    Z_value = unname(mi$statistic),
    P_value = mi$p.value
  )
}))

utils::write.csv(
  moran_resid_table,
  project_file("results", "moran_prefecture_fe_residuals.csv"),
  row.names = FALSE
)

grDevices::png(project_file("figures", "japan_knn_k4_network.png"), width = 1600, height = 1600, res = 200)
graphics::par(mar = c(0, 0, 2, 0))
maps::map("japan", col = "grey90", fill = TRUE, border = "white")
plot(knn_nb, coords, add = TRUE, col = "red", lwd = 1.5, pch = 20, cex = 1.2)
graphics::title("k-Nearest Neighbors (k=4) Network", cex.main = 2)
grDevices::dev.off()
