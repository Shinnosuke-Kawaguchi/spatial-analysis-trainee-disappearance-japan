# Public analysis entry point.
# Run from the repository root: source("src/final.R")

source(file.path("src", "01_prepare_analysis_data.R"))
source(file.path("src", "02_regression_models.R"))
source(file.path("src", "03_output_tables.R"))
source(file.path("src", "04_spatial_weights_and_moran.R"))

message("Core scripts completed. Run src/05_spatial_durbin_model.R for the SDM/impact analysis.")
message("Run src/06_fixed_effect_market_access_optional.R only if data/pref_density.xlsx is available locally.")
