# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║                     LOAD PSD MEASURES                       ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages
library(tidyverse)
library(arrow)
library(data.table)
library(crayon)

# Define file paths
lobe_csv_path <- file.path(project_path, "datasets", "lobe_fooof_results.csv")
region_csv_path <- file.path(project_path, "datasets", "region_fooof_results.csv")
region_spectra_csv_path <- file.path(project_path, "datasets", "region_fooof_spectra_results.csv")
vertex_parquet_path <- file.path(project_path, "datasets", "vertex_fooof_results.parquet")

# Load lobe-level FOOOF results
cat(crayon::blue("Loading lobe-level FOOOF results...\n"))
if (file.exists(lobe_csv_path)) {
  lobe_fooof_data <- fread(lobe_csv_path)
  lobe_fooof_data <- as_tibble(lobe_fooof_data)
  cat(crayon::green("✓ Lobe-level data loaded successfully!\n"))
  cat("  Dimensions:", dim(lobe_fooof_data)[1], "rows,", dim(lobe_fooof_data)[2], "columns\n\n")
} else {
  cat(crayon::red("✗ Lobe-level data file not found!\n\n"))
}

# Load region-level FOOOF results
cat(crayon::blue("Loading region-level FOOOF results...\n"))
if (file.exists(region_csv_path)) {
  region_fooof_data <- fread(region_csv_path)
  region_fooof_data <- as_tibble(region_fooof_data)
  cat(crayon::green("✓ Region-level data loaded successfully!\n"))
  cat("  Dimensions:", dim(region_fooof_data)[1], "rows,", dim(region_fooof_data)[2], "columns\n\n")
} else {
  cat(crayon::red("✗ Region-level data file not found!\n\n"))
}

# Load region-level FOOOF spectra results
cat(crayon::blue("Loading region-level FOOOF spectra results...\n"))
if (file.exists(region_spectra_csv_path)) {
  region_spectra_data <- fread(region_spectra_csv_path)
  region_spectra_data <- as_tibble(region_spectra_data)
  cat(crayon::green("✓ Region-level spectra data loaded successfully!\n"))
  cat("  Dimensions:", dim(region_spectra_data)[1], "rows,", dim(region_spectra_data)[2], "columns\n\n")
} else {
  cat(crayon::red("✗ Region-level spectra data file not found!\n\n"))
}

# Load vertex-level FOOOF results from Parquet file
cat(crayon::blue("Loading vertex-level FOOOF results from Parquet...\n"))
if (file.exists(vertex_parquet_path)) {
  vertex_fooof_data <- read_parquet(vertex_parquet_path)
  cat(crayon::green("✓ Vertex-level data loaded successfully!\n"))
  cat("  Dimensions:", dim(vertex_fooof_data)[1], "rows,", dim(vertex_fooof_data)[2], "columns\n\n")
} else {
  cat(crayon::red("✗ Vertex-level data file not found!\n\n"))
}

cat(crayon::bold$cyan("═══════════════════════════════════════════════════════════════════════════\n"))
