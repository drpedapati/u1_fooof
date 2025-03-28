# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║                     LOAD PSD MEASURES                       ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages if not already loaded
library(tidyverse)
library(arrow)
library(data.table)

# Define file paths
csv_file_path <- file.path(project_path, "vertex_fooof_results.csv")
parquet_file_path <- file.path(project_path, "datasets", "vertex_fooof_results.parquet")

# Check if the CSV file exists
if (!file.exists(csv_file_path)) {
  stop(paste("CSV file not found:", csv_file_path))
}

# Read the CSV file
cat(crayon::blue("Reading CSV file:", csv_file_path, "\n"))
fooof_data <- fread(csv_file_path)

# Convert to tibble for better compatibility with arrow
fooof_data <- as_tibble(fooof_data)

# Display summary of the data
cat(crayon::blue("Data summary:\n"))
cat("Dimensions:", dim(fooof_data)[1], "rows,", dim(fooof_data)[2], "columns\n")
cat("Column names:", paste(names(fooof_data), collapse = ", "), "\n\n")

# Write to Parquet format
cat(crayon::blue("Converting to Parquet format and saving to:", parquet_file_path, "\n"))
write_parquet(fooof_data, parquet_file_path)

# Verify the Parquet file was created
if (file.exists(parquet_file_path)) {
  file_size_mb <- round(file.size(parquet_file_path) / (1024 * 1024), 2)
  cat(crayon::green("✓ Successfully created Parquet file!\n"))
  cat(crayon::green("  File size:", file_size_mb, "MB\n"))
} else {
  cat(crayon::red("✗ Failed to create Parquet file!\n"))
}
