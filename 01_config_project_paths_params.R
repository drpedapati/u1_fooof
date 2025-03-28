# ============================================================================
# PROJECT PATHS CONFIGURATION
# ============================================================================
# This script sets up all project paths dynamically and creates directories
# if they don't exist. It also displays a formatted table of all paths.

# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║                     PROJECT PATHS CONFIGURATION            ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here


# Define base paths with more robust approach
base_path <- normalizePath("/Users/ernie/Downloads/u1_fooof", mustWork = FALSE)
project_path <- normalizePath("/Users/ernie/Downloads/u1_fooof", mustWork = FALSE)
results_path <- "/Users/ernie/Downloads/u1_fooof"

# Create a list of all project paths
project_paths <- list(
  base_path = base_path,
  project_path = project_path,
  figures_path = file.path(project_path, "final_figures"),
  metadata_path = file.path(project_path, "metadata"),
  final_data_path = file.path(project_path, "final_data"),
  scripts_path = project_path,
  reports_path = file.path(project_path, "reports"),
  datasets_path = file.path(project_path, "datasets")
)

# Create directories if they don't exist
invisible(lapply(project_paths, function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    cat(crayon::green(paste("✓ Created directory:", path, "\n")))
  }
}))

# Assign paths to individual variables for backward compatibility
list2env(project_paths, envir = environment())
# Function to format path display
format_path_row <- function(name, path, exists) {
  status <- if(exists) crayon::green("✓") else crayon::red("✗")
  path_display <- if(exists) crayon::green(path) else crayon::red(path)
  paste0(
    crayon::yellow(sprintf("%-20s", name)), " │ ",
    path_display, " │ ",
    status
  )
}

# Check existence and print paths
cat(crayon::yellow(sprintf("%-20s │ %-30s │ %s\n", "PATH NAME", "LOCATION", "STATUS")))
cat(crayon::yellow(paste(rep("─", 90), collapse = "")), "\n")

for (name in names(project_paths)) {
  path_exists <- dir.exists(project_paths[[name]])
  cat(format_path_row(name, project_paths[[name]], path_exists), "\n")
}

cat(crayon::bold$cyan("═══════════════════════════════════════════════════════════════════════════\n"))