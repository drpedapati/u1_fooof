# ============================================================================
# METADATA LOADING
# ============================================================================
# This script loads essential metadata for the SPG302 project analysis:
#
# 1. Desikan-Killiany (DK) Atlas Information:
#    - Loads the 68-region brain atlas dictionary from the VHTP repository
#    - Extracts region names, anatomical information, acronyms, and lobe assignments
#    - Used for mapping EEG channels to brain regions
#
# 2. Treatment Group Information:
#    - Loads participant treatment codes from the metadata directory
#    - Standardizes subject IDs by removing dashes from screen identifiers
#    - Creates a clean mapping between subject IDs and their treatment conditions
# ============================================================================

# Define data source paths
atlas_url <- "https://raw.githubusercontent.com/cincibrainlab/vhtp/refs/heads/main/chanfiles/DK_atlas-68_dict.csv"
treatment_codes_file <- file.path(metadata_path, "GroupAssignments.csv")

cat("\n", bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", bold$cyan("║                    LOADING METADATA                        ║"))
cat("\n", bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))

# Import atlas data
cat(green("→ Loading Desikan-Killiany (DK) Atlas data...\n"))
atlas <- read_csv(atlas_url, show_col_types = FALSE) %>% 
  select(Name, region, Acronyms, lobe)
cat(green(paste0("✓ Atlas loaded successfully with ", nrow(atlas), " brain regions\n")))

# Remove dash in subject column
cat(green("→ Loading treatment codes and standardizing subject IDs...\n"))
treatment_codes <- read_csv(treatment_codes_file, show_col_types = FALSE) 
cat(green(paste0("✓ Treatment data loaded for ", nrow(treatment_codes), " subjects\n")))

cat(bold$cyan("════════════════ Metadata Loading Complete ════════════════\n"))