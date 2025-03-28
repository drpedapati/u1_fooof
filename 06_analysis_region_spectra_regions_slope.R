region_spectra_data

# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║              APERIODIC SLOPE BY LOBE PLOT                  ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Create a combined group-sex variable for easier plotting
cat(crayon::blue("Preparing data for aperiodic slope by lobe comparison...\n"))

# Ensure we have the lobe information in the data
region_fooof_data_atlas <- region_fooof_data %>%
  mutate(node = map_roi_to_atlas(node)) %>% 
  left_join(atlas, by = c("node" = "Name"))

# Use the pre-calculated aperiodic exponent (slope) from the dataset
cat(crayon::blue("Using pre-calculated aperiodic exponents for each region...\n"))

# First, average across nodes within each lobe for each subject
# This ensures we're capturing subject-level variability correctly
lobe_slopes_by_subject <- region_fooof_data_atlas %>%
  filter(!is.na(lobe)) %>%  # Remove any entries with missing lobe information
  mutate(group_sex = paste(group, sex, sep = "-")) %>%
  # Group by subject, lobe, and demographic info
  group_by(eegid, group, sex, lobe, group_sex) %>%
  # Calculate mean slope for each lobe for each subject
  summarize(
    subject_lobe_slope = mean(aperiodic_exponent, na.rm = TRUE),
    .groups = "drop"
  )

# Now calculate group-level statistics from subject-level averages
lobe_slope_summary <- lobe_slopes_by_subject %>%
  group_by(group_sex, lobe) %>%
  summarize(
    # Mean across subjects (not nodes)
    mean_slope = mean(subject_lobe_slope, na.rm = TRUE),
    # Standard error across subjects
    se_slope = sd(subject_lobe_slope, na.rm = TRUE) / sqrt(n()),
    # Count number of subjects for verification
    n_subjects = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(lobe))

# Print summary to verify we have subject-level variability
cat(crayon::blue("Summary of subject counts and variability by group and lobe:\n"))
print(lobe_slope_summary %>% select(group_sex, lobe, n_subjects, mean_slope, se_slope))

# Define custom colors for the lobes with an improved color scheme
# Including colors for all possible lobes including Cingulate, Central, and Prefrontal
lobe_colors <- c(
  "Frontal" = "#3498db",       # Soft blue
  "Prefrontal" = "#2980b9",    # Darker blue
  "Temporal" = "#e74c3c",      # Soft red
  "Parietal" = "#9b59b6",      # Purple
  "Occipital" = "#2ecc71",     # Green
  "Subcortical" = "#1abc9c",   # Turquoise
  "Cingulate" = "#d35400",     # Burnt orange
  "Central" = "#8e44ad"        # Violet
)

# Create a plot for each group-sex combination
cat(crayon::blue("Creating aperiodic slope by lobe plots for each group...\n"))

# Function to create a plot for a specific group-sex combination
create_group_plot <- function(data, group_sex_value) {
  filtered_data <- data %>% filter(group_sex == group_sex_value)
  
  # Extract group and sex for the title
  group_name <- strsplit(group_sex_value, "-")[[1]][1]
  sex_name <- ifelse(strsplit(group_sex_value, "-")[[1]][2] == "F", "Female", "Male")
  
  ggplot(filtered_data, aes(x = lobe, y = mean_slope, fill = lobe)) +
    geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_slope - se_slope, 
                      ymax = mean_slope + se_slope),
                  width = 0.2, position = position_dodge(0.9)) +
    scale_fill_manual(values = lobe_colors, name = "Brain Lobe") +
    labs(
      title = paste(group_name, sex_name, "Aperiodic Slope by Brain Lobe"),
      x = "Brain Lobe",
      y = "Aperiodic Exponent (Slope)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Create individual plots for each group-sex combination
fxs_f_plot <- create_group_plot(lobe_slope_summary, "FXS-F")
fxs_m_plot <- create_group_plot(lobe_slope_summary, "FXS-M")
tdc_f_plot <- create_group_plot(lobe_slope_summary, "TDC-F")
tdc_m_plot <- create_group_plot(lobe_slope_summary, "TDC-M")

# Combine the plots in a 2x2 grid
combined_lobe_plot <- (fxs_f_plot + fxs_m_plot) / (tdc_f_plot + tdc_m_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparison of Aperiodic Slopes by Brain Lobe Across Groups",
    subtitle = "Higher values indicate shallower 1/f slopes",
    caption = "Error bars represent standard error",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(hjust = 1, size = 10, color = "grey50")
    )
  )

# Save the combined plot
plot_path <- file.path(project_path, "figures", "aperiodic_slopes_by_lobe.png")
dir.create(file.path(project_path, "figures"), showWarnings = FALSE)
ggsave(plot_path, combined_lobe_plot, width = 14, height = 12, dpi = 300)

# Display the combined plot
print(combined_lobe_plot)

# Also create a direct comparison plot across groups for each lobe
lobe_comparison_plot <- ggplot(lobe_slope_summary, 
                              aes(x = group_sex, y = mean_slope, fill = group_sex)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_slope - se_slope, 
                    ymax = mean_slope + se_slope),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ lobe, scales = "free_y") +
  scale_fill_manual(values = c(
    "FXS-F" = "#ff7f0e",  # Vibrant orange for FXS Female
    "FXS-M" = "#d62728",  # Red for FXS Male
    "TDC-F" = "#1f77b4",  # Blue for TDC Female
    "TDC-M" = "#2ca02c"   # Green for TDC Male
  ), 
  name = "Group",
  labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  labs(
    title = "Aperiodic Slope Comparison Across Groups by Brain Lobe",
    x = "Group",
    y = "Aperiodic Exponent (Slope)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the comparison plot
comparison_plot_path <- file.path(project_path, "figures", "aperiodic_slopes_comparison_by_lobe.png")
ggsave(comparison_plot_path, lobe_comparison_plot, width = 14, height = 10, dpi = 300)

cat(crayon::green("✓ Aperiodic slope by lobe plots created successfully!\n"))
cat(crayon::green(paste0("✓ Combined plot saved to: ", plot_path, "\n")))
cat(crayon::green(paste0("✓ Comparison plot saved to: ", comparison_plot_path, "\n")))
cat(crayon::bold$cyan("═══════════════════════════════════════════════════════════════════════════\n"))
