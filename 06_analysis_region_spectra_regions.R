region_spectra_data

# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║              APERIODIC SPECTRA BY LOBE PLOT                ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Create a combined group-sex variable for easier plotting
cat(crayon::blue("Preparing data for aperiodic spectra by lobe comparison...\n"))

# Ensure we have the lobe information in the data
region_spectra_data_atlas <- region_spectra_data %>%
  mutate(node = map_roi_to_atlas(node)) %>% 
  left_join(atlas, by = c("node" = "Name"))

# Prepare data: average aperiodic spectra by lobe for each subject, frequency, group, and sex
aperiodic_lobe_data <- region_spectra_data_atlas %>%
  group_by(eegid, freq, group, sex, lobe) %>%
  summarize(mean_aperiodic = mean(aperiodic_spec, na.rm = TRUE), .groups = "drop") %>%
  mutate(group_sex = paste(group, sex, sep = "-"))

# Calculate group means and standard errors for plotting by lobe
aperiodic_lobe_summary <- aperiodic_lobe_data %>%
  group_by(freq, group_sex, lobe) %>%
  summarize(
    mean_aperiodic = mean(mean_aperiodic, na.rm = TRUE),
    se_aperiodic = sd(mean_aperiodic, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(!is.na(lobe))  # Remove any entries with missing lobe information

# Normalize the spectra to have the same intercept
normalized_lobe_summary <- aperiodic_lobe_summary %>%
  group_by(group_sex, lobe) %>%
  mutate(mean_aperiodic = mean_aperiodic - min(mean_aperiodic)) %>%
  ungroup()

# Define custom colors for the lobes
lobe_colors <- c(
  "Frontal" = "#1E88E5",     # Blue
  "Temporal" = "#FFC107",    # Amber
  "Parietal" = "#D81B60",    # Pink
  "Occipital" = "#004D40",   # Teal
  "Limbic" = "#8E24AA",      # Purple
  "Subcortical" = "#F57C00"  # Orange
)

# Create a plot for each group-sex combination
cat(crayon::blue("Creating normalized aperiodic spectra by lobe plots for each group...\n"))

# Function to create a plot for a specific group-sex combination
create_group_plot <- function(data, group_sex_value) {
  filtered_data <- data %>% filter(group_sex == group_sex_value)
  
  # Extract group and sex for the title
  group_name <- strsplit(group_sex_value, "-")[[1]][1]
  sex_name <- ifelse(strsplit(group_sex_value, "-")[[1]][2] == "F", "Female", "Male")
  
  ggplot(filtered_data, aes(x = freq, y = mean_aperiodic, color = lobe)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = mean_aperiodic - se_aperiodic, 
                    ymax = mean_aperiodic + se_aperiodic, 
                    fill = lobe), 
                alpha = 0.2, color = NA) +
    scale_color_manual(values = lobe_colors, name = "Brain Lobe") +
    scale_fill_manual(values = lobe_colors, name = "Brain Lobe") +
    labs(
      title = paste(group_name, sex_name, "Normalized Aperiodic Spectra by Lobe"),
      x = "Frequency (Hz)",
      y = "Normalized Power (dB)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
      axis.title = element_text(face = "bold")
    )
}

# Create individual plots for each group-sex combination using normalized data
fxs_f_plot <- create_group_plot(normalized_lobe_summary, "FXS-F")
fxs_m_plot <- create_group_plot(normalized_lobe_summary, "FXS-M")
tdc_f_plot <- create_group_plot(normalized_lobe_summary, "TDC-F")
tdc_m_plot <- create_group_plot(normalized_lobe_summary, "TDC-M")

# Combine the plots in a 2x2 grid
combined_lobe_plot <- (fxs_f_plot + fxs_m_plot) / (tdc_f_plot + tdc_m_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparison of Normalized Aperiodic Spectra by Brain Lobe Across Groups",
    subtitle = "Each spectrum normalized to start at zero",
    caption = "Shaded areas represent standard error",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(hjust = 1, size = 10, color = "grey50")
    )
  )

# Save the combined plot
plot_path <- file.path(project_path, "figures", "normalized_aperiodic_spectra_by_lobe.png")
dir.create(file.path(project_path, "figures"), showWarnings = FALSE)
ggsave(plot_path, combined_lobe_plot, width = 14, height = 12, dpi = 300)

# Display the combined plot
print(combined_lobe_plot)

cat(crayon::green("✓ Normalized aperiodic spectra by lobe plots created successfully!\n"))
cat(crayon::green(paste0("✓ Combined plot saved to: ", plot_path, "\n")))
cat(crayon::bold$cyan("═══════════════════════════════════════════════════════════════════════════\n"))
