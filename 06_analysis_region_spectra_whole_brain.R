region_spectra_data

# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║              APERIODIC SPECTRA COMPARISON PLOT             ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Create a combined group-sex variable for easier plotting
cat(crayon::blue("Preparing data for aperiodic spectra comparison...\n"))

# Prepare data: average aperiodic spectra across regions for each subject, frequency, group, and sex
aperiodic_data <- region_spectra_data %>%
  group_by(eegid, freq, group, sex) %>%
  summarize(mean_aperiodic = mean(aperiodic_spec, na.rm = TRUE), .groups = "drop") %>%
  mutate(group_sex = paste(group, sex, sep = "-"))

# Calculate group means and standard errors for plotting
aperiodic_summary <- aperiodic_data %>%
  group_by(freq, group_sex) %>%
  summarize(
    mean_aperiodic = mean(mean_aperiodic, na.rm = TRUE),
    se_aperiodic = sd(mean_aperiodic, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Define custom colors for the groups
group_colors <- c(
  "FXS-F" = "#FF5733",  # Orange-red for FXS Female
  "FXS-M" = "#C70039",  # Dark red for FXS Male
  "TDC-F" = "#800080",  # Purple for TDC Female
  "TDC-M" = "#0D47A1"   # Dark blue for TDC Male
)

# Create the first plot
cat(crayon::blue("Creating aperiodic spectra comparison plot...\n"))

aperiodic_plot <- ggplot(aperiodic_summary, aes(x = freq, y = mean_aperiodic, color = group_sex)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_aperiodic - se_aperiodic, 
                  ymax = mean_aperiodic + se_aperiodic, 
                  fill = group_sex), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = group_colors, 
                     name = "Group",
                     labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  scale_fill_manual(values = group_colors, 
                    name = "Group",
                    labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  labs(
    title = "Comparison of Aperiodic Spectra by Group and Sex",
    subtitle = "Averaged across all brain regions",
    x = "Frequency (Hz)",
    y = "Power (dB)",
    caption = "Shaded areas represent standard error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    aspect.ratio = 1,
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1, size = 10, color = "grey50")
  )

# Create the second plot with normalized intercept
normalized_summary <- aperiodic_summary %>%
  group_by(group_sex) %>%
  mutate(mean_aperiodic = mean_aperiodic - min(mean_aperiodic)) %>%
  ungroup()

normalized_plot <- ggplot(normalized_summary, aes(x = freq, y = mean_aperiodic, color = group_sex)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_aperiodic - se_aperiodic, 
                  ymax = mean_aperiodic + se_aperiodic, 
                  fill = group_sex), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = group_colors, 
                     name = "Group",
                     labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  scale_fill_manual(values = group_colors, 
                    name = "Group",
                    labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  labs(
    title = "Normalized Aperiodic Spectra Comparison by Group and Sex",
    subtitle = "Averaged across all brain regions",
    x = "Frequency (Hz)",
    y = "Normalized Power (dB)",
    caption = "Shaded areas represent standard error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    aspect.ratio = 1,
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1, size = 10, color = "grey50")
  )

# Combine the plots side by side
combined_plot <- aperiodic_plot + normalized_plot + plot_layout(ncol = 2)

# Save the combined plot
plot_path <- file.path(project_path, "figures", "aperiodic_spectra_comparison_combined.png")
dir.create(file.path(project_path, "figures"), showWarnings = FALSE)
ggsave(plot_path, combined_plot, width = 14, height = 7, dpi = 300)

# Display the combined plot
print(combined_plot)

cat(crayon::green("✓ Aperiodic spectra comparison plots created successfully!\n"))
cat(crayon::green(paste0("✓ Combined plot saved to: ", plot_path, "\n")))
cat(crayon::bold$cyan("═══════════════════════════════════════════════════════════════════════════\n"))
