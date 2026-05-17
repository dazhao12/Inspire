suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

summary_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Word_Aligned_first_nonMAC_bilingual_summary_3_30_2026"
output_dir <- file.path(summary_dir, "ppt_figures_3_30_2026")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
demo_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_first_nonMAC_3_30_2026"

n_anchor <- 112042

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, colour = "#4d4d4d"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

save_plot <- function(plot_obj, file_name, width = 10, height = 6) {
  ggsave(
    filename = file.path(output_dir, file_name),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

auto_height <- function(n_items, base_height = 4.5, per_item = 0.28, max_height = 16) {
  min(max_height, base_height + per_item * n_items)
}

demo_raw <- fread(file.path(demo_dir, "Demographic_Operation_Level.csv"))
comorb <- fread(file.path(summary_dir, "comorbidity_binary_en.csv"))
acute <- fread(file.path(summary_dir, "acute_status_en.csv"))
meds <- fread(file.path(summary_dir, "medications_en.csv"))
labs_window <- fread(file.path(summary_dir, "labs_window_en.csv"))
labs_current <- fread(file.path(summary_dir, "labs_current_nearest_en.csv"))
vitals <- fread(file.path(summary_dir, "preop_vitals_en.csv"))
intraop <- fread(file.path(summary_dir, "intraop_en.csv"))
outcomes <- fread(file.path(summary_dir, "outcomes_binary_en.csv"))
overview <- fread(file.path(summary_dir, "overview_en.csv"))

demo_num_long <- rbindlist(list(
  data.table(Variable = "Age", value = demo_raw$Age),
  data.table(Variable = "Height", value = demo_raw$Height),
  data.table(Variable = "Weight", value = demo_raw$Weight),
  data.table(Variable = "BMI", value = demo_raw$BMI)
), fill = TRUE)
demo_num_long <- demo_num_long[!is.na(value)]

p0 <- ggplot(demo_num_long, aes(x = value)) +
  geom_histogram(fill = "#355070", color = "white", bins = 40) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  labs(
    title = "Demographic Distributions",
    subtitle = sprintf("Anchor cohort: n = %s first non-MAC operations", comma(n_anchor)),
    x = "Value",
    y = "Count"
  ) +
  base_theme
save_plot(p0, "figure00_demographics_distribution.png", width = 10, height = 8)

demo_missing <- data.table(
  Variable = c("Age", "Height", "Weight", "BMI", "ASA", "Race"),
  Missing = c(
    100 * mean(is.na(demo_raw$Age)),
    100 * mean(is.na(demo_raw$Height)),
    100 * mean(is.na(demo_raw$Weight)),
    100 * mean(is.na(demo_raw$BMI)),
    100 * mean(is.na(demo_raw$asa)),
    100 * mean(is.na(demo_raw$race))
  )
)
demo_missing[, Variable := factor(Variable, levels = rev(Variable))]
p0b <- ggplot(demo_missing, aes(x = Variable, y = Missing / 100)) +
  geom_col(fill = "#BC4749", width = 0.72) +
  geom_text(aes(label = sprintf("%.2f%%", Missing)), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Demographic Missingness",
    subtitle = "Key baseline variables in the anchor-operation table",
    x = NULL,
    y = "Missingness"
  ) +
  base_theme
save_plot(p0b, "figure00b_demographics_missingness.png")

plot_comorb <- copy(comorb[order(-prevalence_pct)][1:15])
plot_comorb[, Variable := factor(Variable, levels = rev(Variable))]
p1 <- ggplot(plot_comorb, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_col(fill = "#2F6B5F", width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top Comorbidities",
    subtitle = sprintf("Anchor cohort: n = %s first non-MAC operations", comma(n_anchor)),
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p1, "figure01_comorbidities_top15.png")

plot_comorb_30 <- copy(comorb[order(-prevalence_pct)][1:30])
plot_comorb_30[, Variable := factor(Variable, levels = rev(Variable))]
p1b <- ggplot(plot_comorb_30, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_col(fill = "#2F6B5F", width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.1, size = 3.4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Top 30 Comorbidities",
    subtitle = sprintf("Anchor cohort: n = %s first non-MAC operations", comma(n_anchor)),
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p1b, "figure01_comorbidities_top30.png", width = 11, height = 10)

plot_comorb_all <- copy(comorb[order(-prevalence_pct)])
plot_comorb_all[, Variable := factor(Variable, levels = rev(Variable))]
p1c <- ggplot(plot_comorb_all, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_point(size = 2.6, colour = "#1B4332") +
  geom_segment(aes(xend = Variable, y = 0, yend = prevalence_pct / 100), colour = "#74A892", linewidth = 1) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "All Comorbidities",
    subtitle = "Full baseline comorbidity profile",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p1c, "figure01b_comorbidities_all.png", width = 10, height = auto_height(nrow(plot_comorb_all), 4.5, 0.26, 12))

plot_acute <- copy(acute[order(-prevalence_pct)])
plot_acute[, Variable := factor(Variable, levels = rev(Variable))]
p2 <- ggplot(plot_acute, aes(x = Variable, y = prevalence_pct / 100, fill = median_interval_min / 1440)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.2f%%", prevalence_pct)), hjust = -0.1, size = 3.6) +
  coord_flip() +
  scale_fill_gradient(low = "#B9D7EA", high = "#1D4E89", labels = function(x) sprintf("%.1f d", x), name = "Median interval") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Acute Events Within 3 Months Before Surgery",
    subtitle = "Bar length = prevalence; color = median time-to-surgery",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p2, "figure02_acute_status.png")

plot_meds <- copy(meds[order(-prevalence_pct)][1:15])
plot_meds[, Variable := factor(Variable, levels = rev(Variable))]
p3 <- ggplot(plot_meds, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_col(fill = "#7B3F00", width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top Preoperative Medication Classes",
    subtitle = "Medication exposure within the longer of 2 weeks pre-op or admission-to-OR",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p3, "figure03_medications_top15.png")

plot_meds_30 <- copy(meds[order(-prevalence_pct)][1:30])
plot_meds_30[, Variable := factor(Variable, levels = rev(Variable))]
p3b <- ggplot(plot_meds_30, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_col(fill = "#7B3F00", width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), hjust = -0.1, size = 3.4) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Top 30 Preoperative Medication Classes",
    subtitle = "Medication exposure within the longer of 2 weeks pre-op or admission-to-OR",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p3b, "figure03_medications_top30.png", width = 11, height = 10)

plot_meds_all <- copy(meds[order(-prevalence_pct)])
plot_meds_all[, Variable := factor(Variable, levels = rev(Variable))]
p3c <- ggplot(plot_meds_all, aes(x = Variable, y = prevalence_pct / 100)) +
  geom_point(size = 2.3, colour = "#7F5539") +
  geom_segment(aes(xend = Variable, y = 0, yend = prevalence_pct / 100), colour = "#D4A373", linewidth = 1) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "All Preoperative Medication Classes",
    subtitle = "Full medication exposure profile",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p3c, "figure03b_medications_all.png", width = 10.5, height = auto_height(nrow(plot_meds_all), 4.5, 0.22, 14))

labs_cov <- copy(labs_window[, .(
  Window = Variable,
  Any_lab = pct_ops_any_lab,
  Creatinine = pct_creatinine_nearest,
  Hemoglobin = pct_hb_nearest,
  WBC = pct_wbc_nearest
)])
labs_cov_long <- melt(labs_cov, id.vars = "Window", variable.name = "Marker", value.name = "Coverage")
labs_cov_long[, Window := factor(Window, levels = labs_cov$Window)]
p4 <- ggplot(labs_cov_long, aes(x = Window, y = Coverage / 100, fill = Marker)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(Any_lab = "#264653", Creatinine = "#2A9D8F", Hemoglobin = "#E76F51", WBC = "#E9C46A")) +
  labs(
    title = "Preoperative Lab Coverage by Window",
    subtitle = "Current-stay preop window = admission_time <= chart_time < OR-in, after stay assignment",
    x = NULL,
    y = "Coverage"
  ) +
  base_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_plot(p4, "figure04_lab_window_coverage.png", width = 11, height = 6)

plot_labs_missing <- copy(labs_current[, .(Variable, missing_pct)])
plot_labs_missing <- plot_labs_missing[order(-missing_pct)]
plot_labs_missing[, Variable := factor(Variable, levels = rev(Variable))]
p4b <- ggplot(plot_labs_missing, aes(x = Variable, y = missing_pct / 100)) +
  geom_col(fill = "#A63D40", width = 0.72) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Missingness Across Current-Stay Nearest Labs",
    subtitle = "Each variable uses the nearest preoperative value within the current-stay window",
    x = NULL,
    y = "Missingness"
  ) +
  base_theme
save_plot(p4b, "figure04b_labs_missingness_all.png", width = 10.5, height = auto_height(nrow(plot_labs_missing), 4.5, 0.2, 14))

plot_labs_dist <- copy(labs_current[, .(Variable, median, p25, p75)])
plot_labs_dist <- plot_labs_dist[order(-median)]
plot_labs_dist[, Variable := factor(Variable, levels = rev(Variable))]
p4c <- ggplot(plot_labs_dist, aes(x = Variable, y = median)) +
  geom_linerange(aes(ymin = p25, ymax = p75), colour = "#7B6D8D", linewidth = 1.8, alpha = 0.85) +
  geom_point(size = 2.4, colour = "#2D1E2F") +
  coord_flip() +
  labs(
    title = "Distributions of Current-Stay Nearest Labs",
    subtitle = "Point = median; line = IQR",
    x = NULL,
    y = "Observed value"
  ) +
  base_theme
save_plot(p4c, "figure04c_labs_distribution_all.png", width = 10.5, height = auto_height(nrow(plot_labs_dist), 4.5, 0.2, 14))

plot_vitals_cov <- copy(vitals[, .(Variable = Category, Coverage_Pct, pct_missing)])
plot_vitals_cov[, Variable := factor(Variable, levels = rev(Variable))]
p5 <- ggplot(plot_vitals_cov, aes(x = Variable)) +
  geom_col(aes(y = Coverage_Pct / 100), fill = "#2A6F97", width = 0.7) +
  geom_col(aes(y = -pct_missing / 100), fill = "#D1495B", width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey40") +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(abs(x) * 100, "%")) +
  labs(
    title = "Preoperative Vital Sign Coverage and Missingness",
    subtitle = "Blue = available; red = missing",
    x = NULL,
    y = "Percent"
  ) +
  base_theme
save_plot(p5, "figure05_preop_vitals_coverage_missingness.png")

plot_vitals_dist <- copy(vitals[, .(Variable = Category, Median = Median, Q1 = Q1, Q3 = Q3)])
plot_vitals_dist[, Variable := factor(Variable, levels = rev(Variable))]
p6 <- ggplot(plot_vitals_dist, aes(x = Variable, y = Median)) +
  geom_linerange(aes(ymin = Q1, ymax = Q3), colour = "#7C6A0A", linewidth = 2.2, alpha = 0.8) +
  geom_point(size = 3.5, colour = "#1F3B4D") +
  coord_flip() +
  labs(
    title = "Preoperative Vital Sign Distributions",
    subtitle = "Point = median; line = IQR",
    x = NULL,
    y = "Observed value"
  ) +
  base_theme
save_plot(p6, "figure06_preop_vitals_distribution.png")

plot_intra <- copy(intraop[order(-Pct_Users)][1:12])
plot_intra[, Variable := factor(Variable, levels = rev(Variable))]
p7 <- ggplot(plot_intra, aes(x = Variable, y = Pct_Users / 100, fill = Aggregation)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.1f%%", Pct_Users)), hjust = -0.1, size = 3.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = c(sum = "#8C5E58", mean = "#457B9D", any_use = "#2A9D8F")) +
  labs(
    title = "Most Common Intraoperative Drugs, Fluids, and Events",
    subtitle = "Top variables ranked by user rate",
    x = NULL,
    y = "User rate"
  ) +
  base_theme
save_plot(p7, "figure07_intraop_top_usage.png")

plot_intra_all <- copy(intraop[order(-Pct_Users)])
plot_intra_all[, Variable := factor(Variable, levels = rev(Variable))]
p7b <- ggplot(plot_intra_all, aes(x = Variable, y = Pct_Users / 100, colour = Aggregation)) +
  geom_point(size = 2.1) +
  geom_segment(aes(xend = Variable, y = 0, yend = Pct_Users / 100), linewidth = 0.9, alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c(sum = "#8C5E58", mean = "#457B9D", any_use = "#2A9D8F")) +
  labs(
    title = "All Intraoperative Variables",
    subtitle = "User rate across drugs, fluids, and event variables",
    x = NULL,
    y = "User rate"
  ) +
  base_theme
save_plot(p7b, "figure07b_intraop_all.png", width = 11, height = auto_height(nrow(plot_intra_all), 4.5, 0.16, 16))

plot_outcomes <- copy(outcomes[order(-prevalence_pct)][1:15])
plot_outcomes[, Variable := factor(Variable, levels = rev(Variable))]
p8 <- ggplot(plot_outcomes, aes(x = Variable, y = prevalence_pct / 100, fill = missing_pct)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.2f%%", prevalence_pct)), hjust = -0.1, size = 3.6) +
  coord_flip() +
  scale_fill_gradient(low = "#DCEAF7", high = "#4C78A8", name = "Missing %") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Top Postoperative Outcomes",
    subtitle = "Ranked by prevalence in the first 30 days after OR-out",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p8, "figure08_outcomes_top15.png")

plot_outcomes_30 <- copy(outcomes[order(-prevalence_pct)][1:30])
plot_outcomes_30[, Variable := factor(Variable, levels = rev(Variable))]
p8b <- ggplot(plot_outcomes_30, aes(x = Variable, y = prevalence_pct / 100, fill = missing_pct)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = sprintf("%.2f%%", prevalence_pct)), hjust = -0.1, size = 3.3) +
  coord_flip() +
  scale_fill_gradient(low = "#DCEAF7", high = "#4C78A8", name = "Missing %") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Top 30 Postoperative Outcomes",
    subtitle = "Ranked by prevalence in the first 30 days after OR-out",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p8b, "figure08_outcomes_top30.png", width = 11, height = 10)

plot_outcomes_all <- copy(outcomes[order(-prevalence_pct)])
plot_outcomes_all[, Variable := factor(Variable, levels = rev(Variable))]
p8c <- ggplot(plot_outcomes_all, aes(x = Variable, y = prevalence_pct / 100, colour = median_interval_min / 1440)) +
  geom_point(size = 2.4) +
  geom_segment(aes(xend = Variable, y = 0, yend = prevalence_pct / 100), linewidth = 1, alpha = 0.7) +
  coord_flip() +
  scale_colour_gradient(low = "#BDE0FE", high = "#1D3557", labels = function(x) sprintf("%.1f d", x), name = "Median interval") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "All Postoperative Outcomes",
    subtitle = "Full postoperative event profile with median interval after surgery",
    x = NULL,
    y = "Prevalence"
  ) +
  base_theme
save_plot(p8c, "figure08b_outcomes_all.png", width = 11, height = auto_height(nrow(plot_outcomes_all), 4.5, 0.19, 15))

block_missing <- rbindlist(list(
  data.table(Block = "Demographics BMI", Missing = 100 * mean(is.na(fread("/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Demographics_Timeline_first_nonMAC_3_30_2026/Demographic_Operation_Level.csv", select = "BMI")$BMI))),
  data.table(Block = "Current-stay any lab", Missing = 100 - labs_window[window == "history_preop_current_stay", pct_ops_any_lab]),
  data.table(Block = "Current-stay creatinine", Missing = 100 - labs_window[window == "history_preop_current_stay", pct_creatinine_nearest]),
  data.table(Block = "Current-stay hemoglobin", Missing = 100 - labs_window[window == "history_preop_current_stay", pct_hb_nearest]),
  data.table(Block = "Preop SpO2", Missing = vitals[Category == "SpO2", pct_missing]),
  data.table(Block = "Preop MBP", Missing = vitals[Category == "Mean BP", pct_missing]),
  data.table(Block = "Intraop timeseries ops", Missing = 100 * (1 - overview[variable == "intraop_timeseries_unique_ops", value] / overview[variable == "anchor_operations", value]))
))
block_missing[, Block := factor(Block, levels = rev(Block))]
p9 <- ggplot(block_missing, aes(x = Block, y = Missing / 100)) +
  geom_col(fill = "#B23A48", width = 0.72) +
  geom_text(aes(label = sprintf("%.1f%%", Missing)), hjust = -0.1, size = 3.8) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Selected Missingness Snapshot",
    subtitle = "Useful for one PPT slide on data completeness",
    x = NULL,
    y = "Missingness"
  ) +
  base_theme
save_plot(p9, "figure09_missingness_snapshot.png")

cat(sprintf("Saved PPT-friendly figures to %s\n", output_dir))
