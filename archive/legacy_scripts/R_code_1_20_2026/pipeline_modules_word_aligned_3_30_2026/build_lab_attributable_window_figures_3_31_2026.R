#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

processed_dir <- "/N/project/analgesia_perioperation/data/INSPIRE_1.3/processed/Lab_Attributable_Window_Coverage_3_31_2026"
figure_dir <- file.path(processed_dir, "figures")
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

coverage <- fread(file.path(processed_dir, "lab_attributable_window_coverage_summary.csv"))
item_cov <- fread(file.path(processed_dir, "lab_attributable_window_item_coverage.csv"))

window_order <- c(
  "current_stay_preop",
  "attributable_60d",
  "attributable_30d",
  "attributable_15d",
  "attributable_7d",
  "cumulative_preop"
)

window_label_map <- c(
  current_stay_preop = "Current stay",
  attributable_60d = "Attributable 60d",
  attributable_30d = "Attributable 30d",
  attributable_15d = "Attributable 15d",
  attributable_7d = "Attributable 7d",
  cumulative_preop = "Cumulative preop"
)

coverage[, window := factor(window, levels = window_order)]
item_cov[, window := factor(window, levels = window_order)]
coverage[, window_label := factor(window_label_map[as.character(window)], levels = unname(window_label_map[window_order]))]
item_cov[, window_label := factor(window_label_map[as.character(window)], levels = unname(window_label_map[window_order]))]

overall_long <- melt(
  coverage[, .(window_label, pct_ops_any_lab, pct_hb, pct_creatinine, pct_wbc)],
  id.vars = "window_label",
  variable.name = "metric",
  value.name = "coverage_pct"
)
metric_label_map <- c(
  pct_ops_any_lab = "Any lab",
  pct_hb = "Hemoglobin",
  pct_creatinine = "Creatinine",
  pct_wbc = "WBC"
)
overall_long[, metric := factor(metric_label_map[metric], levels = c("Any lab", "Hemoglobin", "Creatinine", "WBC"))]

p1 <- ggplot(overall_long, aes(x = window_label, y = coverage_pct, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  geom_text(
    aes(label = sprintf("%.1f%%", coverage_pct)),
    position = position_dodge(width = 0.75),
    vjust = -0.2,
    size = 3
  ) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  scale_fill_manual(values = c("#194d66", "#c45b3c", "#3f7d20", "#8d6a9f")) +
  labs(
    title = "Preoperative Lab Coverage Across Candidate Windows",
    subtitle = "Overall coverage and key markers under different attributable-window definitions",
    x = NULL,
    y = "Coverage",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 18, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(figure_dir, "figure_lab_window_coverage_overall.png"),
  plot = p1,
  width = 12,
  height = 6.5,
  dpi = 320
)

selected_items <- c("hb", "creatinine", "wbc", "albumin", "hba1c")
selected_label_map <- c(
  hb = "Hemoglobin",
  creatinine = "Creatinine",
  wbc = "WBC",
  albumin = "Albumin",
  hba1c = "HbA1c"
)
selected_dt <- item_cov[item_name %in% selected_items]
selected_dt[, item_label := factor(selected_label_map[item_name], levels = selected_label_map[selected_items])]

p2 <- ggplot(selected_dt, aes(x = window_label, y = pct_ops_with_item, color = item_label, group = item_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  geom_text(aes(label = sprintf("%.1f%%", pct_ops_with_item)), vjust = -0.65, size = 3, show.legend = FALSE) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 105)) +
  scale_color_manual(values = c("#c45b3c", "#3f7d20", "#194d66", "#b38a14", "#8d6a9f")) +
  labs(
    title = "Coverage of Representative Labs by Window",
    subtitle = "Coverage rises sharply once attributable preadmission labs are allowed",
    x = NULL,
    y = "Coverage",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 18, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(figure_dir, "figure_lab_window_selected_items.png"),
  plot = p2,
  width = 12,
  height = 6.5,
  dpi = 320
)

heatmap_dt <- copy(item_cov[window %in% window_order[1:5]])
heatmap_dt <- heatmap_dt[item_name %in% item_cov[window == "attributable_30d"][order(-pct_ops_with_item)][1:20, item_name]]
heatmap_dt[, item_name := factor(item_name, levels = rev(unique(heatmap_dt[item_cov[window == "attributable_30d"][order(-pct_ops_with_item)][1:20, item_name], on = "item_name"]$item_name)))]

p3 <- ggplot(heatmap_dt, aes(x = window_label, y = item_name, fill = pct_ops_with_item)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f", pct_ops_with_item)), size = 2.8) +
  scale_fill_gradient(low = "#f7efe5", high = "#1f5c7a", labels = label_percent(scale = 1)) +
  labs(
    title = "Top 20 Lab Items: Coverage Heatmap by Window",
    subtitle = "Restricted to current-stay and attributable windows",
    x = NULL,
    y = NULL,
    fill = "Coverage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 18, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  filename = file.path(figure_dir, "figure_lab_window_heatmap_top20.png"),
  plot = p3,
  width = 11.5,
  height = 9.5,
  dpi = 320
)

cat("Saved figures to:\n")
cat(figure_dir, "\n")
