# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 2c: projected cumulative conceptual-design entries from
# 2025 to 2050 under the Baseline and Accelerated scenarios.
#
# Panel c shows pipeline-expansion assumptions underpinning FOAK deployment.
# The triangle and diamond mark the Baseline and Accelerated pivot years,
# respectively, after which assumed design-entry rates decline. Shaded bands
# indicate simulated uncertainty intervals.
#
# Required input:
# None. Panel c is generated from the design-entry assumptions specified below.
#
# Outputs:
# outputs/Figure_02/intermediate/fig02_panel_c_design_additions.png
# outputs/Figure_02/intermediate/fig02_panel_c_design_additions.pdf
# outputs/Figure_02/intermediate/fig02_panel_c_design_additions.rds
# outputs/Figure_02/intermediate/fig02_panel_c_design_additions_core.rds

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()
set.seed(42)

# ============================================================
# FIGURE 2 — PANEL C ONLY
# Pipeline expansion / cumulative conceptual-design starts
# ============================================================

# ------------------------------------------------------------
# PROJECT PATHS
# ------------------------------------------------------------

find_repo_root <- function(markers = c(".git", file.path("data", "Main_Figures"))) {
  path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  repeat {
    marker_found <- any(file.exists(file.path(path, markers)))
    
    if (marker_found) {
      return(path)
    }
    
    parent <- dirname(path)
    
    if (identical(parent, path)) {
      stop(
        "Could not find repository root containing one of these markers:\n",
        paste(markers, collapse = "\n"),
        "\n\nCurrent working directory was: ",
        getwd()
      )
    }
    
    path <- parent
  }
}

REPO_ROOT <- find_repo_root()

OUT_DIR <- file.path(
  REPO_ROOT,
  "outputs",
  "Figure_02",
  "intermediate"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 88
SAVE_H_MM <- 72
SAVE_DPI  <- 600

FILE_STUB <- "fig02_panel_c_design_additions"

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
PANEL_LABEL_SIZE <- 10

# ------------------------------------------------------------
# PANEL TAG GEOMETRY
# ------------------------------------------------------------

TAG_X <- 0.01
TAG_Y <- 0.99

# ------------------------------------------------------------
# COLOURS
# ------------------------------------------------------------

col_baseline_line <- "grey30"
col_baseline_fill <- "grey70"
col_accel_line    <- "#006400"
col_accel_fill    <- "#90EE90"

# ============================================================
# PANEL C — PIPELINE EXPANSION
# ============================================================

years <- 2026:2050

pivot_year_baseline <- 2035
pivot_year_accel    <- 2030

n_sim <- 10000

lambda0_baseline    <- 2.06
lambda2050_baseline <- 0.824

lambda0_accel       <- 3.33
lambda2050_accel    <- 0.666

years_decline_baseline <- 2050 - pivot_year_baseline
years_decline_accel    <- 2050 - pivot_year_accel

k_baseline <- -log(lambda2050_baseline / lambda0_baseline) / years_decline_baseline
k_accel    <- -log(lambda2050_accel / lambda0_accel) / years_decline_accel

lambda_t <- function(year, lambda0, k, pivot_year) {
  if (year <= pivot_year) {
    lambda0
  } else {
    lambda0 * exp(-k * (year - pivot_year))
  }
}

lambda_path_baseline <- sapply(
  years,
  lambda_t,
  lambda0 = lambda0_baseline,
  k = k_baseline,
  pivot_year = pivot_year_baseline
)

lambda_path_accel <- sapply(
  years,
  lambda_t,
  lambda0 = lambda0_accel,
  k = k_accel,
  pivot_year = pivot_year_accel
)

sim_baseline <- replicate(n_sim, rpois(length(years), lambda_path_baseline))
sim_accel    <- replicate(n_sim, rpois(length(years), lambda_path_accel))

cum_baseline <- t(apply(sim_baseline, 2, cumsum))
cum_accel    <- t(apply(sim_accel, 2, cumsum))

summarise_sim <- function(sim, scenario) {
  data.frame(
    Year = years,
    Mean = apply(sim, 2, mean),
    Q5   = apply(sim, 2, quantile, probs = 0.05),
    Q95  = apply(sim, 2, quantile, probs = 0.95),
    Scenario = scenario
  )
}

plot_df_c <- rbind(
  summarise_sim(cum_accel, "Accelerated"),
  summarise_sim(cum_baseline, "Baseline")
)

plot_df_c$Scenario <- factor(
  plot_df_c$Scenario,
  levels = c("Accelerated", "Baseline")
)

zero_acc <- data.frame(
  Year = 2025,
  Mean = 0,
  Q5 = 0,
  Q95 = 0,
  Scenario = factor("Accelerated", levels = c("Accelerated", "Baseline"))
)

zero_base <- data.frame(
  Year = 2025,
  Mean = 0,
  Q5 = 0,
  Q95 = 0,
  Scenario = factor("Baseline", levels = c("Accelerated", "Baseline"))
)

plot_df_c <- rbind(zero_acc, zero_base, plot_df_c)
plot_df_c <- plot_df_c[order(plot_df_c$Scenario, plot_df_c$Year), ]

pivot_points <- data.frame(
  Year = c(pivot_year_baseline, pivot_year_accel),
  Scenario = factor(
    c("Baseline", "Accelerated"),
    levels = c("Accelerated", "Baseline")
  ),
  shape_id = c("Baseline", "Accelerated")
)

pivot_points$Mean <- c(
  plot_df_c$Mean[
    plot_df_c$Year == pivot_year_baseline &
      plot_df_c$Scenario == "Baseline"
  ][1],
  plot_df_c$Mean[
    plot_df_c$Year == pivot_year_accel &
      plot_df_c$Scenario == "Accelerated"
  ][1]
)

p_c_core <- ggplot(
  plot_df_c,
  aes(x = Year, y = Mean, fill = Scenario, colour = Scenario)
) +
  geom_ribbon(aes(ymin = Q5, ymax = Q95), alpha = 0.20, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(
    data = pivot_points,
    aes(x = Year, y = Mean, shape = shape_id),
    fill = "white",
    colour = "black",
    stroke = 0.45,
    size = 2.1,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_shape_manual(
    values = c("Accelerated" = 23, "Baseline" = 24),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(Accelerated = col_accel_fill, Baseline = col_baseline_fill),
    guide = "none"
  ) +
  scale_colour_manual(
    values = c(Accelerated = col_accel_line, Baseline = col_baseline_line),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(2025, 2050, by = 5),
    labels = paste0(seq(2025, 2050, by = 5), "\n"),
    limits = c(2025, 2050.4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(20, 60, by = 20),
    limits = c(0, 60),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Year",
    y = "Cumulative conceptual-design starts"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = BASE_SIZE) +
  theme(
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black",
      margin = margin(t = 4)
    ),
    axis.title.y = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black"
    ),
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20",
      margin = margin(t = 2)
    ),
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    plot.margin = margin(t = 8, r = 8, b = 6, l = 8),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

p_c <- ggdraw(p_c_core) +
  draw_plot_label(
    "c",
    x = TAG_X,
    y = TAG_Y,
    hjust = 0,
    vjust = 1,
    size = PANEL_LABEL_SIZE,
    fontface = "bold"
  )

print(p_c)

# ============================================================
# SAVE PANEL C
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = p_c,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = p_c,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)

saveRDS(
  p_c,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

saveRDS(
  p_c_core,
  file = file.path(OUT_DIR, paste0(FILE_STUB, "_core.rds"))
)

p_c
