# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 9: AMR-specific late-stage risk sensitivity
# for the active design cohort.
#
# Required input:
# sfig09_amr_accelerated_counterfactual.xlsx
#
# Outputs:
# sfig09_amr_accelerated_counterfactual.png
# sfig09_amr_accelerated_counterfactual.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 9
# AMR-SPECIFIC LATE-STAGE RISK SENSITIVITY
# a) Accelerated scenario, active AMR cohort
# b) AMR-specific late-stage risk sensitivity
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE <- "sfig09_amr_accelerated_counterfactual.xlsx"

OUT_DIR <- "."

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_DPI <- 600
SAVE_W_MM <- 180
SAVE_H_MM <- 72

FILE_STUB <- "sfig09_amr_accelerated_counterfactual"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 9

line_cols <- c(
  "OECD"     = "#1b9e77",
  "Non-OECD" = "#A67C00"
)

fill_cols <- c(
  "OECD"     = "#1b9e77",
  "Non-OECD" = "#A67C00"
)

hide_zero <- function(x) {
  ifelse(x == 0, "", as.character(x))
}

base_theme <- theme_minimal(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks = element_line(colour = "grey35", linewidth = 0.3),
    axis.text = element_text(size = BASE_SIZE),
    axis.title = element_text(size = BASE_SIZE),
    plot.title = element_text(size = BASE_SIZE, face = "plain", hjust = 0.5),
    plot.margin = margin(6, 8, 6, 8),
    legend.position = "bottom",
    legend.title = element_text(size = BASE_SIZE),
    legend.text = element_text(size = BASE_SIZE),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(4, "mm"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

df <- read_excel(FILE, sheet = 1) %>%
  transmute(
    year = as.numeric(year),
    group = case_when(
      str_to_lower(as.character(group)) == "global" ~ "Global",
      str_to_lower(as.character(group)) == "oecd" ~ "OECD",
      str_to_lower(as.character(group)) %in% c("non_oecd", "non-oecd", "non oecd") ~ "Non-OECD",
      TRUE ~ as.character(group)
    ),
    counterfactual = case_when(
      is.logical(counterfactual) ~ counterfactual,
      str_to_lower(as.character(counterfactual)) %in% c("true", "t", "1", "yes", "y") ~ TRUE,
      str_to_lower(as.character(counterfactual)) %in% c("false", "f", "0", "no", "n") ~ FALSE,
      TRUE ~ NA
    ),
    P10 = as.numeric(P10),
    P50 = as.numeric(P50),
    P90 = as.numeric(P90)
  ) %>%
  filter(
    group %in% c("OECD", "Non-OECD"),
    !is.na(counterfactual),
    year <= 2045
  ) %>%
  arrange(
    counterfactual,
    group,
    year
  )

# ============================================================
# BUILD PANELS
# ============================================================

p_a_core <- ggplot(
  df %>% filter(counterfactual == FALSE),
  aes(x = year)
) +
  geom_ribbon(
    aes(ymin = P10, ymax = P90, fill = group),
    alpha = 0.18,
    colour = NA
  ) +
  geom_step(
    aes(y = P50, colour = group),
    linewidth = 0.85,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = NULL,
    values = line_cols,
    breaks = c("OECD", "Non-OECD")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_cols,
    breaks = c("OECD", "Non-OECD"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(2025, 2045.6),
    breaks = c(2025, 2030, 2035, 2040, 2045),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(0, 5, 10, 15, 20),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  base_theme

p_b_core <- ggplot(
  df %>% filter(counterfactual == TRUE),
  aes(x = year)
) +
  geom_ribbon(
    aes(ymin = P10, ymax = P90, fill = group),
    alpha = 0.18,
    colour = NA
  ) +
  geom_step(
    aes(y = P50, colour = group),
    linewidth = 0.85,
    direction = "hv"
  ) +
  scale_colour_manual(
    name = NULL,
    values = line_cols,
    breaks = c("OECD", "Non-OECD")
  ) +
  scale_fill_manual(
    name = NULL,
    values = fill_cols,
    breaks = c("OECD", "Non-OECD"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(2025, 2045.6),
    breaks = c(2025, 2030, 2035, 2040, 2045),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(0, 5, 10, 15, 20),
    labels = hide_zero,
    expand = c(0, 0)
  ) +
  labs(
    x = "Year",
    y = "Cumulative FOAK SMRs"
  ) +
  base_theme

# ============================================================
# SHARED LEGEND
# ============================================================

shared_legend <- cowplot::get_legend(
  p_a_core +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

p_a <- ggdraw(p_a_core + theme(legend.position = "none")) +
  draw_plot_label(
    "a",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 12,
    fontface = "plain"
  )

p_b <- ggdraw(p_b_core + theme(legend.position = "none")) +
  draw_plot_label(
    "b",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 12,
    fontface = "plain"
  )

# ============================================================
# COMBINE PANELS
# ============================================================

panels_only <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

fig_combined <- plot_grid(
  panels_only,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

print(fig_combined)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 9
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = fig_combined,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

fig_combined
