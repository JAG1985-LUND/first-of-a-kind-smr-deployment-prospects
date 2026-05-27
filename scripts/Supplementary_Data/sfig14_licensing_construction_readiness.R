# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 14:
# Licensing- and construction-readiness across development phases
#
# Required input:
# data/Supplementary_Data/sfig_source_data_licensing_construction_readiness.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig14_licensing_construction_readiness.png
# outputs/Supplementary_Data/sfig14_licensing_construction_readiness.pdf

rm(list = ls())
graphics.off()

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(tibble)
})

# ============================================================
# SUPPLEMENTARY FIGURE 14
# a) Licensing-readiness across development phases
# b) Construction-readiness across development phases
# Shared compact legend below both panels
# ============================================================

DATA_DIR <- file.path(
  "data",
  "Supplementary_Data"
)

FILE_IN <- file.path(
  DATA_DIR,
  "sfig_source_data_licensing_construction_readiness.xlsx"
)

SHEET_A <- "licensing_readiness"
SHEET_B <- "construction_readiness"

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Data"
)

OUT_BASE <- "sfig14_readiness_across_phases_combined"

if (!file.exists(FILE_IN)) {
  stop("Input file not found: ", FILE_IN)
}

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

SAVE_W_MM <- 180
SAVE_H_MM <- 185
SAVE_DPI  <- 600
BASE_SIZE <- 8

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

phase_order_a <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing"
)

phase_order_b <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction"
)

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

pick_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

normalize_phase <- function(x) {
  x <- trimws(as.character(x))
  x_low <- tolower(x)
  
  dplyr::case_when(
    x_low %in% c("cd phase", "cd", "conceptual design", "conceptual") ~ "Conceptual design",
    x_low %in% c("bd phase", "bd", "basic design", "basic") ~ "Basic design",
    x_low %in% c("dd phase", "dd", "detailed design", "detailed") ~ "Detailed design",
    x_low %in% c("licensing phase", "licensing", "licensed", "under licensing") ~ "Licensing",
    x_low %in% c("construction phase", "construction", "under construction") ~ "Construction",
    TRUE ~ NA_character_
  )
}

normalize_cooling <- function(x) {
  x <- trimws(as.character(x))
  x_low <- tolower(x)
  
  dplyr::case_when(
    grepl("^water", x_low) ~ "Water-cooled",
    !is.na(x_low) & x_low != "" ~ "Non-water-cooled",
    TRUE ~ NA_character_
  )
}

theme_base <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text   = element_text(size = BASE_SIZE),
    axis.title  = element_text(size = BASE_SIZE),
    legend.position = "none",
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(4, 4, 4, 4)
  )

prepare_sheet <- function(sheet_name, metric_patterns, phase_order) {
  
  raw <- read_excel(FILE_IN, sheet = sheet_name, na = c("N/A", "NA", ""))
  names(raw) <- clean_names_local(names(raw))
  
  phase_col  <- pick_col(names(raw), c("^cohort$", "^phase$"))
  design_col <- pick_col(names(raw), c("^design$", "design"))
  metric_col <- pick_col(names(raw), metric_patterns)
  
  if (any(is.na(c(phase_col, design_col, metric_col)))) {
    stop("Required columns not found in sheet '", sheet_name, "'.")
  }
  
  raw %>%
    transmute(
      phase_raw  = .data[[phase_col]],
      design_raw = .data[[design_col]],
      readiness  = suppressWarnings(as.numeric(.data[[metric_col]]))
    ) %>%
    mutate(
      Phase   = normalize_phase(phase_raw),
      Cooling = normalize_cooling(design_raw)
    ) %>%
    filter(
      !is.na(readiness),
      !is.na(Phase),
      !is.na(Cooling),
      Phase %in% phase_order
    ) %>%
    mutate(
      Phase   = factor(Phase, levels = phase_order),
      Cooling = factor(Cooling, levels = c("Water-cooled", "Non-water-cooled")),
      PhaseNum = as.numeric(Phase)
    )
}

make_panel <- function(dat, y_label, panel_label) {
  
  p_core <- ggplot(dat, aes(x = PhaseNum, y = readiness)) +
    geom_jitter(
      aes(colour = Phase, shape = Cooling),
      width = 0.16,
      height = 0,
      size = 2.6,
      alpha = 0.95
    ) +
    stat_smooth(
      method = "lm",
      se = FALSE,
      linewidth = 0.7,
      colour = "grey35"
    ) +
    scale_x_continuous(
      breaks = seq_along(levels(dat$Phase)),
      labels = levels(dat$Phase),
      limits = c(0.8, length(levels(dat$Phase)) + 0.2),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = 1:6,
      limits = c(1, 6),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    scale_colour_manual(
      values = phase_cols[levels(dat$Phase)],
      breaks = levels(dat$Phase)
    ) +
    scale_shape_manual(
      values = c("Water-cooled" = 16, "Non-water-cooled" = 17),
      breaks = c("Water-cooled", "Non-water-cooled")
    ) +
    labs(
      x = "Development phase",
      y = y_label
    ) +
    theme_base
  
  ggdraw(p_core) +
    draw_plot_label(
      panel_label,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "plain"
    )
}

df_a <- prepare_sheet(
  sheet_name = SHEET_A,
  metric_patterns = c("^licensing_readiness$", "^licensing$", "licensing_readiness"),
  phase_order = phase_order_a
)

df_b <- prepare_sheet(
  sheet_name = SHEET_B,
  metric_patterns = c("^construction_readiness$", "^construction$", "construction_readiness"),
  phase_order = phase_order_b
)

p_a <- make_panel(
  dat = df_a,
  y_label = "Licensing-readiness",
  panel_label = "a"
)

p_b <- make_panel(
  dat = df_b,
  y_label = "Construction-readiness",
  panel_label = "b"
)

legend_phase_plot <- ggplot(
  tibble(
    Phase = factor(phase_order_b, levels = phase_order_b),
    x = seq_along(phase_order_b),
    y = 1
  ),
  aes(x = x, y = y, colour = Phase)
) +
  geom_point(size = 3.0) +
  scale_colour_manual(
    name = "Development phase",
    values = phase_cols,
    breaks = phase_order_b
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left",
      override.aes = list(size = 3.2)
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = BASE_SIZE, face = "plain"),
    legend.text  = element_text(size = BASE_SIZE - 0.2),
    legend.key.width  = unit(4.2, "mm"),
    legend.key.height = unit(4.2, "mm"),
    legend.spacing.x  = unit(0.8, "mm"),
    legend.spacing.y  = unit(0, "mm"),
    legend.box.spacing = unit(0, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 0, 0, 0)
  )

legend_phase <- get_legend(legend_phase_plot)

legend_cooling_plot <- ggplot(
  tibble(
    Cooling = factor(
      c("Water-cooled", "Non-water-cooled"),
      levels = c("Water-cooled", "Non-water-cooled")
    ),
    x = 1:2,
    y = 1
  ),
  aes(x = x, y = y, shape = Cooling)
) +
  geom_point(size = 3.0, colour = "black") +
  scale_shape_manual(
    name = "Cooling type",
    values = c("Water-cooled" = 16, "Non-water-cooled" = 17),
    breaks = c("Water-cooled", "Non-water-cooled")
  ) +
  guides(
    shape = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left",
      override.aes = list(size = 3.2, colour = "black")
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.title = element_text(size = BASE_SIZE, face = "plain"),
    legend.text  = element_text(size = BASE_SIZE - 0.2),
    legend.key.width  = unit(4.2, "mm"),
    legend.key.height = unit(4.2, "mm"),
    legend.spacing.x  = unit(0.8, "mm"),
    legend.spacing.y  = unit(0, "mm"),
    legend.box.spacing = unit(0, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 0, 0, 0)
  )

legend_cooling <- get_legend(legend_cooling_plot)

legend_block <- plot_grid(
  legend_phase,
  legend_cooling,
  ncol = 1,
  rel_heights = c(1, 1)
)

panels_block <- plot_grid(
  p_a,
  p_b,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1, 1)
)

final_plot <- plot_grid(
  panels_block,
  legend_block,
  ncol = 1,
  rel_heights = c(1, 0.14)
)

print(final_plot)

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".png")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(OUT_BASE, ".pdf")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)

final_plot