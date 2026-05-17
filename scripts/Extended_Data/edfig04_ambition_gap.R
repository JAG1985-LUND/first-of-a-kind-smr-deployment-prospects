suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 4 — VENDOR AMBITION GAP
# a) Vendor FOAK target vs modelled FOAK by phase
# b) Vendor FOAK target vs modelled FOAK by sample
# FINAL CLEAN JAG VERSION
# - keeps original plotting structure
# - reads edfig04 source workbook
# - saves PNG + PDF only
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE_IN <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Extended Data/Extended_Data_Files/edfig04_source_data_ambition_gap.xlsx"

OUT_DIR <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Extended Data/Extended_Data_Fig_04"

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 112
SAVE_DPI  <- 600

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------

BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 2.10
LEGEND_TITLE_SIZE <- 2.35

# ------------------------------------------------------------
# X SCALE
# ------------------------------------------------------------

X_LIMITS <- c(2025, 2057.2)
X_BREAKS <- c(2025, 2035, 2045, 2055)

# ------------------------------------------------------------
# LOAD
# ------------------------------------------------------------

panel_a_raw <- read_excel(
  FILE_IN,
  sheet = "panel_a",
  .name_repair = "minimal"
)

panel_b_raw <- read_excel(
  FILE_IN,
  sheet = "panel_b",
  .name_repair = "minimal"
)

names(panel_a_raw)[1:12] <- c(
  "sample",
  "sample_size",
  "trl",
  "vendor_p10", "vendor_p50", "vendor_p90",
  "bas_gap_p10", "bas_gap_p50", "bas_gap_p90",
  "acc_gap_p10", "acc_gap_p50", "acc_gap_p90"
)

names(panel_b_raw)[1:12] <- c(
  "sample",
  "sample_size",
  "trl",
  "vendor_p10", "vendor_p50", "vendor_p90",
  "bas_gap_p10", "bas_gap_p50", "bas_gap_p90",
  "acc_gap_p10", "acc_gap_p50", "acc_gap_p90"
)

vendors_gap_master <- bind_rows(
  panel_a_raw,
  panel_b_raw
)

# ------------------------------------------------------------
# THEME
# ------------------------------------------------------------

base_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, margin = margin(t = 4)),
    axis.title.y = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(4, 6, 4, 6)
  )

tag_panel <- function(p, tag, x = 0.01, y = 0.99) {
  ggdraw() +
    draw_plot(p) +
    draw_plot_label(
      label = tag,
      x = x, y = y,
      hjust = 0, vjust = 1,
      size = 11,
      fontface = "bold"
    )
}

# ============================================================
# PANEL A — PHASE LEVEL
# ============================================================

plot_df_a <- vendors_gap_master %>%
  filter(sample %in% c("construction", "licensing", "detailed_design", "basic_design", "conceptual_design")) %>%
  transmute(
    sample,
    trl,
    vendor_p10 = as.numeric(vendor_p10),
    vendor_p50 = as.numeric(vendor_p50),
    vendor_p90 = as.numeric(vendor_p90),
    bas_p10    = vendor_p10 + as.numeric(bas_gap_p10),
    bas_p50    = vendor_p50 + as.numeric(bas_gap_p50),
    bas_p90    = vendor_p90 + as.numeric(bas_gap_p90),
    acc_p10    = vendor_p10 + as.numeric(acc_gap_p10),
    acc_p50    = vendor_p50 + as.numeric(acc_gap_p50),
    acc_p90    = vendor_p90 + as.numeric(acc_gap_p90)
  ) %>%
  mutate(
    sample_label = recode(
      sample,
      "construction"      = "Construction",
      "licensing"         = "Licensing",
      "detailed_design"   = "Detailed design",
      "basic_design"      = "Basic design",
      "conceptual_design" = "Conceptual design"
    ),
    y_mid = c(
      "Construction"      = 1,
      "Licensing"         = 2,
      "Detailed design"   = 3,
      "Basic design"      = 4,
      "Conceptual design" = 5
    )[sample_label] |> unname(),
    y_bas = y_mid + 0.25,
    y_acc = y_mid - 0.25,
    fill_group = sample_label
  ) %>%
  arrange(y_mid)

fill_cols_a <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

p_a_core <- ggplot(plot_df_a) +
  geom_segment(
    aes(x = vendor_p10, xend = vendor_p90, y = y_mid, yend = y_mid),
    linewidth = 0.7,
    colour = "black",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = bas_p10, xend = bas_p90, y = y_bas, yend = y_bas),
    linewidth = 0.8,
    colour = "grey45",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = acc_p10, xend = acc_p90, y = y_acc, yend = y_acc),
    linewidth = 0.8,
    colour = "#228B22",
    lineend = "butt"
  ) +
  geom_point(
    aes(x = vendor_p50, y = y_mid),
    shape = 4,
    size = 2.8,
    stroke = 0.9,
    colour = "black"
  ) +
  geom_point(
    aes(x = bas_p50, y = y_bas, fill = fill_group),
    shape = 22,
    size = 3.0,
    stroke = 0.9,
    colour = "grey45"
  ) +
  geom_point(
    aes(x = acc_p50, y = y_acc, fill = fill_group),
    shape = 23,
    size = 3.0,
    stroke = 0.9,
    colour = "#228B22"
  ) +
  scale_fill_manual(
    values = fill_cols_a,
    guide = "none"
  ) +
  scale_x_continuous(
    limits = X_LIMITS,
    breaks = X_BREAKS,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = 1:5,
    labels = c(
      "Construction",
      "Licensing",
      "Detailed design",
      "Basic design",
      "Conceptual design"
    ),
    limits = c(0.5, 5.35),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    x = "Year",
    y = NULL
  ) +
  base_theme

# ============================================================
# PANEL B — SAMPLE LEVEL
# ============================================================

plot_df_b <- vendors_gap_master %>%
  filter(sample %in% c("global", "oecd", "non_oecd", "water_cooled", "AMRs")) %>%
  transmute(
    sample,
    vendor_p10 = as.numeric(vendor_p10),
    vendor_p50 = as.numeric(vendor_p50),
    vendor_p90 = as.numeric(vendor_p90),
    bas_p10    = vendor_p10 + as.numeric(bas_gap_p10),
    bas_p50    = vendor_p50 + as.numeric(bas_gap_p50),
    bas_p90    = vendor_p90 + as.numeric(bas_gap_p90),
    acc_p10    = vendor_p10 + as.numeric(acc_gap_p10),
    acc_p50    = vendor_p50 + as.numeric(acc_gap_p50),
    acc_p90    = vendor_p90 + as.numeric(acc_gap_p90)
  ) %>%
  mutate(
    sample_label = recode(
      sample,
      "global"       = "Global",
      "oecd"         = "OECD designs",
      "non_oecd"     = "Non-OECD designs",
      "water_cooled" = "Water-cooled designs",
      "AMRs"         = "Advanced modular reactors"
    ),
    y_mid = c(
      "Global"                    = 5,
      "OECD designs"              = 4,
      "Non-OECD designs"          = 3,
      "Water-cooled designs"      = 2,
      "Advanced modular reactors" = 1
    )[sample_label] |> unname(),
    y_bas = y_mid + 0.22,
    y_acc = y_mid - 0.22
  ) %>%
  arrange(desc(y_mid))

p_b_core <- ggplot(plot_df_b) +
  geom_segment(
    aes(x = vendor_p10, xend = vendor_p90, y = y_mid, yend = y_mid),
    linewidth = 0.7,
    colour = "black",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = bas_p10, xend = bas_p90, y = y_bas, yend = y_bas),
    linewidth = 0.8,
    colour = "grey45",
    lineend = "butt"
  ) +
  geom_segment(
    aes(x = acc_p10, xend = acc_p90, y = y_acc, yend = y_acc),
    linewidth = 0.8,
    colour = "#228B22",
    lineend = "butt"
  ) +
  geom_point(
    aes(x = vendor_p50, y = y_mid),
    shape = 4,
    size = 2.8,
    stroke = 0.9,
    colour = "black"
  ) +
  geom_point(
    aes(x = bas_p50, y = y_bas),
    shape = 22,
    size = 3.0,
    stroke = 0.9,
    fill = "white",
    colour = "grey45"
  ) +
  geom_point(
    aes(x = acc_p50, y = y_acc),
    shape = 23,
    size = 3.0,
    stroke = 0.9,
    fill = "white",
    colour = "#228B22"
  ) +
  scale_x_continuous(
    limits = X_LIMITS,
    breaks = X_BREAKS,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = 5:1,
    labels = c(
      "Global",
      "OECD designs",
      "Non-OECD designs",
      "Water-cooled designs",
      "Advanced modular reactors"
    ),
    limits = c(0.6, 5.35),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Year",
    y = NULL
  ) +
  base_theme +
  theme(
    plot.margin = margin(4, 12, 4, 6)
  )

# ------------------------------------------------------------
# ALIGN PANELS
# ------------------------------------------------------------

aligned_panels <- align_plots(
  p_a_core,
  p_b_core,
  align = "hv",
  axis = "tblr"
)

p_a <- tag_panel(aligned_panels[[1]], "a", x = 0.01, y = 0.99)
p_b <- tag_panel(aligned_panels[[2]], "b", x = 0.04, y = 0.99)

# ============================================================
# SHARED LEGEND
# ============================================================

y_leg <- 0.65

legend_plot <- ggplot() +
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    clip = "off"
  ) +
  annotate(
    "text",
    x = 0.13, y = y_leg,
    label = "Vendor target",
    hjust = 0, vjust = 0.5,
    size = LEGEND_TITLE_SIZE
  ) +
  annotate(
    "segment",
    x = 0.24, xend = 0.30,
    y = y_leg, yend = y_leg,
    linewidth = 0.7,
    colour = "black"
  ) +
  annotate(
    "point",
    x = 0.27, y = y_leg,
    shape = 4,
    size = 2.8,
    stroke = 0.9,
    colour = "black"
  ) +
  annotate(
    "text",
    x = 0.32, y = y_leg,
    label = "Median vendor FOAK target\n(P10–P90 interval)",
    hjust = 0, vjust = 0.5,
    lineheight = 0.9,
    size = LEGEND_TEXT_SIZE
  ) +
  annotate(
    "text",
    x = 0.50, y = y_leg,
    label = "Scenario",
    hjust = 0, vjust = 0.5,
    size = LEGEND_TITLE_SIZE
  ) +
  annotate(
    "segment",
    x = 0.57, xend = 0.63,
    y = y_leg, yend = y_leg,
    linewidth = 0.8,
    colour = "grey45"
  ) +
  annotate(
    "point",
    x = 0.60, y = y_leg,
    shape = 22,
    size = 3.0,
    stroke = 0.9,
    fill = "white",
    colour = "grey45"
  ) +
  annotate(
    "text",
    x = 0.65, y = y_leg,
    label = "Baseline median\n(P10–P90 interval)",
    hjust = 0, vjust = 0.5,
    lineheight = 0.9,
    size = LEGEND_TEXT_SIZE
  ) +
  annotate(
    "segment",
    x = 0.77, xend = 0.83,
    y = y_leg, yend = y_leg,
    linewidth = 0.8,
    colour = "#228B22"
  ) +
  annotate(
    "point",
    x = 0.80, y = y_leg,
    shape = 23,
    size = 3.0,
    stroke = 0.9,
    fill = "white",
    colour = "#228B22"
  ) +
  annotate(
    "text",
    x = 0.85, y = y_leg,
    label = "Accelerated median\n(P10–P90 interval)",
    hjust = 0, vjust = 0.5,
    lineheight = 0.9,
    size = LEGEND_TEXT_SIZE
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    plot.margin = margin(0, 2, 0, 2),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# ============================================================
# COMBINE
# ============================================================

top_row <- plot_grid(
  p_a,
  p_b,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "h",
  axis = "tb"
)

combined_plot <- plot_grid(
  top_row,
  legend_plot,
  ncol = 1,
  rel_heights = c(1, 0.10)
)

print(combined_plot)

# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

ggsave(
  filename = file.path(OUT_DIR, "edfig04_ambition_gap.pdf"),
  plot = combined_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, "edfig04_ambition_gap.png"),
  plot = combined_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

combined_plot