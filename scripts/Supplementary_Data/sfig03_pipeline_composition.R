# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 3: composition and characteristics of the
# global SMR design pipeline at end-2025.
#
# Required input:
# data/Supplementary_Data/sfig03_source_data_pipeline_composition.xlsx
#
# Outputs:
# outputs/Supplementary_Data/sfig03_pipeline_composition.png
# outputs/Supplementary_Data/sfig03_pipeline_composition.pdf

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(cowplot)
  library(readr)
  library(tibble)
  library(grid)
  library(stringr)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 3
# PIPELINE COMPOSITION AND CHARACTERISTICS
# ============================================================

# ------------------------------------------------------------
# PATHS — GITHUB RUN
# ------------------------------------------------------------

DATA_DIR <- file.path(
  "data",
  "Supplementary_Data"
)

FILE <- file.path(
  DATA_DIR,
  "sfig03_source_data_pipeline_composition.xlsx"
)

OUT_DIR <- file.path(
  "outputs",
  "Supplementary_Data"
)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 205
SAVE_DPI  <- 600

FILE_STUB <- "sfig03_pipeline_composition"

# ------------------------------------------------------------
# STYLE
# ------------------------------------------------------------

BASE_SIZE <- 7

axis_line_col <- "grey35"
axis_line_lw  <- 0.3

base_theme <- theme_classic(base_size = BASE_SIZE) +
  theme(
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    panel.border      = element_blank(),
    axis.line.x       = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.line.y       = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks        = element_line(colour = axis_line_col, linewidth = axis_line_lw),
    axis.ticks.length = unit(0.12, "cm"),
    axis.title        = element_text(size = BASE_SIZE),
    axis.text         = element_text(size = BASE_SIZE),
    legend.title      = element_text(size = BASE_SIZE),
    legend.text       = element_text(size = BASE_SIZE),
    legend.background = element_blank(),
    legend.key        = element_blank(),
    plot.margin       = margin(5.5, 5.5, 5.5, 5.5),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA)
  )

# ------------------------------------------------------------
# PALETTES AND ORDERS
# ------------------------------------------------------------

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3",
  "Failed design"     = "#A9A9A9"
)

phase_cols_active <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
)

cooling_palette <- c(
  "Water-cooled" = "#1f77b4",
  "Metal-cooled" = "#A9A9A9",
  "Salt-cooled"  = "#1b9e77",
  "Gas-cooled"   = "#d62728"
)

size_palette <- c(
  "30–99"   = "#E6D5E8",
  "100–199" = "#C9AED6",
  "200–299" = "#A07CB8",
  "300–500" = "#6F4C9B"
)

phase_order_a <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational",
  "Failed design"
)

phase_order_b <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

cooling_order <- c(
  "Water-cooled",
  "Metal-cooled",
  "Salt-cooled",
  "Gas-cooled"
)

size_order <- c(
  "30–99",
  "100–199",
  "200–299",
  "300–500"
)

phase_labels_a <- c(
  "Conceptual design" = "Conceptual",
  "Basic design"      = "Basic",
  "Detailed design"   = "Detailed",
  "Licensing"         = "Licensing",
  "Construction"      = "Construction",
  "Operational"       = "Operational",
  "Failed design"     = "Failed"
)

phase_labels_b <- c(
  "Conceptual design" = "Conceptual",
  "Basic design"      = "Basic",
  "Detailed design"   = "Detailed",
  "Licensing"         = "Licensing",
  "Construction"      = "Construction",
  "Operational"       = "Operational"
)

cooling_labels_multiline <- c(
  "Water-cooled" = "Water-\ncooled",
  "Metal-cooled" = "Metal-\ncooled",
  "Salt-cooled"  = "Salt-\ncooled",
  "Gas-cooled"   = "Gas-\ncooled"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x
}

wrap_panel <- function(p, tag) {
  ggdraw() +
    draw_plot(p) +
    draw_plot_label(
      tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 12,
      fontface = "plain"
    )
}

# ============================================================
# LOAD AND STANDARDISE DATA
# ============================================================

df_raw <- read_excel(FILE) %>%
  rename_with(clean_names_local)

required_cols <- c(
  "phase",
  "cooling_type"
)

missing_cols <- setdiff(
  required_cols,
  names(df_raw)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    "\nAvailable columns are: ",
    paste(names(df_raw), collapse = ", ")
  )
}

size_col <- intersect(
  c(
    "size",
    "capacity",
    "capacity_mwe",
    "power_output_mwe",
    "output_mwe",
    "mwe"
  ),
  names(df_raw)
)[1]

if (is.na(size_col)) {
  stop(
    "No size/capacity column found. Available columns are: ",
    paste(names(df_raw), collapse = ", ")
  )
}

df <- df_raw %>%
  mutate(
    phase = as.character(phase),
    cooling_type = as.character(cooling_type),
    size_raw = .data[[size_col]],
    phase = case_when(
      str_detect(phase, regex("concept", ignore_case = TRUE)) ~ "Conceptual design",
      str_detect(phase, regex("basic",   ignore_case = TRUE)) ~ "Basic design",
      str_detect(phase, regex("detail",  ignore_case = TRUE)) ~ "Detailed design",
      str_detect(phase, regex("lic",     ignore_case = TRUE)) ~ "Licensing",
      str_detect(phase, regex("constr",  ignore_case = TRUE)) ~ "Construction",
      str_detect(phase, regex("oper",    ignore_case = TRUE)) ~ "Operational",
      str_detect(phase, regex("fail",    ignore_case = TRUE)) ~ "Failed design",
      TRUE ~ NA_character_
    ),
    cooling_type = case_when(
      str_detect(cooling_type, regex("water", ignore_case = TRUE)) ~ "Water-cooled",
      str_detect(cooling_type, regex("metal", ignore_case = TRUE)) ~ "Metal-cooled",
      str_detect(cooling_type, regex("salt",  ignore_case = TRUE)) ~ "Salt-cooled",
      str_detect(cooling_type, regex("gas",   ignore_case = TRUE)) ~ "Gas-cooled",
      TRUE ~ NA_character_
    ),
    size_num = parse_number(as.character(size_raw)),
    size_class = case_when(
      size_num >= 30  & size_num < 100  ~ "30–99",
      size_num >= 100 & size_num < 200  ~ "100–199",
      size_num >= 200 & size_num < 300  ~ "200–299",
      size_num >= 300 & size_num <= 500 ~ "300–500",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# DERIVED DATA
# ============================================================

phase_counts <- df %>%
  filter(!is.na(phase)) %>%
  count(phase, name = "Count") %>%
  mutate(
    phase = factor(
      phase,
      levels = phase_order_a
    )
  ) %>%
  arrange(phase)

phase_experience <- tribble(
  ~phase,               ~Share,
  "Conceptual design",   51.3,
  "Basic design",        30.5,
  "Detailed design",     11.0,
  "Licensing",            2.3,
  "Construction",         3.4,
  "Operational",          1.6
) %>%
  mutate(
    phase = factor(
      phase,
      levels = phase_order_b
    )
  ) %>%
  arrange(phase)

cooling_counts <- df %>%
  filter(
    !is.na(cooling_type),
    phase != "Failed design"
  ) %>%
  count(cooling_type, name = "Count") %>%
  mutate(
    cooling_type = factor(
      cooling_type,
      levels = cooling_order
    )
  ) %>%
  arrange(cooling_type)

size_counts <- df %>%
  filter(
    !is.na(size_class),
    phase != "Failed design"
  ) %>%
  count(size_class, name = "Count") %>%
  mutate(
    size_class = factor(
      size_class,
      levels = size_order
    )
  ) %>%
  arrange(size_class)

cooling_phase_counts <- df %>%
  filter(
    !is.na(cooling_type),
    !is.na(phase),
    phase != "Failed design"
  ) %>%
  count(cooling_type, phase, name = "Count") %>%
  mutate(
    cooling_type = factor(
      cooling_type,
      levels = cooling_order
    ),
    phase = factor(
      phase,
      levels = rev(phase_order_b)
    )
  ) %>%
  arrange(cooling_type, phase)

# ============================================================
# PANEL A
# ============================================================

p_a <- ggplot(phase_counts, aes(x = phase, y = Count, fill = phase)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = phase_cols, drop = FALSE) +
  scale_x_discrete(labels = phase_labels_a, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 30),
    breaks = c(10, 20, 30),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Design status",
    y = "Number of SMR designs"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = BASE_SIZE,
      angle = 28,
      hjust = 1,
      vjust = 1,
      lineheight = 0.9
    ),
    axis.title.x = element_text(margin = margin(t = 6))
  )

# ============================================================
# PANEL B
# ============================================================

p_b <- ggplot(phase_experience, aes(x = phase, y = Share, fill = phase)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = phase_cols_active, drop = FALSE) +
  scale_x_discrete(labels = phase_labels_b, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = c(10, 20, 30, 40, 50, 60),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Development phase",
    y = "Cumulative experience (%)"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = BASE_SIZE,
      angle = 28,
      hjust = 1,
      vjust = 1,
      lineheight = 0.9
    ),
    axis.title.x = element_text(margin = margin(t = 6))
  )

# ============================================================
# PANEL C
# ============================================================

p_c <- ggplot(cooling_counts, aes(x = cooling_type, y = Count, fill = cooling_type)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = cooling_palette, drop = FALSE) +
  scale_x_discrete(labels = cooling_labels_multiline, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = c(5, 10, 15, 20, 25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Reactor cooling type",
    y = "Number of SMR designs"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = BASE_SIZE,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.9
    ),
    axis.title.x = element_text(margin = margin(t = 6))
  )

# ============================================================
# PANEL D
# ============================================================

p_d <- ggplot(size_counts, aes(x = size_class, y = Count, fill = size_class)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = size_palette, drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(5, 10, 15, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Power output range (MWe)",
    y = "Number of SMR designs"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = BASE_SIZE,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    )
  )

# ============================================================
# PANEL E
# ============================================================

p_e <- ggplot(cooling_phase_counts, aes(x = cooling_type, y = Count, fill = phase)) +
  geom_col(width = 0.58) +
  scale_fill_manual(values = phase_cols_active, drop = FALSE) +
  scale_x_discrete(labels = cooling_labels_multiline, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = c(5, 10, 15, 20, 25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Reactor cooling type",
    y = "Number of SMR designs"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = BASE_SIZE,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.9
    )
  )

# ============================================================
# ASSEMBLE FIGURE
# ============================================================

pa <- wrap_panel(p_a, "a")
pb <- wrap_panel(p_b, "b")
pc <- wrap_panel(p_c, "c")
pd <- wrap_panel(p_d, "d")
pe <- wrap_panel(p_e, "e")

top_grid <- plot_grid(
  pa, pb,
  pc, pd,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

bottom_grid <- plot_grid(
  NULL,
  pe,
  NULL,
  ncol = 3,
  rel_widths = c(0.10, 0.80, 0.10)
)

fig <- plot_grid(
  top_grid,
  bottom_grid,
  ncol = 1,
  rel_heights = c(1.60, 0.95)
)

print(fig)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 3
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = cairo_pdf,
  bg = "white"
)