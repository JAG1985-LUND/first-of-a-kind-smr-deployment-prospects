
# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Figure 1: OECD and non-OECD SMR design pipelines and regional
# composition in 2025.
#
# Panel a shows the annual number of SMR designs in the OECD pipeline,
# stacked by development phase.
# Panel b shows the annual number of SMR designs in the non-OECD pipeline,
# stacked by development phase.
# Panel c shows the number of SMR designs across six macro-regions in 2025,
# by development phase.
# Panel d shows the number of SMR designs across six macro-regions in 2025,
# by reactor cooling type.
#
# Working directory:
# Set the working directory to the repository root or scripts folder before running this script.
#
# Required input:
# data/fig01_source_data_historical_pipeline.xlsx
#
# Outputs:
# outputs/Figure_01/fig01_historical_pipeline.png
# outputs/Figure_01/fig01_historical_pipeline.pdf



suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(cowplot)
  library(tibble)
  library(grid)
})

rm(list = ls())
graphics.off()

# ------------------------------------------------------------

library(readxl)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------
FILE <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/fig01_source_data_historical_pipeline.xlsx"
OUT  <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Figure_01"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# LOAD SOURCE DATA
# ------------------------------------------------------------
fig01_source_data_historical_pipeline <- read_excel(FILE)
View(fig01_source_data_historical_pipeline)


# ------------------------------------------------------------
# EXPORT SETTINGS
# ------------------------------------------------------------
SAVE_W_MM <- 180
SAVE_H_MM <- 150
SAVE_DPI  <- 600

# ------------------------------------------------------------
# TIME AXES
# ------------------------------------------------------------
YEAR_END   <- 2025L
x_breaks_a <- c(1985, 1995, 2005, 2015, 2025)
x_breaks_b <- c(1995, 2005, 2015, 2025)

# ------------------------------------------------------------
# AXIS SCALES
# ------------------------------------------------------------
a_y_limits <- c(0, 50)
a_y_breaks <- seq(10, 50, 10)

b_y_limits <- c(0, 50)
b_y_breaks <- seq(10, 50, 10)

# ------------------------------------------------------------
# TYPOGRAPHY
# ------------------------------------------------------------
BASE_SIZE         <- 7
AXIS_TEXT_SIZE    <- 6.5
AXIS_TITLE_SIZE   <- 7
LEGEND_TEXT_SIZE  <- 6.2
LEGEND_TITLE_SIZE <- 7
TITLE_SIZE        <- 7
TAG_SIZE          <- 10

# ------------------------------------------------------------
# SHARED SPACING
# ------------------------------------------------------------
shared_plot_margin <- margin(t = 4, r = 6, b = 4, l = 4)
shared_title_theme <- element_text(
  hjust  = 0.5,
  size   = TITLE_SIZE,
  face   = "bold",
  margin = margin(b = 2)
)

# ------------------------------------------------------------
# PHASE ORDER
# ------------------------------------------------------------
phase_order <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational"
)

phase_cols <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE",
  "Operational"       = "#4393C3"
)

phase_legend_override <- list(
  fill = unname(phase_cols[phase_order])
)

# ------------------------------------------------------------
# COOLING ORDER
# ------------------------------------------------------------
cool_order <- c(
  "Water-cooled",
  "Metal-cooled",
  "Salt-cooled",
  "Gas-cooled"
)

cooling_palette <- c(
  "Water-cooled" = "#1f77b4",
  "Metal-cooled" = "#A9A9A9",
  "Salt-cooled"  = "#1b9e77",
  "Gas-cooled"   = "#d62728"
)

# ------------------------------------------------------------
# FAILED DESIGN EXIT YEARS
# ------------------------------------------------------------
failure_years <- tibble(
  SMR = c("FBNR", "Flexblue", "LEADIR-PS", "MSTW", "PRISM"),
  fail_year = c(2020, 2016, 2015, 2016, 1994)
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
normalize_phase <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    x %in% c("CD", "Conceptual design") ~ "Conceptual design",
    x %in% c("BD", "Basic design") ~ "Basic design",
    x %in% c("DD", "Detailed design") ~ "Detailed design",
    x %in% c("Licensing", "Licensed") ~ "Licensing",
    x %in% c("Construction", "Under construction") ~ "Construction",
    x %in% c("Operational", "Operating") ~ "Operational",
    x %in% c("Failed design", "Failed") ~ "Failed design",
    TRUE ~ x
  )
}

normalize_design <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    x %in% c("Molten salt-cooled", "Molten Salt-cooled", "Salt-cooled") ~ "Salt-cooled",
    x %in% c("Metal-cooled", "MetaL-cooled", "Metal cooled") ~ "Metal-cooled",
    x %in% c("Water-cooled", "Water cooled") ~ "Water-cooled",
    x %in% c("Gas-cooled", "Gas cooled") ~ "Gas-cooled",
    TRUE ~ x
  )
}

normalize_oecd <- function(x) {
  x <- trimws(tolower(as.character(x)))
  dplyr::case_when(
    x %in% c("1", "yes", "y", "true", "t", "oecd", "member", "oecd member") ~ "OECD",
    x %in% c("0", "no", "n", "false", "f", "non-oecd", "non oecd", "nonmember", "non-member") ~ "Non-OECD",
    TRUE ~ NA_character_
  )
}

infer_oecd_from_region <- function(region) {
  region <- trimws(as.character(region))
  dplyr::case_when(
    region %in% c("North America", "Europe", "OECD-Asia") ~ "OECD",
    region %in% c("Russia", "China", "RoW") ~ "Non-OECD",
    TRUE ~ NA_character_
  )
}

year_seq <- function(start_year, end_year_exclusive, year_end = YEAR_END) {
  if (is.na(start_year) || is.na(end_year_exclusive)) return(integer(0))
  s <- as.integer(start_year)
  e <- min(as.integer(end_year_exclusive) - 1L, year_end)
  if (e < s) return(integer(0))
  seq.int(s, e)
}

tag_panel <- function(p, tag, x = 0.01, y = 0.99) {
  ggdraw() +
    draw_plot(p) +
    draw_plot_label(
      tag,
      x = x, y = y,
      hjust = 0, vjust = 1,
      size = TAG_SIZE,
      fontface = "bold"
    )
}

base_theme <- function() {
  theme_classic(base_size = BASE_SIZE) +
    theme(
      axis.text.x = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
      axis.text.y = element_text(size = AXIS_TEXT_SIZE, colour = "grey20"),
      axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black"),
      axis.title.y = element_text(size = AXIS_TITLE_SIZE, colour = "black"),
      axis.line.x  = element_line(colour = "grey35", linewidth = 0.3),
      axis.line.y  = element_line(colour = "grey35", linewidth = 0.3),
      axis.ticks   = element_line(colour = "grey35", linewidth = 0.3),
      legend.text  = element_text(size = LEGEND_TEXT_SIZE, colour = "black"),
      legend.title = element_text(size = LEGEND_TITLE_SIZE, colour = "black"),
      legend.key.height = unit(0.32, "cm"),
      legend.key.width  = unit(0.32, "cm"),
      legend.spacing.y  = unit(0.02, "cm"),
      legend.margin     = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.background = element_rect(fill = "white", colour = NA),
      plot.title = shared_title_theme,
      plot.margin = shared_plot_margin,
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

# ============================================================
# LOAD DATA
# ============================================================
df <- read_excel(FILE)
names(df) <- trimws(names(df))

req_cols <- c(
  "SMR", "Phase", "Design", "Region",
  "CD_Start", "BD_Start", "DD_Start", "Lic_Start", "Con_Start", "Op_Start"
)

miss_cols <- setdiff(req_cols, names(df))
if (length(miss_cols) > 0) {
  stop("Missing required columns: ", paste(miss_cols, collapse = ", "))
}

start_cols <- c("CD_Start", "BD_Start", "DD_Start", "Lic_Start", "Con_Start", "Op_Start")

phase_map <- c(
  CD_Start  = "Conceptual design",
  BD_Start  = "Basic design",
  DD_Start  = "Detailed design",
  Lic_Start = "Licensing",
  Con_Start = "Construction",
  Op_Start  = "Operational"
)

oecd_col <- names(df)[grepl("oecd", names(df), ignore.case = TRUE)]
oecd_col <- if (length(oecd_col) > 0) oecd_col[1] else NA_character_

df <- df %>%
  mutate(
    Phase  = normalize_phase(Phase),
    Design = normalize_design(Design),
    Region = trimws(as.character(Region)),
    SMR    = trimws(as.character(SMR)),
    row_id = row_number(),
    across(all_of(start_cols), ~ suppressWarnings(as.numeric(.)))
  )

if (!is.na(oecd_col)) {
  df <- df %>%
    mutate(
      OECD_Group = normalize_oecd(.data[[oecd_col]]),
      OECD_Group = coalesce(OECD_Group, infer_oecd_from_region(Region))
    )
} else {
  df <- df %>%
    mutate(
      OECD_Group = infer_oecd_from_region(Region)
    )
}

df_active <- df %>%
  filter(Phase %in% phase_order)

# ============================================================
# REGION ORDER
# ============================================================
region_order <- df_active %>%
  filter(!is.na(Region), Region != "") %>%
  count(Region, name = "n") %>%
  arrange(desc(n)) %>%
  pull(Region)

region_order <- region_order[region_order != "RoW"]

if ("China" %in% region_order) {
  region_order <- c(region_order[region_order != "China"], "China")
}
if ("RoW" %in% df_active$Region) {
  region_order <- c(region_order, "RoW")
}

# ============================================================
# PIPELINE STINTS
# ============================================================
phase_stints <- df %>%
  select(row_id, SMR, Region, OECD_Group, Phase_status = Phase, all_of(start_cols)) %>%
  pivot_longer(
    cols = all_of(start_cols),
    names_to = "Phase_col",
    values_to = "Start_year"
  ) %>%
  mutate(Phase = unname(phase_map[Phase_col])) %>%
  filter(!is.na(Start_year)) %>%
  group_by(row_id, SMR, Region, OECD_Group, Phase_status) %>%
  arrange(match(Phase, phase_order), .by_group = TRUE) %>%
  mutate(
    Next_start = lead(Start_year),
    Last_start = max(Start_year, na.rm = TRUE),
    Fail_year_override = failure_years$fail_year[match(SMR, failure_years$SMR)],
    Fail_end = case_when(
      !is.na(Fail_year_override) ~ pmin(Fail_year_override + 1L, YEAR_END + 1L),
      TRUE ~ pmin(Last_start + 2L, YEAR_END + 1L)
    ),
    End_year = case_when(
      !is.na(Next_start) ~ Next_start,
      Phase_status == "Failed design" ~ Fail_end,
      TRUE ~ YEAR_END + 1L
    )
  ) %>%
  ungroup()

phase_years <- phase_stints %>%
  filter(!is.na(OECD_Group), OECD_Group %in% c("OECD", "Non-OECD")) %>%
  mutate(Year = map2(Start_year, End_year, year_seq)) %>%
  unnest(cols = Year)

phase_counts_oecd <- phase_years %>%
  count(OECD_Group, Year, Phase, name = "n_designs") %>%
  mutate(
    OECD_Group = factor(OECD_Group, levels = c("OECD", "Non-OECD")),
    Phase = factor(Phase, levels = phase_order)
  )

# ============================================================
# PANEL A — OECD PIPELINE (legend includes Operational with blue key)
# ============================================================
counts_oecd_panel <- phase_counts_oecd %>%
  filter(OECD_Group == "OECD") %>%
  mutate(Phase = factor(as.character(Phase), levels = phase_order))

p_a <- ggplot(counts_oecd_panel, aes(Year, n_designs, fill = Phase)) +
  geom_col(width = 0.8, position = position_stack(reverse = TRUE), show.legend = TRUE) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    limits = phase_order,
    drop = FALSE,
    name = "Phase"
  ) +
  guides(fill = guide_legend(override.aes = phase_legend_override)) +
  scale_x_continuous(
    limits = c(1983.45, YEAR_END + 0.6),
    breaks = x_breaks_a,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = a_y_limits,
    breaks = a_y_breaks,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Year",
    y = "Number of SMR designs",
    title = "OECD SMR pipeline"
  ) +
  base_theme() +
  theme(
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(t = 4)),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1)
  )

# ============================================================
# PANEL B — NON-OECD PIPELINE (no legend)
# ============================================================
counts_nonoecd_panel <- phase_counts_oecd %>%
  filter(OECD_Group == "Non-OECD") %>%
  mutate(Phase = factor(as.character(Phase), levels = phase_order))

p_b <- ggplot(counts_nonoecd_panel, aes(Year, n_designs, fill = Phase)) +
  geom_col(width = 0.8, position = position_stack(reverse = TRUE), show.legend = FALSE) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    limits = phase_order,
    drop = FALSE,
    name = "Phase"
  ) +
  scale_x_continuous(
    limits = c(1992.45, YEAR_END + 0.6),
    breaks = x_breaks_b,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = b_y_limits,
    breaks = b_y_breaks,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Year",
    y = "Number of SMR designs",
    title = "Non-OECD SMR pipeline"
  ) +
  base_theme() +
  theme(
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(t = 4)),
    legend.position = "none"
  )

# ============================================================
# PANEL C — REGION BY PHASE (no legend)
# ============================================================
counts_region <- df_active %>%
  filter(!is.na(Region), Region != "") %>%
  count(Region, Phase, name = "n_designs") %>%
  mutate(
    Region = factor(Region, levels = region_order),
    Phase  = factor(Phase, levels = phase_order)
  )

p_c <- ggplot(counts_region, aes(Region, n_designs, fill = Phase)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE), show.legend = FALSE) +
  scale_fill_manual(
    values = phase_cols,
    breaks = phase_order,
    limits = phase_order,
    drop = FALSE,
    name = "Phase"
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(5, 10, 15, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Region",
    y = "Number of SMR designs",
    title = " "
  ) +
  base_theme() +
  theme(
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, angle = 30, hjust = 1, colour = "grey20"),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(t = 4)),
    legend.position = "none"
  )

# ============================================================
# PANEL D — REGION BY COOLING TYPE (own legend)
# ============================================================
counts_region_type <- df_active %>%
  filter(!is.na(Region), Region != "", !is.na(Design), Design != "", Design %in% cool_order) %>%
  count(Region, Design, name = "n_designs") %>%
  mutate(
    Region = factor(Region, levels = region_order),
    Design = factor(Design, levels = cool_order)
  )

p_d <- ggplot(counts_region_type, aes(Region, n_designs, fill = Design)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(
    values = cooling_palette,
    breaks = cool_order,
    name = "Cooling type"
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    breaks = c(5, 10, 15, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Region",
    y = "Number of SMR designs",
    title = " "
  ) +
  base_theme() +
  theme(
    axis.text.x = element_text(size = AXIS_TEXT_SIZE, angle = 30, hjust = 1, colour = "grey20"),
    axis.title.x = element_text(size = AXIS_TITLE_SIZE, colour = "black", margin = margin(t = 4)),
    legend.position = c(1, 1.00),
    legend.justification = c(1, 1)
  )

# ============================================================
# ALIGN + TAG
# ============================================================
aligned <- align_plots(
  p_a, p_b, p_c, p_d,
  align = "hv",
  axis  = "tblr"
)

pa <- tag_panel(aligned[[1]], "a")
pb <- tag_panel(aligned[[2]], "b")
pc <- tag_panel(aligned[[3]], "c")
pd <- tag_panel(aligned[[4]], "d")

# ============================================================
# LAYOUT: A-B top row, C-D bottom row
# ============================================================
top_row <- plot_grid(
  pa, pb,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

bottom_row <- plot_grid(
  pc, pd,
  ncol = 2,
  rel_widths = c(1, 1),
  align = "hv",
  axis = "tblr"
)

final_fig <- plot_grid(
  top_row, bottom_row,
  ncol = 1,
  rel_heights = c(1.08, 1),
  align = "v",
  axis = "tblr"
)

print(final_fig)

ggsave(
  file.path(OUT, "fig01_historical_pipeline.png"),
  final_fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  file.path(OUT, "fig01_historical_pipeline.pdf"),
  final_fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white"
)


