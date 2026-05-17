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
# Required input:
# data/Main_Figures/fig01_source_data_historical_pipeline.xlsx
#
# Outputs:
# fig01_historical_pipeline.png
# fig01_historical_pipeline.pdf

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
# PATHS
# ------------------------------------------------------------

DATA_DIR <- file.path("data", "Main_Figures")

FILE <- file.path(
  DATA_DIR,
  "fig01_source_data_historical_pipeline.xlsx"
)

OUT <- file.path(
  "outputs",
  "Figure_01"
)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

dir.create(
  OUT,
  showWarnings = FALSE,
  recursive = TRUE
)

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
# PHASE ORDER AND COLOURS
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
# COOLING ORDER AND COLOURS
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

failure_years <- tibble::tibble(
  SMR = c("FBNR", "Flexblue", "LEADIR-PS", "MSTW", "PRISM"),
  fail_year = c(2020, 2016, 2015, 2016, 1994)
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_simple <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_|_$", "", x)
  x
}

normalize_phase <- function(x) {
  key <- trimws(tolower(as.character(x)))
  key <- gsub("[ -]+", "_", key)
  
  dplyr::case_when(
    key %in% c("cd", "conceptual", "conceptual_design") ~ "Conceptual design",
    key %in% c("bd", "basic", "basic_design") ~ "Basic design",
    key %in% c("dd", "detailed", "detailed_design") ~ "Detailed design",
    key %in% c("licensing", "licensed") ~ "Licensing",
    key %in% c("construction", "under_construction", "con") ~ "Construction",
    key %in% c("operational", "operating", "op") ~ "Operational",
    key %in% c("failed", "failed_design") ~ "Failed design",
    TRUE ~ as.character(x)
  )
}

normalize_design <- function(x) {
  key <- trimws(tolower(as.character(x)))
  key <- gsub("[ -]+", "_", key)
  
  dplyr::case_when(
    grepl("water", key) ~ "Water-cooled",
    grepl("metal|sodium|lead|lead_bismuth|heat_pipe", key) ~ "Metal-cooled",
    grepl("salt|molten", key) ~ "Salt-cooled",
    grepl("gas|helium|htr|htgr", key) ~ "Gas-cooled",
    TRUE ~ as.character(x)
  )
}

normalize_region <- function(x) {
  key <- trimws(tolower(as.character(x)))
  key <- gsub("_", " ", key)
  
  dplyr::case_when(
    key %in% c("north america", "n america", "n.america") ~ "North America",
    key == "europe" ~ "Europe",
    key %in% c("oecd asia", "oecd-asia") ~ "OECD-Asia",
    key == "russia" ~ "Russia",
    key == "china" ~ "China",
    key %in% c("row", "rest of world", "rest of the world") ~ "RoW",
    TRUE ~ as.character(x)
  )
}

normalize_oecd <- function(x) {
  key <- trimws(tolower(as.character(x)))
  
  dplyr::case_when(
    key %in% c("1", "yes", "y", "true", "t", "oecd", "member", "oecd member") ~ "OECD",
    key %in% c("0", "no", "n", "false", "f", "non-oecd", "non oecd", "nonmember", "non-member") ~ "Non-OECD",
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
      x = x,
      y = y,
      hjust = 0,
      vjust = 1,
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
# LOAD AND STANDARDISE DATA
# ============================================================

df_raw <- readxl::read_excel(
  FILE,
  na = c("", "NA", "N/A")
)

names(df_raw) <- clean_names_simple(names(df_raw))

req_cols <- c(
  "smr_design",
  "phase",
  "region",
  "oecd_member",
  "cooling_type",
  "cd_start",
  "bd_start",
  "dd_start",
  "lic_start",
  "con_start",
  "op_start"
)

miss_cols <- setdiff(req_cols, names(df_raw))

if (length(miss_cols) > 0) {
  stop("Missing required columns: ", paste(miss_cols, collapse = ", "))
}

df <- df_raw %>%
  dplyr::transmute(
    SMR = trimws(as.character(smr_design)),
    Phase = normalize_phase(phase),
    Design = normalize_design(cooling_type),
    Region = normalize_region(region),
    OECD_Group = normalize_oecd(oecd_member),
    CD_Start = suppressWarnings(as.numeric(cd_start)),
    BD_Start = suppressWarnings(as.numeric(bd_start)),
    DD_Start = suppressWarnings(as.numeric(dd_start)),
    Lic_Start = suppressWarnings(as.numeric(lic_start)),
    Con_Start = suppressWarnings(as.numeric(con_start)),
    Op_Start = suppressWarnings(as.numeric(op_start)),
    row_id = dplyr::row_number()
  ) %>%
  dplyr::mutate(
    OECD_Group = dplyr::coalesce(OECD_Group, infer_oecd_from_region(Region))
  )

start_cols <- c(
  "CD_Start",
  "BD_Start",
  "DD_Start",
  "Lic_Start",
  "Con_Start",
  "Op_Start"
)

phase_map <- c(
  CD_Start  = "Conceptual design",
  BD_Start  = "Basic design",
  DD_Start  = "Detailed design",
  Lic_Start = "Licensing",
  Con_Start = "Construction",
  Op_Start  = "Operational"
)

df_active <- df %>%
  dplyr::filter(Phase %in% phase_order)

# ============================================================
# REGION ORDER
# ============================================================

region_order <- c(
  "North America",
  "Europe",
  "OECD-Asia",
  "Russia",
  "China",
  "RoW"
)

# ============================================================
# PIPELINE STINTS
# ============================================================

phase_stints <- df %>%
  dplyr::select(
    row_id,
    SMR,
    Region,
    OECD_Group,
    Phase_status = Phase,
    dplyr::all_of(start_cols)
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(start_cols),
    names_to = "Phase_col",
    values_to = "Start_year"
  ) %>%
  dplyr::mutate(
    Phase = unname(phase_map[Phase_col])
  ) %>%
  dplyr::filter(!is.na(Start_year)) %>%
  dplyr::group_by(row_id, SMR, Region, OECD_Group, Phase_status) %>%
  dplyr::arrange(match(Phase, phase_order), .by_group = TRUE) %>%
  dplyr::mutate(
    Next_start = dplyr::lead(Start_year),
    Last_start = max(Start_year, na.rm = TRUE),
    Fail_year_override = failure_years$fail_year[match(SMR, failure_years$SMR)],
    Fail_end = dplyr::case_when(
      !is.na(Fail_year_override) ~ pmin(Fail_year_override + 1L, YEAR_END + 1L),
      TRUE ~ pmin(Last_start + 2L, YEAR_END + 1L)
    ),
    End_year = dplyr::case_when(
      !is.na(Next_start) ~ Next_start,
      Phase_status == "Failed design" ~ Fail_end,
      TRUE ~ YEAR_END + 1L
    )
  ) %>%
  dplyr::ungroup()

phase_years <- phase_stints %>%
  dplyr::filter(
    !is.na(OECD_Group),
    OECD_Group %in% c("OECD", "Non-OECD")
  ) %>%
  dplyr::mutate(
    Year = purrr::map2(Start_year, End_year, year_seq)
  ) %>%
  tidyr::unnest(cols = c(Year))

phase_counts_oecd <- phase_years %>%
  dplyr::count(OECD_Group, Year, Phase, name = "n_designs") %>%
  dplyr::mutate(
    OECD_Group = factor(OECD_Group, levels = c("OECD", "Non-OECD")),
    Phase = factor(Phase, levels = phase_order)
  )

# ============================================================
# PANEL A — OECD PIPELINE
# ============================================================

counts_oecd_panel <- phase_counts_oecd %>%
  dplyr::filter(OECD_Group == "OECD") %>%
  dplyr::mutate(
    Phase = factor(as.character(Phase), levels = phase_order)
  )

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
# PANEL B — NON-OECD PIPELINE
# ============================================================

counts_nonoecd_panel <- phase_counts_oecd %>%
  dplyr::filter(OECD_Group == "Non-OECD") %>%
  dplyr::mutate(
    Phase = factor(as.character(Phase), levels = phase_order)
  )

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
# PANEL C — REGION BY PHASE
# ============================================================

counts_region <- df_active %>%
  dplyr::filter(
    !is.na(Region),
    Region != "",
    Region %in% region_order
  ) %>%
  dplyr::count(Region, Phase, name = "n_designs") %>%
  dplyr::mutate(
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
# PANEL D — REGION BY COOLING TYPE
# ============================================================

counts_region_type <- df_active %>%
  dplyr::filter(
    !is.na(Region),
    Region != "",
    Region %in% region_order,
    !is.na(Design),
    Design != "",
    Design %in% cool_order
  ) %>%
  dplyr::count(Region, Design, name = "n_designs") %>%
  dplyr::mutate(
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
# ALIGN AND TAG PANELS
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
# LAYOUT
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
  top_row,
  bottom_row,
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
