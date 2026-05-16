

# ============================================================
# FIGURE 3 — GLOBAL ONLY (DESIGNS WITH VENDOR TARGETS ONLY)
# FINAL • CLEAN • LOCKED
# - Uses SMR column for labels
# - Keeps only designs with non-missing vendor_target
# - Global panel only
# - Vendor FOAK target milestone included
# - No panel tag
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(tibble)
})

# ----------------------------
# PATHS
# ----------------------------
FILE <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/fig03_source_data_vendor_targets.xlsx"
FIG3_ROOT <- "C:/Users/jo3151go/OneDrive/Nature_Energy_SMRs_2026/Figure_03"
dir.create(FIG3_ROOT, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# CONSTANTS
# ----------------------------
HIST_MIN <- 1984
HIST_MAX <- 2025.99
FUT_MIN  <- 2026
FUT_MAX  <- 2057.3

VLINE_X   <- 2050
VLINE_LWD <- 0.50

OP_PROJ_LWD       <- 1.2
OP_PROJ_ALPHA_FUT <- 0.48

ERR_H   <- 0.26
ERR_LWD <- 0.70
ACC_LWD <- 0.68

COL_BASE  <- "grey40"
COL_ACC   <- "#228B22"
COL_START <- "#9E6BB1"

SZ_START    <- 1.6
SZ_P50_BASE <- 1.9
SZ_P50_ACC  <- 2.1
SZ_VENDOR   <- 2.7
ST_VENDOR   <- 0.90

SH_BASE <- 17
SH_ACC  <- 18

MARG_IN <- margin(1, 4, 1, 4)

ops_names <- c("HTR-PM", "KLT-40S", "RITM-200")

PHASE_COLS <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)

PHASE_KEY <- tibble(
  col   = c("CD_Start", "BD_Start", "DD_Start", "Lic_Start", "Con_Start"),
  phase = c("Conceptual design", "Basic design", "Detailed design", "Licensing", "Construction"),
  ord   = 1:5
)

axis_theme <- theme(
  axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
  axis.line.y = element_line(colour = "grey35", linewidth = 0.3)
)

clean_key <- function(x) toupper(gsub("[^A-Z0-9]", "", as.character(x)))

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[1]
}

has_content <- function(x) {
  x_chr <- trimws(as.character(x))
  !is.na(x) & x_chr != "" & toupper(x_chr) != "NA"
}

# ----------------------------
# LOAD DATA
# ----------------------------
df0 <- read_excel(FILE)
names(df0) <- gsub("\\s+", "_", names(df0))

ops_key <- clean_key(ops_names)

if (!"vendor_target" %in% names(df0)) {
  stop("Column 'vendor_target' not found in the workbook.")
}

df0 <- df0 %>%
  mutate(
    design = paste0(as.character(SMR), " (", Country, ")"),
    Phase  = as.character(Phase),
    across(c(CD_Start, BD_Start, DD_Start, Lic_Start, Con_Start, P10, P50, P90, vendor_target), as.numeric),
    is_ops_row = clean_key(SMR) %in% ops_key,
    is_operational_row = is_ops_row | (tolower(Phase) == "operational")
  )

vendor_designs <- df0 %>%
  mutate(has_vendor_row = has_content(vendor_target)) %>%
  group_by(design) %>%
  summarise(has_vendor = any(has_vendor_row), .groups = "drop") %>%
  filter(has_vendor)

df <- df0 %>%
  semi_join(vendor_designs, by = "design")

# ----------------------------
# PANEL FUNCTION
# ----------------------------
make_panel <- function(df, fut_min, show_x = TRUE) {
  
  design_oper <- df %>%
    group_by(design) %>%
    summarise(
      is_operational = any(is_operational_row, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_base <- df %>%
    filter(Scenario == "Baseline") %>%
    group_by(design) %>%
    summarise(
      CD_Start      = first_non_na(CD_Start),
      BD_Start      = first_non_na(BD_Start),
      DD_Start      = first_non_na(DD_Start),
      Lic_Start     = first_non_na(Lic_Start),
      Con_Start     = first_non_na(Con_Start),
      P10           = first_non_na(P10),
      P50           = first_non_na(P50),
      P90           = first_non_na(P90),
      vendor_target = first_non_na(vendor_target),
      .groups       = "drop"
    ) %>%
    left_join(design_oper, by = "design")
  
  df_acc <- df %>%
    filter(Scenario == "Accelerated") %>%
    group_by(design) %>%
    summarise(
      P10     = first_non_na(P10),
      P50     = first_non_na(P50),
      P90     = first_non_na(P90),
      .groups = "drop"
    )
  
  y_levels <- df_base %>%
    arrange(CD_Start, P50, design) %>%
    pull(design) %>%
    rev()
  
  df_base <- df_base %>% mutate(design = factor(design, levels = y_levels))
  df_acc  <- df_acc  %>% mutate(design = factor(design, levels = y_levels))
  
  df_long <- df_base %>%
    select(design, is_operational, CD_Start, BD_Start, DD_Start, Lic_Start, Con_Start, P50) %>%
    pivot_longer(
      cols      = c(CD_Start, BD_Start, DD_Start, Lic_Start, Con_Start),
      names_to  = "col",
      values_to = "start"
    ) %>%
    left_join(PHASE_KEY, by = "col") %>%
    filter(!is.na(start)) %>%
    group_by(design) %>%
    arrange(ord, .by_group = TRUE) %>%
    mutate(end = lead(start, default = HIST_MAX)) %>%
    ungroup() %>%
    mutate(
      start = pmax(pmin(start, HIST_MAX), HIST_MIN),
      end   = pmax(pmin(end,   HIST_MAX), HIST_MIN)
    )
  
  df_seg <- df_long %>%
    mutate(
      end = ifelse(
        phase == "Construction" & is_operational & !is.na(P50),
        pmax(pmin(P50, HIST_MAX), start),
        end
      ),
      phase = factor(phase, levels = names(PHASE_COLS))
    ) %>%
    filter(end > start)
  
  df_proj <- df_base %>%
    filter(is_operational) %>%
    transmute(
      design,
      start = fut_min,
      end   = VLINE_X
    )
  
  df_base_nonop <- df_base %>% filter(!is_operational)
  df_acc_nonop  <- df_acc %>%
    left_join(select(df_base, design, is_operational), by = "design") %>%
    filter(!is_operational)
  
  df_vendor <- df_base %>%
    filter(!is.na(vendor_target)) %>%
    mutate(vendor_target = pmax(pmin(vendor_target, FUT_MAX), HIST_MIN))
  
  ggplot() +
    geom_segment(
      data = df_seg,
      aes(x = start, xend = end, y = design, yend = design, colour = phase),
      linewidth = 1.75,
      lineend = "butt"
    ) +
    geom_segment(
      data = df_proj,
      aes(x = start, xend = end, y = design, yend = design),
      colour = "#BDBDBD",
      linewidth = OP_PROJ_LWD,
      alpha = OP_PROJ_ALPHA_FUT,
      lineend = "butt"
    ) +
    geom_errorbarh(
      data = df_base_nonop,
      aes(y = design, xmin = P10, xmax = P90),
      height = ERR_H,
      colour = COL_BASE,
      linewidth = ERR_LWD
    ) +
    geom_errorbarh(
      data = df_acc_nonop,
      aes(y = design, xmin = P10, xmax = P90),
      height = ERR_H,
      colour = COL_ACC,
      linewidth = ACC_LWD
    ) +
    geom_point(
      data = df_base,
      aes(x = pmax(CD_Start, HIST_MIN), y = design),
      colour = COL_START,
      size = SZ_START
    ) +
    geom_point(
      data = df_vendor,
      aes(x = vendor_target, y = design),
      shape = 4,
      stroke = ST_VENDOR,
      size = SZ_VENDOR,
      colour = "black"
    ) +
    geom_point(
      data = df_base_nonop,
      aes(x = P50, y = design),
      shape = SH_BASE,
      colour = COL_BASE,
      size = SZ_P50_BASE
    ) +
    geom_point(
      data = df_acc_nonop,
      aes(x = P50, y = design),
      shape = SH_ACC,
      colour = COL_ACC,
      size = SZ_P50_ACC
    ) +
    geom_vline(
      xintercept = VLINE_X,
      linetype = "dashed",
      colour = "#cc0000",
      linewidth = VLINE_LWD
    ) +
    scale_colour_manual(values = PHASE_COLS, drop = FALSE) +
    scale_y_discrete(limits = y_levels) +
    scale_x_continuous(
      limits = c(HIST_MIN - 0.5, FUT_MAX),
      breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(x = if (show_x) "Year" else NULL, y = "SMR designs (Country)") +
    theme_classic(base_size = 7) +
    axis_theme +
    theme(
      axis.text.y = element_text(size = 6.5),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      legend.position = "none",
      plot.margin = MARG_IN,
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

# ----------------------------
# CUSTOM LEGEND BLOCKS
# ----------------------------

make_dev_block <- function(phases, y_step = 0.68) {
  n <- length(phases)
  ys <- rev(seq_len(n)) * y_step
  title_y <- max(ys) + 0.45
  
  df_lines <- tibble(
    phase = phases,
    y     = ys
  )
  
  ggplot(df_lines) +
    annotate(
      "text",
      x = 0.01, y = title_y,
      label = "Development phase",
      hjust = 0, vjust = 1,
      size = 3.3
    ) +
    geom_segment(
      aes(x = 0.03, xend = 0.14, y = y, yend = y, colour = phase),
      linewidth = 2.0,
      lineend = "butt"
    ) +
    geom_text(
      aes(x = 0.19, y = y, label = phase),
      hjust = 0, vjust = 0.5,
      size = 3.0,
      colour = "black"
    ) +
    scale_colour_manual(values = PHASE_COLS[phases], guide = "none") +
    coord_cartesian(
      xlim = c(0, 1.02),
      ylim = c(0.22, title_y + 0.10),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

make_mile_block <- function() {
  df_mile <- tibble(
    label = c("Conceptual design start year", "Vendor FOAK target"),
    y     = c(1.85, 1.05)
  )
  
  title_y <- 2.55
  
  ggplot() +
    annotate(
      "text",
      x = 0.01, y = title_y,
      label = "Milestones",
      hjust = 0, vjust = 1,
      size = 3.3
    ) +
    geom_point(
      data = df_mile[1, ],
      aes(x = 0.08, y = y),
      colour = COL_START,
      size = 4.0
    ) +
    geom_point(
      data = df_mile[2, ],
      aes(x = 0.08, y = y),
      shape = 4,
      stroke = 0.90,
      size = 4.6,
      colour = "black"
    ) +
    geom_text(
      data = df_mile,
      aes(x = 0.20, y = y, label = label),
      hjust = 0, vjust = 0.5,
      size = 3.0,
      colour = "black"
    ) +
    coord_cartesian(
      xlim = c(0, 1.02),
      ylim = c(0.35, title_y + 0.10),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

make_scen_block <- function() {
  labels <- c(
    "Baseline FOAK median\n(P10–P90 interval)",
    "Accelerated FOAK median\n(P10–P90 interval)"
  )
  
  df_scen <- tibble(
    label = labels,
    y     = c(1.95, 1.10),
    col   = c(COL_BASE, COL_ACC),
    pch   = c(SH_BASE, SH_ACC)
  )
  
  title_y <- 2.65
  
  ggplot() +
    annotate(
      "text",
      x = 0.01, y = title_y,
      label = "Scenario",
      hjust = 0, vjust = 1,
      size = 3.3
    ) +
    geom_segment(
      data = df_scen,
      aes(x = 0.03, xend = 0.15, y = y, yend = y),
      linewidth = 1.0,
      colour = df_scen$col,
      lineend = "butt"
    ) +
    geom_point(
      data = df_scen,
      aes(x = 0.09, y = y),
      shape = df_scen$pch,
      size = 2.8,
      colour = df_scen$col
    ) +
    geom_text(
      data = df_scen,
      aes(x = 0.20, y = y, label = label),
      hjust = 0, vjust = 0.5,
      size = 2.85,
      colour = "black",
      lineheight = 0.95
    ) +
    coord_cartesian(
      xlim = c(0, 1.02),
      ylim = c(0.35, title_y + 0.10),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

final_fig <- cowplot::plot_grid(
  p_main, leg,
  ncol = 2,
  rel_widths = c(0.69, 0.31),
  align = "h",
  axis = "tb"
)


# ----------------------------
# BUILD FIGURE
# ----------------------------
p_main <- make_panel(df, FUT_MIN, TRUE)

phases_legend <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction"
)

leg <- cowplot::plot_grid(
  NULL,
  make_dev_block(phases_legend, y_step = 0.68),
  NULL,
  make_mile_block(),
  NULL,
  make_scen_block(),
  NULL,
  ncol = 1,
  rel_heights = c(0.02, 0.26, 0.015, 0.12, 0.015, 0.15, 0.42)
)

final_fig <- cowplot::plot_grid(
  p_main, leg,
  ncol = 2,
  rel_widths = c(0.70, 0.30),
  align = "h",
  axis = "tb"
)

# ----------------------------
# SAVE
# ----------------------------
w_in <- 180 / 25.4
h_in <- 230 / 25.4

ggsave(
  file.path(FIG3_ROOT, "fig03_vendor_targets.png"),
  final_fig,
  width = w_in, height = h_in, dpi = 600, bg = "white"
)

ggsave(
  file.path(FIG3_ROOT, "fig03_vendor_targets.pdf"),
  final_fig,
  width = w_in, height = h_in, bg = "white", useDingbats = FALSE
)
