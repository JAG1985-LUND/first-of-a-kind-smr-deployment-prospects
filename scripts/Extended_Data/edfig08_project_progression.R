# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Extended Data Fig. 8: historical SMR design pipelines and
# projected FOAK timelines by OECD membership.
#
# Panel a shows OECD designs. Panel b shows non-OECD designs. Coloured
# horizontal segments show historical development phases, purple points mark
# conceptual-design start years, grey/green markers and intervals show
# Baseline and Accelerated FOAK deployment projections, and the dashed
# vertical line marks 2035.
#
# Required input:
# data/Extended_Data/edfig08_source_data_project_progression_2050.xlsx
#
# Outputs:
# outputs/Extended_Data/Figure_08/edfig08_project_progression_2050.png
# outputs/Extended_Data/Figure_08/edfig08_project_progression_2050.pdf
# outputs/Extended_Data/Figure_08/edfig08_project_progression_2050.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(tibble)
})

rm(list = ls())
graphics.off()

# ============================================================
# EXTENDED DATA FIGURE 8
# PROJECT PROGRESSION TO 2050 BY OECD MEMBERSHIP
# ============================================================

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

SOURCE_FILE <- file.path(
  "data",
  "Extended_Data",
  "edfig08_source_data_project_progression_2050.xlsx"
)

find_repo_root <- function(start_dir = getwd(), target_file = SOURCE_FILE) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (file.exists(file.path(current, target_file))) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      stop(
        "Could not locate repository root.\n",
        "Expected to find: ", target_file, "\n",
        "Start directory was: ",
        normalizePath(start_dir, winslash = "/", mustWork = TRUE),
        "\n\nRun this script from inside the GitHub repository, or set the working directory to the repository root."
      )
    }
    
    current <- parent
  }
}

ROOT_DIR <- find_repo_root()

FILE <- file.path(ROOT_DIR, SOURCE_FILE)
OUT_DIR <- file.path(ROOT_DIR, "outputs", "Extended_Data", "Figure_08")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}

# ------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------

HIST_MIN <- 1984
HIST_MAX <- 2025.99

FUT_MIN_OECD <- 2026
FUT_MIN_NON  <- 2026
FUT_MAX      <- 2057.3

OP_PROJ_END       <- 2050
OP_PROJ_LWD       <- 1.2
OP_PROJ_ALPHA_FUT <- 0.48

CUTOFF_X   <- 2035
CUTOFF_LWD <- 0.45

ERR_H   <- 0.26
ERR_LWD <- 0.70
ACC_LWD <- 0.68

COL_BASE  <- "grey40"
COL_ACC   <- "#228B22"
COL_START <- "#9E6BB1"

SZ_START    <- 1.6
SZ_P50_BASE <- 1.9
SZ_P50_ACC  <- 2.1

SH_BASE <- 17
SH_ACC  <- 18

MARG_IN <- margin(1, 4, 1, 4)

FILE_STUB <- "edfig08_project_progression_2050"

ops_names <- c(
  "HTR-PM",
  "KLT-40S",
  "RITM-200"
)

PHASE_COLS <- c(
  "Conceptual design"     = "#FDE0D2",
  "Basic design"          = "#F4A582",
  "Detailed design"       = "#D6604D",
  "Licensing"             = "#D1E5F0",
  "Construction"          = "#92C5DE",
  "Operational"           = "#4393C3",
  "Operational projected" = "#CCCCCC"
)

PHASE_KEY <- tibble(
  col = c(
    "CD_Start",
    "BD_Start",
    "DD_Start",
    "Lic_Start",
    "Con_Start"
  ),
  phase = c(
    "Conceptual design",
    "Basic design",
    "Detailed design",
    "Licensing",
    "Construction"
  ),
  ord = 1:5
)

axis_theme <- theme(
  axis.line.x = element_line(
    colour = "grey35",
    linewidth = 0.3
  ),
  axis.line.y = element_line(
    colour = "grey35",
    linewidth = 0.3
  )
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_key <- function(x) {
  toupper(gsub("[^A-Z0-9]", "", as.character(x)))
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  x[1]
}

# ------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------

df0 <- readxl::read_excel(FILE)

names(df0) <- gsub(
  "\\s+",
  "_",
  names(df0)
)

required_cols <- c(
  "SMR",
  "Country",
  "Phase",
  "OECD",
  "Region",
  "Scenario",
  "CD_Start",
  "BD_Start",
  "DD_Start",
  "Lic_Start",
  "Con_Start",
  "P10",
  "P50",
  "P90"
)

missing_cols <- setdiff(required_cols, names(df0))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns in Extended Data Fig. 8 source workbook:\n",
    paste(missing_cols, collapse = "\n"),
    "\n\nAvailable columns are:\n",
    paste(names(df0), collapse = "\n")
  )
}

ops_key <- clean_key(ops_names)

df0 <- df0 %>%
  dplyr::mutate(
    design = paste0(SMR, " (", Country, ")"),
    Phase = as.character(Phase),
    dplyr::across(
      c(
        CD_Start,
        BD_Start,
        DD_Start,
        Lic_Start,
        Con_Start,
        P10,
        P50,
        P90
      ),
      as.numeric
    ),
    is_ops_row = clean_key(SMR) %in% ops_key,
    is_operational_row = is_ops_row |
      (
        tolower(Phase) == "operational"
      )
  )

# ------------------------------------------------------------
# PANEL FUNCTION
# ------------------------------------------------------------

make_panel <- function(df, fut_min, show_x) {
  
  design_oper <- df %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      is_operational = any(
        is_operational_row,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  df_base <- df %>%
    dplyr::filter(Scenario == "Baseline") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      CD_Start  = first_non_na(CD_Start),
      BD_Start  = first_non_na(BD_Start),
      DD_Start  = first_non_na(DD_Start),
      Lic_Start = first_non_na(Lic_Start),
      Con_Start = first_non_na(Con_Start),
      P10       = first_non_na(P10),
      P50       = first_non_na(P50),
      P90       = first_non_na(P90),
      .groups   = "drop"
    ) %>%
    dplyr::left_join(
      design_oper,
      by = "design"
    )
  
  df_acc <- df %>%
    dplyr::filter(Scenario == "Accelerated") %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      P10 = first_non_na(P10),
      P50 = first_non_na(P50),
      P90 = first_non_na(P90),
      .groups = "drop"
    )
  
  y_levels <- df_base %>%
    dplyr::arrange(
      CD_Start,
      P50,
      design
    ) %>%
    dplyr::pull(design) %>%
    rev()
  
  df_base <- df_base %>%
    dplyr::mutate(
      design = factor(
        design,
        levels = y_levels
      )
    )
  
  df_acc <- df_acc %>%
    dplyr::mutate(
      design = factor(
        design,
        levels = y_levels
      )
    )
  
  df_long <- df_base %>%
    dplyr::select(
      design,
      is_operational,
      CD_Start,
      BD_Start,
      DD_Start,
      Lic_Start,
      Con_Start,
      P50
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        CD_Start,
        BD_Start,
        DD_Start,
        Lic_Start,
        Con_Start
      ),
      names_to = "col",
      values_to = "start"
    ) %>%
    dplyr::left_join(
      PHASE_KEY,
      by = "col"
    ) %>%
    dplyr::filter(!is.na(start)) %>%
    dplyr::group_by(design) %>%
    dplyr::arrange(
      ord,
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      end = dplyr::lead(
        start,
        default = HIST_MAX
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      start = pmax(
        pmin(start, HIST_MAX),
        HIST_MIN
      ),
      end = pmax(
        pmin(end, HIST_MAX),
        HIST_MIN
      )
    )
  
  df_seg <- df_long %>%
    dplyr::mutate(
      end = ifelse(
        phase == "Construction" &
          is_operational &
          !is.na(P50),
        pmax(
          pmin(P50, HIST_MAX),
          start
        ),
        end
      ),
      phase = factor(
        phase,
        levels = names(PHASE_COLS)
      )
    ) %>%
    dplyr::filter(end > start)
  
  df_op <- df_base %>%
    dplyr::filter(is_operational) %>%
    dplyr::transmute(
      design,
      start = pmax(
        pmin(
          dplyr::coalesce(
            P50,
            Con_Start,
            Lic_Start,
            DD_Start,
            BD_Start,
            CD_Start
          ),
          HIST_MAX
        ),
        HIST_MIN
      ),
      end = HIST_MAX,
      phase = factor(
        "Operational",
        levels = names(PHASE_COLS)
      )
    ) %>%
    dplyr::filter(end > start)
  
  df_proj <- df_base %>%
    dplyr::filter(is_operational) %>%
    dplyr::transmute(
      design,
      start = fut_min,
      end = OP_PROJ_END,
      phase = factor(
        "Operational projected",
        levels = names(PHASE_COLS)
      )
    )
  
  df_base_nonop <- df_base %>%
    dplyr::filter(!is_operational)
  
  df_acc_nonop <- df_acc %>%
    dplyr::left_join(
      dplyr::select(
        df_base,
        design,
        is_operational
      ),
      by = "design"
    ) %>%
    dplyr::filter(!is_operational)
  
  ggplot() +
    geom_segment(
      data = df_seg,
      aes(
        x = start,
        xend = end,
        y = design,
        yend = design,
        colour = phase
      ),
      linewidth = 1.75,
      lineend = "butt"
    ) +
    geom_segment(
      data = df_op,
      aes(
        x = start,
        xend = end,
        y = design,
        yend = design,
        colour = phase
      ),
      linewidth = 1.75,
      lineend = "butt"
    ) +
    geom_segment(
      data = df_proj,
      aes(
        x = start,
        xend = end,
        y = design,
        yend = design
      ),
      colour = PHASE_COLS["Operational projected"],
      linewidth = OP_PROJ_LWD,
      alpha = OP_PROJ_ALPHA_FUT,
      lineend = "butt"
    ) +
    geom_errorbarh(
      data = df_base_nonop,
      aes(
        y = design,
        xmin = P10,
        xmax = P90
      ),
      height = ERR_H,
      colour = COL_BASE,
      linewidth = ERR_LWD
    ) +
    geom_errorbarh(
      data = df_acc_nonop,
      aes(
        y = design,
        xmin = P10,
        xmax = P90
      ),
      height = ERR_H,
      colour = COL_ACC,
      linewidth = ACC_LWD
    ) +
    geom_point(
      data = df_base,
      aes(
        x = pmax(CD_Start, HIST_MIN),
        y = design
      ),
      colour = COL_START,
      size = SZ_START
    ) +
    geom_point(
      data = df_base_nonop,
      aes(
        x = P50,
        y = design
      ),
      shape = SH_BASE,
      colour = COL_BASE,
      size = SZ_P50_BASE
    ) +
    geom_point(
      data = df_acc_nonop,
      aes(
        x = P50,
        y = design
      ),
      shape = SH_ACC,
      colour = COL_ACC,
      size = SZ_P50_ACC
    ) +
    geom_vline(
      xintercept = CUTOFF_X,
      linetype = "dashed",
      colour = "#6B7F9E",
      linewidth = CUTOFF_LWD
    ) +
    scale_colour_manual(
      values = PHASE_COLS,
      drop = FALSE
    ) +
    scale_y_discrete(
      limits = y_levels
    ) +
    scale_x_continuous(
      limits = c(HIST_MIN - 0.5, FUT_MAX),
      breaks = c(
        1990,
        2000,
        2010,
        2020,
        2030,
        2040,
        2050
      ),
      expand = expansion(
        mult = c(0, 0.02)
      )
    ) +
    labs(
      x = if (show_x) "Year" else NULL,
      y = NULL
    ) +
    theme_classic(base_size = 7) +
    axis_theme +
    theme(
      axis.text.y = element_text(size = 6.5),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      legend.position = "none",
      plot.margin = MARG_IN,
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

# ------------------------------------------------------------
# CUSTOM LEGEND BLOCKS
# ------------------------------------------------------------

make_dev_block <- function(phases, y_step = 0.86) {
  n <- length(phases)
  ys <- rev(seq_len(n)) * y_step
  title_y <- max(ys) + 0.95
  
  df_lines <- tibble(
    phase = phases,
    y = ys
  )
  
  ggplot(df_lines) +
    annotate(
      "text",
      x = 0.02,
      y = title_y,
      label = "Development phase",
      hjust = 0,
      vjust = 1,
      size = 3.3
    ) +
    geom_segment(
      aes(
        x = 0.04,
        xend = 0.20,
        y = y,
        yend = y,
        colour = phase
      ),
      linewidth = 2.0,
      lineend = "butt"
    ) +
    geom_text(
      aes(
        x = 0.27,
        y = y,
        label = phase
      ),
      hjust = 0,
      vjust = 0.5,
      size = 3.0,
      colour = "black"
    ) +
    scale_colour_manual(
      values = PHASE_COLS[phases],
      guide = "none"
    ) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0.3, title_y + 0.2),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

make_scen_block <- function() {
  labels <- c(
    "Baseline FOAK median\n(P10–P90 interval)",
    "Accelerated FOAK median\n(P10–P90 interval)"
  )
  
  df_scen <- tibble(
    label = labels,
    y = c(2.25, 0.95),
    col = c(COL_BASE, COL_ACC),
    pch = c(SH_BASE, SH_ACC)
  )
  
  title_y <- 3.25
  
  ggplot() +
    annotate(
      "text",
      x = 0.02,
      y = title_y,
      label = "Scenario",
      hjust = 0,
      vjust = 1,
      size = 3.3
    ) +
    geom_segment(
      data = df_scen,
      aes(
        x = 0.04,
        xend = 0.20,
        y = y,
        yend = y
      ),
      linewidth = 1.0,
      colour = df_scen$col,
      lineend = "butt"
    ) +
    geom_point(
      data = df_scen,
      aes(
        x = 0.12,
        y = y
      ),
      shape = df_scen$pch,
      size = 2.8,
      colour = df_scen$col
    ) +
    geom_text(
      data = df_scen,
      aes(
        x = 0.27,
        y = y,
        label = label
      ),
      hjust = 0,
      vjust = 0.5,
      size = 2.85,
      colour = "black",
      lineheight = 0.95
    ) +
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0.35, title_y + 0.15),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

stack_legends_B <- function(dev_block, scen_block) {
  cowplot::plot_grid(
    NULL,
    dev_block,
    NULL,
    scen_block,
    NULL,
    ncol = 1,
    rel_heights = c(
      0.05,
      0.56,
      0.035,
      0.255,
      0.10
    )
  )
}

# ------------------------------------------------------------
# BUILD PANELS
# ------------------------------------------------------------

df_oecd <- df0 %>%
  dplyr::filter(OECD == "Yes")

df_non <- df0 %>%
  dplyr::filter(
    OECD == "No",
    Region != "North America"
  )

pA_main <- make_panel(
  df_oecd,
  FUT_MIN_OECD,
  FALSE
)

pB_main <- make_panel(
  df_non,
  FUT_MIN_NON,
  TRUE
)

phases_A <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing"
)

phases_B <- c(
  "Conceptual design",
  "Basic design",
  "Detailed design",
  "Licensing",
  "Construction",
  "Operational",
  "Operational projected"
)

# ------------------------------------------------------------
# LEGEND STACKING
# ------------------------------------------------------------

legA <- cowplot::plot_grid(
  NULL,
  make_dev_block(phases_A, y_step = 0.74),
  NULL,
  make_scen_block(),
  NULL,
  ncol = 1,
  rel_heights = c(
    0.08,
    0.11,
    0.012,
    0.085,
    0.11
  )
)

legB <- stack_legends_B(
  make_dev_block(phases_B, y_step = 0.82),
  make_scen_block()
)

aligned <- cowplot::align_plots(
  pA_main,
  pB_main,
  align = "v",
  axis = "l"
)

pA <- cowplot::plot_grid(
  aligned[[1]],
  legA,
  ncol = 2,
  rel_widths = c(0.73, 0.27)
)

pB <- cowplot::plot_grid(
  aligned[[2]],
  legB,
  ncol = 2,
  rel_widths = c(0.73, 0.27)
)

pA <- cowplot::ggdraw(pA) +
  cowplot::draw_plot_label(
    "a",
    x = 0.002,
    y = 1.00,
    size = 12,
    fontface = "bold"
  )

pB <- cowplot::ggdraw(pB) +
  cowplot::draw_plot_label(
    "b",
    x = 0.002,
    y = 1.05,
    size = 12,
    fontface = "bold"
  )

final_fig <- cowplot::plot_grid(
  pA,
  pB,
  ncol = 1,
  rel_heights = c(1.5, 1),
  align = "v",
  axis = "l"
)

print(final_fig)

# ------------------------------------------------------------
# SAVE EXTENDED DATA FIGURE 8
# ------------------------------------------------------------

w_in <- 180 / 25.4
h_in <- 215 / 25.4

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".png")
  ),
  plot = final_fig,
  width = w_in,
  height = h_in,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".pdf")
  ),
  plot = final_fig,
  width = w_in,
  height = h_in,
  bg = "white",
  useDingbats = FALSE
)

saveRDS(
  final_fig,
  file = file.path(
    OUT_DIR,
    paste0(FILE_STUB, ".rds")
  )
)

final_fig