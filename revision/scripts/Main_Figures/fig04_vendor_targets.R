# ============================================================
# FIGURE 4
# FOAK deployment timelines for SMR designs with
# vendor-stated targets
#
# GITHUB REPRODUCTION SCRIPT
#
# Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
# ============================================================


# ============================================================
# 1. PACKAGES
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


rm(list = ls())
graphics.off()


# ============================================================
# 2. REPOSITORY PATHS
# ============================================================

find_repo_root <- function(start = getwd()) {
  
  current <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )
  
  repeat {
    
    source_file <- file.path(
      current,
      "revision",
      "data",
      "Main_Figures",
      "fig04_source_data_vendor_targets.xlsx"
    )
    
    if (file.exists(source_file)) {
      return(current)
    }
    
    parent <- dirname(current)
    
    if (identical(parent, current)) {
      
      stop(
        paste0(
          "Could not locate repository root.\n",
          "Expected to find: ",
          "revision/data/Main_Figures/",
          "fig04_source_data_vendor_targets.xlsx\n",
          "Start directory was: ",
          start,
          "\n\n",
          "Run this script from inside the GitHub repository."
        )
      )
    }
    
    current <- parent
  }
}


ROOT_DIR <- find_repo_root()


FILE <- file.path(
  ROOT_DIR,
  "revision",
  "data",
  "Main_Figures",
  "fig04_source_data_vendor_targets.xlsx"
)


OUT <- file.path(
  ROOT_DIR,
  "revision",
  "outputs",
  "Main_Figures"
)


dir.create(
  OUT,
  recursive = TRUE,
  showWarnings = FALSE
)


if (!file.exists(FILE)) {
  stop("Input file not found: ", FILE)
}


FILE_STUB <- "fig04_vendor_targets"


# ============================================================
# 3. EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 180
SAVE_H_MM <- 198
SAVE_DPI  <- 600


# ============================================================
# 4. CONSTANTS
# ============================================================

HIST_MIN <- 1984
HIST_MAX <- 2025.99

FUT_MIN <- 2026
FUT_MAX <- 2057.3

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


ops_names <- c(
  "HTR-PM",
  "KLT-40S",
  "RITM-200"
)


PHASE_COLS <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
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


# ============================================================
# 5. HELPERS
# ============================================================

axis_theme <- theme(
  axis.line.x = element_line(
    colour = "grey35",
    linewidth = 0.3
  ),
  axis.line.y = element_line(
    colour = "grey35",
    linewidth = 0.3
  ),
  axis.ticks = element_line(
    colour = "grey35",
    linewidth = 0.3
  )
)


clean_key <- function(x) {
  
  toupper(
    gsub(
      "[^A-Z0-9]",
      "",
      as.character(x)
    )
  )
}


first_non_na <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  x[1]
}


has_content <- function(x) {
  
  x_chr <- trimws(
    as.character(x)
  )
  
  !is.na(x) &
    x_chr != "" &
    toupper(x_chr) != "NA"
}


# ============================================================
# 6. LOAD SOURCE DATA
# ============================================================

df0 <- readxl::read_excel(
  FILE
)


names(df0) <- gsub(
  "\\s+",
  "_",
  names(df0)
)


# ============================================================
# 7. CHECK REQUIRED COLUMNS
# ============================================================

required_cols <- c(
  "SMR",
  "Country",
  "Phase",
  "Scenario",
  "CD_Start",
  "BD_Start",
  "DD_Start",
  "Lic_Start",
  "Con_Start",
  "P10",
  "P50",
  "P90",
  "vendor_target"
)


missing_cols <- setdiff(
  required_cols,
  names(df0)
)


if (length(missing_cols) > 0) {
  
  stop(
    "Missing required columns in Figure 4 source workbook:\n",
    paste(
      missing_cols,
      collapse = "\n"
    ),
    "\n\nAvailable columns are:\n",
    paste(
      names(df0),
      collapse = "\n"
    )
  )
}


# ============================================================
# 8. STANDARDISE SOURCE DATA
# ============================================================

ops_key <- clean_key(
  ops_names
)


df0 <- df0 %>%
  mutate(
    
    design = paste0(
      as.character(SMR),
      " (",
      Country,
      ")"
    ),
    
    Phase = as.character(
      Phase
    ),
    
    across(
      c(
        CD_Start,
        BD_Start,
        DD_Start,
        Lic_Start,
        Con_Start,
        P10,
        P50,
        P90,
        vendor_target
      ),
      as.numeric
    ),
    
    is_ops_row =
      clean_key(SMR) %in% ops_key,
    
    is_operational_row =
      is_ops_row |
      (tolower(Phase) == "operational")
  )


# ============================================================
# 9. RETAIN DESIGNS WITH VENDOR FOAK TARGETS
# ============================================================

vendor_designs <- df0 %>%
  
  mutate(
    has_vendor_row =
      has_content(vendor_target)
  ) %>%
  
  group_by(
    design
  ) %>%
  
  summarise(
    has_vendor =
      any(has_vendor_row),
    .groups = "drop"
  ) %>%
  
  filter(
    has_vendor
  )


df <- df0 %>%
  
  semi_join(
    vendor_designs,
    by = "design"
  )


# ============================================================
# 10. MAIN PANEL
# ============================================================

make_panel <- function(
    df,
    fut_min,
    show_x = TRUE
) {
  
  design_oper <- df %>%
    
    group_by(
      design
    ) %>%
    
    summarise(
      is_operational =
        any(
          is_operational_row,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
  
  
  # The source workbook retains the original scenario identifiers
  # "Baseline" and "Accelerated". These are displayed in the figure
  # as "Reference" and "Proliferation", respectively.
  
  df_base <- df %>%
    
    filter(
      Scenario == "Baseline"
    ) %>%
    
    group_by(
      design
    ) %>%
    
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
    
    left_join(
      design_oper,
      by = "design"
    )
  
  
  df_acc <- df %>%
    
    filter(
      Scenario == "Accelerated"
    ) %>%
    
    group_by(
      design
    ) %>%
    
    summarise(
      P10 = first_non_na(P10),
      P50 = first_non_na(P50),
      P90 = first_non_na(P90),
      .groups = "drop"
    )
  
  
  y_levels <- df_base %>%
    
    arrange(
      CD_Start,
      P50,
      design
    ) %>%
    
    pull(
      design
    ) %>%
    
    rev()
  
  
  df_base <- df_base %>%
    
    mutate(
      design = factor(
        design,
        levels = y_levels
      )
    )
  
  
  df_acc <- df_acc %>%
    
    mutate(
      design = factor(
        design,
        levels = y_levels
      )
    )
  
  
  # ----------------------------------------------------------
  # HISTORICAL DEVELOPMENT-PHASE SEGMENTS
  # ----------------------------------------------------------
  
  df_long <- df_base %>%
    
    select(
      design,
      is_operational,
      CD_Start,
      BD_Start,
      DD_Start,
      Lic_Start,
      Con_Start,
      P50
    ) %>%
    
    pivot_longer(
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
    
    left_join(
      PHASE_KEY,
      by = "col"
    ) %>%
    
    filter(
      !is.na(start)
    ) %>%
    
    group_by(
      design
    ) %>%
    
    arrange(
      ord,
      .by_group = TRUE
    ) %>%
    
    mutate(
      end = lead(
        start,
        default = HIST_MAX
      )
    ) %>%
    
    ungroup() %>%
    
    mutate(
      
      start = pmax(
        pmin(
          start,
          HIST_MAX
        ),
        HIST_MIN
      ),
      
      end = pmax(
        pmin(
          end,
          HIST_MAX
        ),
        HIST_MIN
      )
    )
  
  
  df_seg <- df_long %>%
    
    mutate(
      
      end = ifelse(
        phase == "Construction" &
          is_operational &
          !is.na(P50),
        
        pmax(
          pmin(
            P50,
            HIST_MAX
          ),
          start
        ),
        
        end
      ),
      
      phase = factor(
        phase,
        levels = names(PHASE_COLS)
      )
    ) %>%
    
    filter(
      end > start
    )
  
  
  # ----------------------------------------------------------
  # FUTURE PROJECTION SEGMENTS FOR OPERATIONAL DESIGNS
  # ----------------------------------------------------------
  
  df_proj <- df_base %>%
    
    filter(
      is_operational
    ) %>%
    
    transmute(
      design,
      start = fut_min,
      end   = VLINE_X
    )
  
  
  # ----------------------------------------------------------
  # REFERENCE AND PROLIFERATION MODEL OUTPUTS
  # ----------------------------------------------------------
  
  df_base_nonop <- df_base %>%
    
    filter(
      !is_operational
    )
  
  
  df_acc_nonop <- df_acc %>%
    
    left_join(
      select(
        df_base,
        design,
        is_operational
      ),
      by = "design"
    ) %>%
    
    filter(
      !is_operational
    )
  
  
  # ----------------------------------------------------------
  # VENDOR FOAK TARGETS
  # ----------------------------------------------------------
  
  df_vendor <- df_base %>%
    
    filter(
      !is.na(vendor_target)
    ) %>%
    
    mutate(
      vendor_target = pmax(
        pmin(
          vendor_target,
          FUT_MAX
        ),
        HIST_MIN
      )
    )
  
  
  # ----------------------------------------------------------
  # DRAW MAIN PANEL
  # ----------------------------------------------------------
  
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
      data = df_proj,
      aes(
        x = start,
        xend = end,
        y = design,
        yend = design
      ),
      colour = "#BDBDBD",
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
        x = pmax(
          CD_Start,
          HIST_MIN
        ),
        y = design
      ),
      colour = COL_START,
      size = SZ_START
    ) +
    
    geom_point(
      data = df_vendor,
      aes(
        x = vendor_target,
        y = design
      ),
      shape = 4,
      stroke = ST_VENDOR,
      size = SZ_VENDOR,
      colour = "black"
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
      xintercept = VLINE_X,
      linetype = "dashed",
      colour = "#cc0000",
      linewidth = VLINE_LWD
    ) +
    
    scale_colour_manual(
      values = PHASE_COLS,
      drop = FALSE
    ) +
    
    scale_y_discrete(
      limits = y_levels
    ) +
    
    scale_x_continuous(
      limits = c(
        HIST_MIN - 0.5,
        FUT_MAX
      ),
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
        mult = c(
          0,
          0.02
        )
      )
    ) +
    
    labs(
      x = if (show_x) "Year" else NULL,
      y = "SMR designs (Country)"
    ) +
    
    theme_classic(
      base_size = 7
    ) +
    
    axis_theme +
    
    theme(
      
      axis.text.y = element_text(
        size = 6.5,
        colour = "black"
      ),
      
      axis.text.x = element_text(
        size = 7,
        colour = "grey20"
      ),
      
      axis.title.x = element_text(
        size = 8,
        colour = "black"
      ),
      
      axis.title.y = element_text(
        size = 8,
        colour = "black"
      ),
      
      legend.position = "none",
      
      plot.margin = margin(
        t = 1,
        r = 4,
        b = 0,
        l = 4
      ),
      
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


# ============================================================
# 11. FIXED BOTTOM LEGEND
# ============================================================

make_bottom_legend <- function() {
  
  x_title <- -0.040
  
  y_phase <- 0.74
  y_mile  <- 0.50
  y_scen  <- 0.26
  
  legend_shift <- -0.025
  
  x_sym_1 <- 0.155 + legend_shift
  x_txt_1 <- 0.190 + legend_shift
  
  x_sym_2 <- 0.500 + legend_shift
  x_txt_2 <- 0.540 + legend_shift
  
  key_len <- 0.028
  
  phase_x <- c(
    x_sym_1 - key_len / 2,
    0.350 + legend_shift,
    0.500 + legend_shift,
    0.675 + legend_shift,
    0.820 + legend_shift
  )
  
  phase_txt <-
    phase_x +
    key_len +
    0.014
  
  
  ggplot() +
    
    coord_cartesian(
      xlim = c(0, 1),
      ylim = c(0, 1),
      clip = "off"
    ) +
    
    theme_void() +
    
    theme(
      
      plot.margin = margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      ),
      
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        colour = NA
      )
    ) +
    
    
    # --------------------------------------------------------
  # PHASE
  # --------------------------------------------------------
  
  annotate(
    "text",
    x = x_title,
    y = y_phase,
    label = "Phase",
    hjust = 0,
    vjust = 0.5,
    size = 2.25,
    colour = "black"
  ) +
    
    annotate(
      "segment",
      x = phase_x[1],
      xend = phase_x[1] + key_len,
      y = y_phase,
      yend = y_phase,
      linewidth = 2.0,
      colour = PHASE_COLS["Conceptual design"],
      lineend = "butt"
    ) +
    
    annotate(
      "text",
      x = phase_txt[1],
      y = y_phase,
      label = "Conceptual design",
      hjust = 0,
      vjust = 0.5,
      size = 2.05,
      colour = "black"
    ) +
    
    annotate(
      "segment",
      x = phase_x[2],
      xend = phase_x[2] + key_len,
      y = y_phase,
      yend = y_phase,
      linewidth = 2.0,
      colour = PHASE_COLS["Basic design"],
      lineend = "butt"
    ) +
    
    annotate(
      "text",
      x = phase_txt[2],
      y = y_phase,
      label = "Basic design",
      hjust = 0,
      vjust = 0.5,
      size = 2.05,
      colour = "black"
    ) +
    
    annotate(
      "segment",
      x = phase_x[3],
      xend = phase_x[3] + key_len,
      y = y_phase,
      yend = y_phase,
      linewidth = 2.0,
      colour = PHASE_COLS["Detailed design"],
      lineend = "butt"
    ) +
    
    annotate(
      "text",
      x = phase_txt[3],
      y = y_phase,
      label = "Detailed design",
      hjust = 0,
      vjust = 0.5,
      size = 2.05,
      colour = "black"
    ) +
    
    annotate(
      "segment",
      x = phase_x[4],
      xend = phase_x[4] + key_len,
      y = y_phase,
      yend = y_phase,
      linewidth = 2.0,
      colour = PHASE_COLS["Licensing"],
      lineend = "butt"
    ) +
    
    annotate(
      "text",
      x = phase_txt[4],
      y = y_phase,
      label = "Licensing",
      hjust = 0,
      vjust = 0.5,
      size = 2.05,
      colour = "black"
    ) +
    
    annotate(
      "segment",
      x = phase_x[5],
      xend = phase_x[5] + key_len,
      y = y_phase,
      yend = y_phase,
      linewidth = 2.0,
      colour = PHASE_COLS["Construction"],
      lineend = "butt"
    ) +
    
    annotate(
      "text",
      x = phase_txt[5],
      y = y_phase,
      label = "Construction",
      hjust = 0,
      vjust = 0.5,
      size = 2.05,
      colour = "black"
    ) +
    
    
    # --------------------------------------------------------
  # MILESTONES
  # --------------------------------------------------------
  
  annotate(
    "text",
    x = x_title,
    y = y_mile,
    label = "Milestones",
    hjust = 0,
    vjust = 0.5,
    size = 2.25,
    colour = "black"
  ) +
    
    annotate(
      "point",
      x = x_sym_1,
      y = y_mile,
      colour = COL_START,
      size = 2.8
    ) +
    
    annotate(
      "text",
      x = x_txt_1,
      y = y_mile,
      label = "Conceptual design start year",
      hjust = 0,
      vjust = 0.5,
      size = 2.25,
      colour = "black"
    ) +
    
    annotate(
      "point",
      x = x_sym_2,
      y = y_mile,
      shape = 4,
      stroke = 0.90,
      size = 4.2,
      colour = "black"
    ) +
    
    annotate(
      "text",
      x = x_txt_2,
      y = y_mile,
      label = "Vendor FOAK target",
      hjust = 0,
      vjust = 0.5,
      size = 2.25,
      colour = "black"
    ) +
    
    
    # --------------------------------------------------------
  # SCENARIO
  # --------------------------------------------------------
  
  annotate(
    "text",
    x = x_title,
    y = y_scen,
    label = "Scenario",
    hjust = 0,
    vjust = 0.5,
    size = 2.25,
    colour = "black"
  ) +
    
    annotate(
      "segment",
      x = x_sym_1 - 0.025,
      xend = x_sym_1 + 0.030,
      y = y_scen,
      yend = y_scen,
      linewidth = 1.0,
      colour = COL_BASE,
      lineend = "butt"
    ) +
    
    annotate(
      "point",
      x = x_sym_1,
      y = y_scen,
      shape = SH_BASE,
      size = 2.7,
      colour = COL_BASE
    ) +
    
    annotate(
      "text",
      x = x_txt_1,
      y = y_scen,
      label = "Reference FOAK median\n(P10-P90 interval)",
      hjust = 0,
      vjust = 0.5,
      size = 2.25,
      colour = "black",
      lineheight = 0.92
    ) +
    
    annotate(
      "segment",
      x = x_sym_2 - 0.025,
      xend = x_sym_2 + 0.030,
      y = y_scen,
      yend = y_scen,
      linewidth = 1.0,
      colour = COL_ACC,
      lineend = "butt"
    ) +
    
    annotate(
      "point",
      x = x_sym_2,
      y = y_scen,
      shape = SH_ACC,
      size = 2.7,
      colour = COL_ACC
    ) +
    
    annotate(
      "text",
      x = x_txt_2,
      y = y_scen,
      label = "Proliferation FOAK median\n(P10-P90 interval)",
      hjust = 0,
      vjust = 0.5,
      size = 2.25,
      colour = "black",
      lineheight = 0.92
    )
}


# ============================================================
# 12. BUILD FIGURE
# ============================================================

p_main <- make_panel(
  df,
  FUT_MIN,
  TRUE
)


p_legend <- make_bottom_legend()


final_fig <- plot_grid(
  p_main,
  p_legend,
  ncol = 1,
  rel_heights = c(
    1,
    0.115
  ),
  align = "v",
  axis = "lr"
)


print(
  final_fig
)


# ============================================================
# 13. SAVE PNG
# ============================================================

ggsave(
  filename = file.path(
    OUT,
    paste0(
      FILE_STUB,
      ".png"
    )
  ),
  plot = final_fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


# ============================================================
# 14. SAVE PDF
# ============================================================

ggsave(
  filename = file.path(
    OUT,
    paste0(
      FILE_STUB,
      ".pdf"
    )
  ),
  plot = final_fig,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)


# ============================================================
# 15. CONFIRM OUTPUTS
# ============================================================

cat(
  "\nFigure 4 successfully generated.\n"
)


cat(
  "Input:\n",
  FILE,
  "\n\n"
)


cat(
  "Outputs:\n"
)


cat(
  file.path(
    OUT,
    paste0(
      FILE_STUB,
      ".png"
    )
  ),
  "\n"
)


cat(
  file.path(
    OUT,
    paste0(
      FILE_STUB,
      ".pdf"
    )
  ),
  "\n"
)