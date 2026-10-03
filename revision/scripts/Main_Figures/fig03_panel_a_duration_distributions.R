# ============================================================
# FIGURE 3 — PANEL A
# PHASE-DURATION DISTRIBUTIONS
# GITHUB-REPRODUCIBLE VERSION
#
# Reproduces Figure 3a: phase-duration assumptions by
# development stage and scenario for the probabilistic
# framework of SMR project progression and pipeline expansion.
#
# Panel a shows scenario-specific phase-duration distributions
# for five SMR development stages:
#   Conceptual design (CD)
#   Basic design (BD)
#   Detailed design (DD)
#   Licensing (LIC)
#   Construction (CON)
#
# Curves show Reference and Proliferation scenario assumptions;
# vertical ticks indicate median durations.
#
# Required input:
# revision/data/Main_Figures/
#   fig03_source_data_foak_framework.xlsx
#
# Required sheet:
#   phase_durations
#
# Outputs:
# revision/outputs/Main_Figures/
#   fig03_panel_a_duration_distributions.png
#   fig03_panel_a_duration_distributions.pdf
#   fig03_panel_a_duration_distributions.rds
#   fig03_panel_a_duration_distributions_core.rds
# ============================================================


suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(stringr)
  library(tibble)
})


rm(list = ls())
graphics.off()


# ============================================================
# REPOSITORY PATHS
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
      "fig03_source_data_foak_framework.xlsx"
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
          "fig03_source_data_foak_framework.xlsx\n",
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
  "fig03_source_data_foak_framework.xlsx"
)


OUT_DIR <- file.path(
  ROOT_DIR,
  "revision",
  "outputs",
  "Main_Figures"
)


if (!file.exists(FILE)) {
  
  stop(
    "Input file not found: ",
    FILE
  )
}


dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# REQUIRED SHEET
# ============================================================

available_sheets <- readxl::excel_sheets(
  FILE
)


sheet_name <- available_sheets[
  stringr::str_detect(
    tolower(available_sheets),
    "phase_durations"
  )
][1]


if (is.na(sheet_name)) {
  
  stop(
    "Could not find a sheet matching 'phase_durations'."
  )
}


# ============================================================
# EXPORT SETTINGS
# ============================================================

SAVE_W_MM <- 88
SAVE_H_MM <- 76
SAVE_DPI  <- 600

FILE_STUB <- "fig03_panel_a_duration_distributions"


# ============================================================
# TYPOGRAPHY
# ============================================================

BASE_SIZE        <- 7
AXIS_TEXT_SIZE   <- 6.5
AXIS_TITLE_SIZE  <- 7
PANEL_LABEL_SIZE <- 12


# ============================================================
# COLOURS
# ============================================================

phase_colors <- c(
  "Conceptual design" = "#FDE0D2",
  "Basic design"      = "#F4A582",
  "Detailed design"   = "#D6604D",
  "Licensing"         = "#D1E5F0",
  "Construction"      = "#92C5DE"
)


scenario_line_colors <- c(
  "Reference"     = "grey30",
  "Proliferation" = "#006400"
)


# ============================================================
# PANEL LABEL POSITION
# ============================================================

TAG_X <- 0.02
TAG_Y <- 0.98


# ============================================================
# GEOMETRY
# ============================================================

phase_center <- c(
  "Conceptual design" = 5,
  "Basic design"      = 4,
  "Detailed design"   = 3,
  "Licensing"         = 2,
  "Construction"      = 1
)


ridge_height <- 0.24
pair_gap     <- 0.18
x_step       <- 0.02

X_MIN <- 0
X_MAX <- 15


# ============================================================
# HELPERS
# ============================================================

clean_names_local <- function(x) {
  
  x <- trimws(x)
  
  x <- gsub(
    "[[:space:][:punct:]]+",
    "_",
    x
  )
  
  x <- gsub(
    "_+",
    "_",
    x
  )
  
  x <- gsub(
    "^_|_$",
    "",
    x
  )
  
  tolower(x)
}


trunc_lognorm_density <- function(
    x,
    meanlog,
    sdlog,
    lower,
    upper
) {
  
  denom <-
    plnorm(
      upper,
      meanlog,
      sdlog
    ) -
    plnorm(
      lower,
      meanlog,
      sdlog
    )
  
  d <-
    dlnorm(
      x,
      meanlog,
      sdlog
    ) /
    denom
  
  d[
    x < lower |
      x > upper
  ] <- NA_real_
  
  d
}


trunc_lognorm_quantile <- function(
    p,
    meanlog,
    sdlog,
    lower,
    upper
) {
  
  p_lower <- plnorm(
    lower,
    meanlog,
    sdlog
  )
  
  p_upper <- plnorm(
    upper,
    meanlog,
    sdlog
  )
  
  qlnorm(
    p_lower +
      p *
      (
        p_upper -
          p_lower
      ),
    meanlog,
    sdlog
  )
}


map_phase <- function(x) {
  
  x2 <- tolower(
    trimws(
      as.character(x)
    )
  )
  
  case_when(
    
    str_detect(
      x2,
      "concept"
    ) |
      str_detect(
        x2,
        "\\bcd\\b"
      ) ~
      "Conceptual design",
    
    str_detect(
      x2,
      "basic"
    ) |
      str_detect(
        x2,
        "\\bbd\\b"
      ) ~
      "Basic design",
    
    str_detect(
      x2,
      "detailed"
    ) |
      str_detect(
        x2,
        "\\bdd\\b"
      ) ~
      "Detailed design",
    
    str_detect(
      x2,
      "licens"
    ) ~
      "Licensing",
    
    str_detect(
      x2,
      "construct"
    ) ~
      "Construction",
    
    TRUE ~
      NA_character_
  )
}


map_scenario <- function(x) {
  
  x2 <- tolower(
    trimws(
      as.character(x)
    )
  )
  
  case_when(
    
    str_detect(
      x2,
      "proliferation"
    ) |
      str_detect(
        x2,
        "^pro"
      ) |
      str_detect(
        x2,
        "accelerated"
      ) |
      str_detect(
        x2,
        "^acc"
      ) ~
      "Proliferation",
    
    str_detect(
      x2,
      "reference"
    ) |
      str_detect(
        x2,
        "^ref"
      ) |
      str_detect(
        x2,
        "baseline"
      ) |
      str_detect(
        x2,
        "^bas"
      ) ~
      "Reference",
    
    TRUE ~
      NA_character_
  )
}


# ============================================================
# READ + STANDARDISE
# ============================================================

raw_a <- readxl::read_excel(
  FILE,
  sheet = sheet_name
)


names(raw_a) <- clean_names_local(
  names(raw_a)
)


required_cols <- c(
  "phase",
  "scenario",
  "mu_log",
  "sigma_log",
  "lower",
  "upper"
)


missing_cols <- setdiff(
  required_cols,
  names(raw_a)
)


if (length(missing_cols) > 0) {
  
  stop(
    paste0(
      "Missing required columns in phase_durations sheet:\n",
      paste(
        missing_cols,
        collapse = ", "
      ),
      "\n\nAvailable columns are:\n",
      paste(
        names(raw_a),
        collapse = "\n"
      )
    )
  )
}


dat_a <- raw_a %>%
  
  transmute(
    
    phase = map_phase(
      phase
    ),
    
    scenario = map_scenario(
      scenario
    ),
    
    mu_log = suppressWarnings(
      as.numeric(
        mu_log
      )
    ),
    
    sigma_log = suppressWarnings(
      as.numeric(
        sigma_log
      )
    ),
    
    lower = suppressWarnings(
      as.numeric(
        lower
      )
    ),
    
    upper = suppressWarnings(
      as.numeric(
        upper
      )
    )
  ) %>%
  
  mutate(
    
    p50 = trunc_lognorm_quantile(
      0.5,
      mu_log,
      sigma_log,
      lower,
      upper
    )
  ) %>%
  
  filter(
    !is.na(phase),
    !is.na(scenario),
    !is.na(mu_log),
    !is.na(sigma_log),
    !is.na(lower),
    !is.na(upper),
    !is.na(p50)
  ) %>%
  
  distinct() %>%
  
  arrange(
    
    factor(
      phase,
      levels = names(
        phase_center
      )
    ),
    
    factor(
      scenario,
      levels = c(
        "Reference",
        "Proliferation"
      )
    )
  )


expected_rows <- expand.grid(
  phase = names(
    phase_center
  ),
  scenario = c(
    "Reference",
    "Proliferation"
  ),
  stringsAsFactors = FALSE
)


missing_rows <- anti_join(
  expected_rows,
  dat_a,
  by = c(
    "phase",
    "scenario"
  )
)


if (nrow(missing_rows) > 0) {
  
  stop(
    paste0(
      "Missing one or more phase-scenario combinations:\n",
      paste(
        paste(
          missing_rows$phase,
          missing_rows$scenario
        ),
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# BUILD DENSITY DATA
# ============================================================

density_list <- lapply(
  seq_len(
    nrow(dat_a)
  ),
  function(i) {
    
    row_i <- dat_a[
      i,
    ]
    
    
    x_seq <- seq(
      from = max(
        row_i$lower,
        X_MIN
      ),
      to = min(
        row_i$upper,
        X_MAX
      ),
      by = x_step
    )
    
    
    dens <- trunc_lognorm_density(
      x = x_seq,
      meanlog = row_i$mu_log,
      sdlog = row_i$sigma_log,
      lower = row_i$lower,
      upper = row_i$upper
    )
    
    
    dens_scaled <-
      dens /
      max(
        dens,
        na.rm = TRUE
      ) *
      ridge_height
    
    
    centre <- unname(
      phase_center[
        row_i$phase
      ]
    )
    
    
    if (
      row_i$scenario ==
      "Reference"
    ) {
      
      y_bottom <-
        centre +
        pair_gap /
        2
      
      y_top <-
        y_bottom +
        dens_scaled
      
    } else {
      
      y_bottom <-
        centre -
        pair_gap /
        2 -
        ridge_height
      
      y_top <-
        y_bottom +
        dens_scaled
    }
    
    
    tibble(
      phase = row_i$phase,
      scenario = row_i$scenario,
      x = x_seq,
      density = dens,
      y_bottom = y_bottom,
      y_top = y_top,
      centre = centre
    )
  }
)


density_df <- bind_rows(
  density_list
)


median_df <- density_df %>%
  
  group_by(
    phase,
    scenario,
    centre
  ) %>%
  
  summarise(
    
    p50 =
      dat_a$p50[
        match(
          paste(
            phase,
            scenario
          ),
          paste(
            dat_a$phase,
            dat_a$scenario
          )
        )
      ][1],
    
    y_bottom = approx(
      x,
      y_bottom,
      xout = p50,
      rule = 2
    )$y,
    
    y_top = approx(
      x,
      y_top,
      xout = p50,
      rule = 2
    )$y,
    
    .groups = "drop"
  )


# ============================================================
# PANEL A CORE
# ============================================================

y_pad <-
  ridge_height +
  pair_gap /
  2 +
  0.08


p_a_core <- ggplot() +
  
  geom_ribbon(
    
    data = density_df,
    
    aes(
      x = x,
      ymin = y_bottom,
      ymax = y_top,
      fill = phase,
      group = interaction(
        phase,
        scenario
      )
    ),
    
    alpha = 0.80
  ) +
  
  geom_line(
    
    data = density_df,
    
    aes(
      x = x,
      y = y_top,
      colour = scenario,
      group = interaction(
        phase,
        scenario
      )
    ),
    
    linewidth = 0.72
  ) +
  
  geom_segment(
    
    data = median_df,
    
    aes(
      x = p50,
      xend = p50,
      y = y_bottom,
      yend = y_top
    ),
    
    colour = "black",
    linewidth = 0.72
  ) +
  
  scale_fill_manual(
    values = phase_colors,
    guide = "none"
  ) +
  
  scale_colour_manual(
    values = scenario_line_colors,
    guide = "none"
  ) +
  
  scale_x_continuous(
    
    limits = c(
      X_MIN,
      X_MAX
    ),
    
    breaks = seq(
      0,
      14,
      by = 2
    ),
    
    expand = c(
      0,
      0
    )
  ) +
  
  scale_y_continuous(
    
    limits = c(
      min(
        phase_center
      ) -
        y_pad,
      
      max(
        phase_center
      ) +
        y_pad
    ),
    
    breaks = c(
      5,
      4,
      3,
      2,
      1
    ),
    
    labels = c(
      "CD",
      "BD",
      "DD",
      "LIC",
      "CON"
    ),
    
    expand = c(
      0,
      0
    )
  ) +
  
  labs(
    x = "Phase duration (years)",
    y = NULL
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  theme_classic(
    base_size = BASE_SIZE
  ) +
  
  theme(
    
    axis.title.x = element_text(
      size = AXIS_TITLE_SIZE,
      colour = "black"
    ),
    
    axis.title.y = element_blank(),
    
    axis.text.x = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    axis.text.y = element_text(
      size = AXIS_TEXT_SIZE,
      colour = "grey20"
    ),
    
    axis.line.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    
    axis.line.y = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    
    axis.ticks.x = element_line(
      colour = "grey35",
      linewidth = 0.3
    ),
    
    axis.ticks.y = element_blank(),
    
    axis.ticks.length = unit(
      2.5,
      "pt"
    ),
    
    plot.margin = margin(
      t = 8,
      r = 8,
      b = 8,
      l = 8
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


# ============================================================
# PANEL A LABELLED
# ============================================================

p_a <- ggdraw(
  p_a_core
) +
  
  draw_plot_label(
    "a",
    x = TAG_X,
    y = TAG_Y,
    hjust = 0,
    vjust = 1,
    size = PANEL_LABEL_SIZE,
    fontface = "bold"
  )


print(
  p_a
)


# ============================================================
# SAVE EXPORTS
# ============================================================

PNG_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    ".png"
  )
)


PDF_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    ".pdf"
  )
)


RDS_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    ".rds"
  )
)


RDS_CORE_FILE <- file.path(
  OUT_DIR,
  paste0(
    FILE_STUB,
    "_core.rds"
  )
)


ggsave(
  filename = PNG_FILE,
  plot = p_a,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)


ggsave(
  filename = PDF_FILE,
  plot = p_a,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  bg = "white",
  useDingbats = FALSE
)


saveRDS(
  p_a,
  file = RDS_FILE
)


saveRDS(
  p_a_core,
  file = RDS_CORE_FILE
)


# ============================================================
# CONFIRM OUTPUTS
# ============================================================

cat(
  "\nFigure 3 panel a successfully exported.\n"
)


cat(
  "Repository root:\n",
  ROOT_DIR,
  "\n\n"
)


cat(
  "Input:\n",
  FILE,
  "\n\n"
)


cat(
  "Outputs:\n",
  PNG_FILE,
  "\n",
  PDF_FILE,
  "\n",
  RDS_FILE,
  "\n",
  RDS_CORE_FILE,
  "\n"
)


p_a