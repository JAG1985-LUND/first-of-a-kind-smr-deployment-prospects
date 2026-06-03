
# Code for the manuscript Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Reproduces Supplementary Fig. 12:
# Fitted distributions for SMR and historical construction durations.
#
# Panel a: SMR construction durations (n = 24)
# Panel b: Historical construction durations for 30-500 MW reactors
#
# Required input:
# data/Supplementary_Data/sfig12_source_data_historical_reactors_500MW.xlsx
#
# Outputs:
# outputs/Supplementary_Figures/Figure_12/sfig12_historical_reactors_500MW.png
# outputs/Supplementary_Figures/Figure_12/sfig12_historical_reactors_500MW.pdf
# outputs/Supplementary_Figures/Figure_12/sfig12_historical_reactors_500MW.rds

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(fitdistrplus)
  library(tibble)
  library(cowplot)
  library(grid)
})

rm(list = ls())
graphics.off()

# ============================================================
# SUPPLEMENTARY FIGURE 12
# FITTED DISTRIBUTIONS FOR CONSTRUCTION DURATIONS
# ============================================================

# ------------------------------------------------------------
# FIND REPOSITORY ROOT
# ------------------------------------------------------------

find_repo_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  
  repeat {
    has_git     <- dir.exists(file.path(current_dir, ".git"))
    has_data    <- dir.exists(file.path(current_dir, "data"))
    has_scripts <- dir.exists(file.path(current_dir, "scripts"))
    
    if (has_git && has_data && has_scripts) {
      return(current_dir)
    }
    
    parent_dir <- dirname(current_dir)
    
    if (identical(parent_dir, current_dir)) {
      stop(
        "Repository root not found. Run this script from inside the ",
        "first-of-a-kind-smr-deployment-prospects repository."
      )
    }
    
    current_dir <- parent_dir
  }
}

ROOT_DIR <- find_repo_root()

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

FILE_CANDIDATES <- c(
  "sfig12_source_data_historical_reactors_500MW.xlsx",
  "sfig12_historical_reactors_500MW.xlsx"
)

FILE_IN_CANDIDATES <- file.path(
  ROOT_DIR,
  "data",
  "Supplementary_Data",
  FILE_CANDIDATES
)

FILE_IN <- FILE_IN_CANDIDATES[file.exists(FILE_IN_CANDIDATES)][1]

if (is.na(FILE_IN)) {
  stop(
    "Input file not found. Checked: ",
    paste(FILE_IN_CANDIDATES, collapse = " | ")
  )
}

FILE <- basename(FILE_IN)

OUT_DIR <- file.path(
  ROOT_DIR,
  "outputs",
  "Supplementary_Figures",
  "Figure_12"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

FILE_STUB <- "sfig12_historical_reactors_500MW"

# ------------------------------------------------------------
# SETTINGS
# ------------------------------------------------------------

SAVE_W_MM <- 180
SAVE_H_MM <- 105
SAVE_DPI  <- 600
BASE_SIZE <- 7

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_names_local <- function(x) {
  x <- trimws(x)
  x <- gsub("[[:space:][:punct:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  tolower(x)
}

safe_fit <- function(x, dist) {
  tryCatch(
    fitdist(x, dist),
    error = function(e) tryCatch(
      fitdist(x, dist, method = "mme"),
      error = function(e) NULL
    )
  )
}

build_density_df <- function(years) {
  fits <- list(
    Gamma     = safe_fit(years, "gamma"),
    Lognormal = safe_fit(years, "lnorm"),
    Normal    = safe_fit(years, "norm"),
    Weibull   = safe_fit(years, "weibull")
  )
  
  fits <- Filter(Negate(is.null), fits)
  
  if (length(fits) == 0) {
    stop("No fitted distributions could be estimated.")
  }
  
  aic_tbl <- tibble(
    Distribution = names(fits),
    AIC = sapply(fits, function(f) f$aic)
  ) %>%
    arrange(AIC)
  
  best_dist <- aic_tbl$Distribution[1]
  
  x_grid <- seq(
    min(years),
    max(years),
    length.out = 1000
  )
  
  dens_list <- list()
  
  if ("Gamma" %in% names(fits)) {
    dens_list[["Gamma"]] <- tibble(
      x = x_grid,
      Density = dgamma(
        x_grid,
        shape = fits$Gamma$estimate["shape"],
        rate  = fits$Gamma$estimate["rate"]
      ),
      Distribution = "Gamma"
    )
  }
  
  if ("Lognormal" %in% names(fits)) {
    dens_list[["Lognormal"]] <- tibble(
      x = x_grid,
      Density = dlnorm(
        x_grid,
        meanlog = fits$Lognormal$estimate["meanlog"],
        sdlog   = fits$Lognormal$estimate["sdlog"]
      ),
      Distribution = "Lognormal"
    )
  }
  
  if ("Normal" %in% names(fits)) {
    dens_list[["Normal"]] <- tibble(
      x = x_grid,
      Density = dnorm(
        x_grid,
        mean = fits$Normal$estimate["mean"],
        sd   = fits$Normal$estimate["sd"]
      ),
      Distribution = "Normal"
    )
  }
  
  if ("Weibull" %in% names(fits)) {
    dens_list[["Weibull"]] <- tibble(
      x = x_grid,
      Density = dweibull(
        x_grid,
        shape = fits$Weibull$estimate["shape"],
        scale = fits$Weibull$estimate["scale"]
      ),
      Distribution = "Weibull"
    )
  }
  
  dens_df <- bind_rows(dens_list) %>%
    mutate(
      Distribution = ifelse(
        Distribution == best_dist,
        paste0(Distribution, " (best AIC)"),
        Distribution
      )
    )
  
  legend_order <- c(
    "Gamma",
    "Gamma (best AIC)",
    "Lognormal",
    "Lognormal (best AIC)",
    "Normal",
    "Normal (best AIC)",
    "Weibull",
    "Weibull (best AIC)"
  )
  
  dens_df$Distribution <- factor(
    dens_df$Distribution,
    levels = legend_order[legend_order %in% unique(dens_df$Distribution)]
  )
  
  dens_df
}

extract_numeric_vector <- function(df) {
  names(df) <- clean_names_local(names(df))
  
  candidate_names <- names(df)[
    grepl("year|yrs|duration|construction", names(df), ignore.case = TRUE)
  ]
  
  if (length(candidate_names) == 0) {
    return(NULL)
  }
  
  for (nm in candidate_names) {
    x <- suppressWarnings(as.numeric(df[[nm]]))
    x <- x[is.finite(x) & x > 0]
    
    if (length(x) > 0) {
      return(x)
    }
  }
  
  NULL
}

read_panel_b_years <- function(file_in) {
  sheets <- excel_sheets(file_in)
  
  for (sh in sheets) {
    dat <- read_excel(
      file_in,
      sheet = sh
    )
    
    x <- extract_numeric_vector(dat)
    
    if (!is.null(x) && length(x) >= 80) {
      return(x)
    }
  }
  
  dat <- read_excel(
    file_in,
    sheet = 1
  )
  
  x <- extract_numeric_vector(dat)
  
  if (is.null(x) || length(x) == 0) {
    stop(
      "Could not identify historical-construction durations in ",
      basename(file_in)
    )
  }
  
  x
}

base_theme <- theme_minimal(base_size = BASE_SIZE) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(colour = "grey35", linewidth = 0.3),
    axis.line.y = element_line(colour = "grey35", linewidth = 0.3),
    axis.ticks  = element_line(colour = "grey35", linewidth = 0.3),
    axis.text   = element_text(size = BASE_SIZE, colour = "grey20"),
    axis.title  = element_text(size = BASE_SIZE),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

dist_cols <- c(
  "Gamma"                = "forestgreen",
  "Gamma (best AIC)"    = "forestgreen",
  "Lognormal"            = "blue",
  "Lognormal (best AIC)" = "blue",
  "Normal"               = "#A020F0",
  "Normal (best AIC)"    = "#A020F0",
  "Weibull"              = "red",
  "Weibull (best AIC)"   = "red"
)

tag_panel <- function(p, tag) {
  ggdraw(p) +
    draw_plot_label(
      label = tag,
      x = 0.01,
      y = 0.99,
      hjust = 0,
      vjust = 1,
      size = 11,
      fontface = "bold"
    )
}

# ------------------------------------------------------------
# PANEL A DATA
# ------------------------------------------------------------

years_a <- c(
  3, 3.5, 4, 4, 4, 4, 4, 4, 4.5, 4.6,
  5, 5, 5, 5, 5, 5, 5.5, 5.5, 5.5, 6, 6, 6, 6, 9
)

years_a <- as.numeric(years_a)
years_a <- years_a[is.finite(years_a) & years_a > 0]

# ------------------------------------------------------------
# PANEL B DATA
# ------------------------------------------------------------

years_b <- read_panel_b_years(FILE_IN)
years_b <- years_b[is.finite(years_b) & years_b > 0]

# ------------------------------------------------------------
# DENSITIES
# ------------------------------------------------------------

dens_a <- build_density_df(years_a)
dens_b <- build_density_df(years_b)

# ------------------------------------------------------------
# PANEL A
# ------------------------------------------------------------

p_a_core <- ggplot() +
  geom_histogram(
    data = tibble(years = years_a),
    aes(x = years, y = after_stat(density)),
    binwidth = 1,
    boundary = 0,
    closed = "right",
    fill = "grey85",
    colour = "black",
    linewidth = 0.3
  ) +
  geom_line(
    data = dens_a,
    aes(x = x, y = Density, colour = Distribution),
    linewidth = 1.1
  ) +
  scale_colour_manual(
    values = dist_cols,
    name = "Fitted distribution"
  ) +
  scale_x_continuous(
    breaks = c(3, 5, 7, 9),
    limits = c(3, 9),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.4, 0.1),
    limits = c(0, 0.4),
    expand = c(0, 0)
  ) +
  labs(
    x = "Years",
    y = "Density"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    plot.margin = margin(4, 6, 4, 6)
  )

# ------------------------------------------------------------
# PANEL B
# ------------------------------------------------------------

p_b_core <- ggplot() +
  geom_histogram(
    data = tibble(years = years_b),
    aes(x = years, y = after_stat(density)),
    bins = 12,
    fill = "grey85",
    colour = "black",
    linewidth = 0.3
  ) +
  geom_line(
    data = dens_b,
    aes(x = x, y = Density, colour = Distribution),
    linewidth = 1.1
  ) +
  scale_colour_manual(
    values = dist_cols,
    name = "Fitted distribution"
  ) +
  scale_x_continuous(
    breaks = c(2.5, 5.0, 7.5, 10.0),
    limits = c(2.5, 11.0),
    expand = c(0, 0)
  ) +
  labs(
    x = "Years",
    y = "Density"
  ) +
  base_theme +
  theme(
    legend.position = "none",
    plot.margin = margin(4, 6, 4, 6)
  )

# ------------------------------------------------------------
# SHARED LEGEND
# ------------------------------------------------------------

legend_plot <- ggplot() +
  geom_line(
    data = dens_b,
    aes(x = x, y = Density, colour = Distribution),
    linewidth = 1.1
  ) +
  scale_colour_manual(
    values = dist_cols,
    name = "Fitted distribution"
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left"
    )
  ) +
  theme_void(base_size = BASE_SIZE) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = BASE_SIZE),
    legend.text = element_text(size = BASE_SIZE),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

shared_legend <- get_legend(legend_plot)

# ------------------------------------------------------------
# COMBINE
# ------------------------------------------------------------

panels_ab <- plot_grid(
  tag_panel(p_a_core, "a"),
  tag_panel(p_b_core, "b"),
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

final_plot <- plot_grid(
  panels_ab,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.11)
)

print(final_plot)

# ============================================================
# SAVE SUPPLEMENTARY FIGURE 12
# ============================================================

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".png")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  dpi = SAVE_DPI,
  bg = "white"
)

ggsave(
  filename = file.path(OUT_DIR, paste0(FILE_STUB, ".pdf")),
  plot = final_plot,
  width = SAVE_W_MM,
  height = SAVE_H_MM,
  units = "mm",
  device = grDevices::cairo_pdf,
  bg = "white"
)

saveRDS(
  object = final_plot,
  file = file.path(OUT_DIR, paste0(FILE_STUB, ".rds"))
)

final_plot