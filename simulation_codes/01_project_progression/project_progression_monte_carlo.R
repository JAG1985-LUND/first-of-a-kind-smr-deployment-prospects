# ============================================================
# PROJECT PROGRESSION MONTE CARLO
# FULL CLEAN SCRIPT — GITHUB / SUBMISSION VERSION
# LOGNORMAL EVERYWHERE
#
# Code for the manuscript:
# Gordon, J.A., Kazlou, T., Jewell, J. and Cherp, A.
# First-of-a-Kind deployment prospects for small modular reactors
#
# Module:
# 01_project_progression
#
# Required input workbook:
# project_progression_inputs.xlsx
#
# Required sheets:
# 1) smr_design_inputs
# 2) phase_rules
#
# Clean outputs:
# 01_project_progression_outputs/project_progression_output_final/
#
# Files written:
# 1) project_progression_output_final.xlsx
# 2) project_progression_output_bundle_final.rds
# 3) README.md
#
# Excel workbook contains:
# 1) baseline_draws
# 2) accelerated_draws
# 3) summary_quantiles
# 4) summary_descriptives
# 5) run_metadata
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(writexl)
})

rm(list = ls())
graphics.off()

# -------------------- PATHS --------------------

P_INPUTS <- "project_progression_inputs.xlsx"

OUTDIR <- file.path(
  "01_project_progression_outputs",
  "project_progression_output_final"
)

if (!file.exists(P_INPUTS)) {
  stop("Input workbook not found: ", P_INPUTS, call. = FALSE)
}

if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive = TRUE)
}

# -------------------- GLOBALS --------------------

RUN_TYPE   <- "final"
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 10000L
SEED       <- 42

set.seed(SEED)

run_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

required_sheets <- c("smr_design_inputs", "phase_rules")
available_sheets <- readxl::excel_sheets(P_INPUTS)

missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Missing required sheet(s): ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

# -------------------- STANDARDISERS --------------------

as_phase <- function(x) {
  str_to_lower(str_replace_all(str_trim(as.character(x)), "[ -]", "_"))
}

as_scen <- function(x) {
  str_to_lower(str_trim(as.character(x)))
}

norm_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, str_trim(x))
  x <- ifelse(tolower(x) %in% c("na", "n/a", "", "null"), NA_character_, x)
  x
}

key_ps <- function(phase, scenario) {
  paste0(phase, "__", scenario)
}

extract_year <- function(x) {
  y <- suppressWarnings(as.integer(str_extract(as.character(x), "\\d{4}")))
  if (is.na(y)) NA_integer_ else y
}

safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}

safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

# -------------------- LOAD DESIGN INPUTS --------------------

smr_specs <- readxl::read_excel(P_INPUTS, sheet = "smr_design_inputs") %>%
  dplyr::transmute(
    design = as.character(design),
    phase  = as_phase(phase),
    country = as.character(country),
    region  = as.character(region),
    oecd = as.logical(oecd),
    national_champion = as.logical(national_champion),
    size = suppressWarnings(as.numeric(size)),
    cooling_type = as.character(cooling_type),
    water_cooled = as.logical(water_cooled),
    spectrum = as.character(spectrum),
    uranium_enrichment = as.character(uranium_enrichment),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    bd_start = suppressWarnings(as.numeric(bd_start)),
    dd_start = suppressWarnings(as.numeric(dd_start)),
    licensing_start = suppressWarnings(as.numeric(licensing_start)),
    construction_start = suppressWarnings(as.numeric(construction_start)),
    operational_start = suppressWarnings(as.numeric(operational_start))
  )

stopifnot(nrow(smr_specs) == dplyr::n_distinct(smr_specs$design))

# -------------------- LOAD PHASE RULES --------------------

phase_rules <- readxl::read_excel(P_INPUTS, sheet = "phase_rules") %>%
  dplyr::transmute(
    design = as.character(design),
    baseline_condition = norm_na(baseline_condition),
    accelerated_condition = norm_na(accelerated_condition),
    phase_rule_baseline_licensing = norm_na(phase_rule_baseline_licensing),
    phase_rule_accelerated_licensing = suppressWarnings(as.numeric(phase_rule_accelerated_licensing))
  )

smr_master <- smr_specs %>%
  dplyr::left_join(phase_rules, by = "design")

# -------------------- PHASE PARAMETERS --------------------
# conceptual/basic/detailed mean/sd are arithmetic mean/sd.
# licensing/construction mean/sd are log-space meanlog/sdlog.

phase_params <- tibble::tribble(
  ~phase,              ~scenario,       ~mean,     ~sd,     ~min,  ~max,
  "conceptual_design", "baseline",       6.66,     3.46,    2.0,   15.0,
  "conceptual_design", "accelerated",    5.11,     1.95,    2.0,   10.0,
  "basic_design",      "baseline",       4.76,     3.10,    2.0,   14.0,
  "basic_design",      "accelerated",    3.39,     1.03,    2.0,   5.0,
  "detailed_design",   "baseline",       4.16,     2.09,    2.0,   9.0,
  "detailed_design",   "accelerated",    3.36,     1.15,    2.0,   5.0,
  "licensing",         "baseline",       1.46879,  0.2867,  2.0,   8.0,
  "licensing",         "accelerated",    0.8405,   0.1432,  1.0,   3.0,
  "construction",      "baseline",       1.64149,  0.2064,  3.0,   7.5,
  "construction",      "accelerated",    1.3768,   0.1242,  3.0,   5.0
) %>%
  dplyr::mutate(
    phase = as_phase(phase),
    scenario = as_scen(scenario)
  )

param_map <- phase_params %>%
  dplyr::mutate(key = key_ps(phase, scenario)) %>%
  split(.$key) %>%
  purrr::map(
    ~ list(
      mean = .x$mean[[1]],
      sd   = .x$sd[[1]],
      min  = .x$min[[1]],
      max  = .x$max[[1]]
    )
  )

get_p <- function(phase, scenario) {
  k <- key_ps(as_phase(phase), as_scen(scenario))
  
  if (!k %in% names(param_map)) {
    stop("Missing phase params for: ", k, call. = FALSE)
  }
  
  param_map[[k]]
}

# -------------------- DISTRIBUTION HELPERS --------------------

lognorm_from_mean_sd <- function(mean, sd) {
  v <- log(1 + (sd^2) / (mean^2))
  list(
    meanlog = log(mean) - 0.5 * v,
    sdlog = sqrt(v)
  )
}

r_lnorm_trunc_cond_total <- function(elapsed, a, b, meanlog, sdlog) {
  elapsed <- max(0, elapsed)
  lo <- max(a, elapsed)
  
  if (lo >= b) return(lo)
  
  Fa <- plnorm(lo, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  
  if (is.na(Fa) || is.na(Fb) || Fb <= Fa) return(lo)
  
  qlnorm(runif(1, Fa, Fb), meanlog, sdlog)
}

draw_total_duration <- function(phase, scenario, elapsed) {
  phase <- as_phase(phase)
  scenario <- as_scen(scenario)
  p <- get_p(phase, scenario)
  
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  
  if (phase %in% c("licensing", "construction")) {
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

# -------------------- CONDITION OVERRIDES --------------------

apply_next_override <- function(next_phase, cond) {
  cond <- norm_na(cond)
  
  if (is.na(cond)) return(NA_real_)
  
  if (next_phase == "basic_design" && cond == "force_bd_start_2026") {
    return(2026.0)
  }
  
  if (next_phase == "detailed_design" && cond == "force_dd_start_2026") {
    return(2026.0)
  }
  
  if (next_phase == "construction" && grepl("^construction_start_\\d{4}$", cond)) {
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  if (next_phase == "operational" && grepl("^force_operational_\\d{4}$", cond)) {
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  NA_real_
}

# -------------------- CN/RU LICENSING RULE --------------------

licensing_duration_with_rules <- function(row, scenario, elapsed) {
  scenario <- as_scen(scenario)
  ctry <- as.character(row$country)
  
  base_rule <- norm_na(row$phase_rule_baseline_licensing)
  acc_rule  <- suppressWarnings(as.numeric(row$phase_rule_accelerated_licensing))
  
  is_cnru <- ctry %in% c("CN", "RU")
  
  if (scenario == "accelerated") {
    if (is_cnru || (!is.na(acc_rule) && acc_rule == 1)) {
      return(1.0)
    }
    
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  if (is_cnru) {
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  if (!is.na(base_rule) && as_scen(base_rule) == "accelerated") {
    return(draw_total_duration("licensing", "accelerated", elapsed))
  }
  
  draw_total_duration("licensing", "baseline", elapsed)
}

# -------------------- SIMULATE ONE DESIGN TO FOAK --------------------

simulate_to_foak <- function(row, scenario) {
  scenario <- as_scen(scenario)
  
  phases <- c(
    "conceptual_design",
    "basic_design",
    "detailed_design",
    "licensing",
    "construction",
    "operational"
  )
  
  cur <- row$phase
  
  if (is.na(cur) || !cur %in% phases) {
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "bad_phase",
        cd_start = NA_real_,
        bd_start = NA_real_,
        dd_start = NA_real_,
        licensing_start = NA_real_,
        construction_start = NA_real_,
        operational_start = NA_real_,
        foak_year = NA_real_
      )
    )
  }
  
  cond <- if (scenario == "baseline") {
    row$baseline_condition
  } else {
    row$accelerated_condition
  }
  
  cond <- norm_na(cond)
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  cd_e <- bd_e <- dd_e <- lic_e <- con_e <- NA_real_
  
  if (cur == "operational") {
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "already_operational",
        cd_start = cd_s,
        bd_start = bd_s,
        dd_start = dd_s,
        licensing_start = lic_s,
        construction_start = con_s,
        operational_start = op_s,
        foak_year = op_s
      )
    )
  }
  
  if (row$design == "CAREM" && cur == "construction") {
    p_con <- get_p("construction", scenario)
    
    total_con <- r_lnorm_trunc_cond_total(
      elapsed = 0,
      a = p_con$min,
      b = p_con$max,
      meanlog = p_con$mean,
      sdlog = p_con$sd
    )
    
    frac <- if (scenario == "accelerated") {
      runif(1, 0.40, 0.50)
    } else {
      runif(1, 0.30, 0.40)
    }
    
    remain <- (1 - frac) * total_con
    
    con_s <- 2026.0
    con_e <- 2026.0 + remain
    op_s  <- con_e
    
    return(
      tibble(
        design = row$design,
        scenario = scenario,
        status = "simulated",
        cd_start = cd_s,
        bd_start = bd_s,
        dd_start = dd_s,
        licensing_start = lic_s,
        construction_start = con_s,
        operational_start = op_s,
        foak_year = op_s
      )
    )
  }
  
  get_start <- function(phase) {
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design")      return(bd_s)
    if (phase == "detailed_design")   return(dd_s)
    if (phase == "licensing")         return(lic_s)
    if (phase == "construction")      return(con_s)
    if (phase == "operational")       return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val) {
    if (phase == "basic_design")      bd_s  <<- val
    if (phase == "detailed_design")   dd_s  <<- val
    if (phase == "licensing")         lic_s <<- val
    if (phase == "construction")      con_s <<- val
    if (phase == "operational")       op_s  <<- val
  }
  
  set_end <- function(phase, val) {
    if (phase == "conceptual_design") cd_e  <<- val
    if (phase == "basic_design")      bd_e  <<- val
    if (phase == "detailed_design")   dd_e  <<- val
    if (phase == "licensing")         lic_e <<- val
    if (phase == "construction")      con_e <<- val
  }
  
  i0 <- match(cur, phases)
  
  for (i in i0:(length(phases) - 1)) {
    ph <- phases[i]
    st <- get_start(ph)
    
    if (is.na(st)) {
      st <- ifelse(ph == cur, CUT_YEAR, NEXT_FLOOR)
    }
    
    elapsed <- if (ph == cur && st <= CUT_YEAR) {
      max(0, CUT_YEAR - st + 1)
    } else {
      0
    }
    
    if (ph == "licensing") {
      tot <- licensing_duration_with_rules(row, scenario, elapsed)
    } else {
      tot <- draw_total_duration(ph, scenario, elapsed)
    }
    
    en <- st + tot
    
    if (elapsed > 0 && en < CUT_YEAR) {
      en <- CUT_YEAR
    }
    
    nxt <- phases[i + 1]
    nxt_start <- max(NEXT_FLOOR, en)
    
    ov <- apply_next_override(nxt, cond)
    
    if (!is.na(ov)) {
      nxt_start <- ov
      en <- nxt_start
    }
    
    set_end(ph, en)
    
    if (is.na(get_start(nxt))) {
      set_start(nxt, nxt_start)
    }
    
    if (nxt == "operational") {
      ov2 <- apply_next_override("operational", cond)
      
      if (!is.na(ov2)) {
        nxt_start <- ov2
        en <- nxt_start
        set_end("construction", en)
      }
      
      op_s <- nxt_start
      break
    }
  }
  
  tibble(
    design = row$design,
    scenario = scenario,
    status = "simulated",
    cd_start = cd_s,
    bd_start = bd_s,
    dd_start = dd_s,
    licensing_start = lic_s,
    construction_start = con_s,
    operational_start = op_s,
    foak_year = op_s
  )
}

# -------------------- MONTE CARLO RUNNER --------------------

design_list <- split(smr_master, smr_master$design)

run_mc <- function(scenario, sims) {
  purrr::map_dfr(seq_len(sims), function(s) {
    purrr::map_dfr(design_list, ~ simulate_to_foak(.x[1, ], scenario)) %>%
      dplyr::mutate(sim = s) %>%
      dplyr::select(
        sim,
        design,
        scenario,
        status,
        cd_start,
        bd_start,
        dd_start,
        licensing_start,
        construction_start,
        operational_start,
        foak_year
      )
  })
}

# -------------------- RUN MONTE CARLO --------------------

set.seed(SEED)

mc_baseline_project_progression <- run_mc("baseline", SIMS)
mc_accelerated_project_progression <- run_mc("accelerated", SIMS)

# -------------------- SUMMARY TABLES --------------------

summ_baseline <- mc_baseline_project_progression %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "baseline")

summ_accelerated <- mc_accelerated_project_progression %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "accelerated")

summary_quantiles <- dplyr::bind_rows(
  summ_baseline,
  summ_accelerated
) %>%
  dplyr::select(design, scenario, P10, P50, P90) %>%
  dplyr::arrange(scenario, design)

make_descriptives <- function(df, scenario_name) {
  df %>%
    dplyr::group_by(design) %>%
    dplyr::summarise(
      n    = sum(is.finite(foak_year)),
      mean = safe_mean(foak_year),
      sd   = safe_sd(foak_year),
      min  = safe_min(foak_year),
      q1   = safe_q(foak_year, 0.25),
      p10  = safe_q(foak_year, 0.10),
      p50  = safe_q(foak_year, 0.50),
      q3   = safe_q(foak_year, 0.75),
      p90  = safe_q(foak_year, 0.90),
      max  = safe_max(foak_year),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = scenario_name) %>%
    dplyr::select(
      design,
      scenario,
      n,
      mean,
      sd,
      min,
      q1,
      p10,
      p50,
      q3,
      p90,
      max
    )
}

summary_descriptives <- dplyr::bind_rows(
  make_descriptives(mc_baseline_project_progression, "baseline"),
  make_descriptives(mc_accelerated_project_progression, "accelerated")
) %>%
  dplyr::arrange(scenario, design)

run_metadata <- tibble::tibble(
  item = c(
    "script_name",
    "module",
    "run_type",
    "run_timestamp",
    "seed",
    "sims",
    "cut_year",
    "next_floor",
    "working_directory",
    "input_workbook",
    "input_sheet_designs",
    "input_sheet_rules",
    "output_directory",
    "excel_output",
    "bundle_output",
    "r_version",
    "platform"
  ),
  value = c(
    "project_progression_monte_carlo.R",
    "01_project_progression",
    RUN_TYPE,
    run_timestamp,
    as.character(SEED),
    as.character(SIMS),
    as.character(CUT_YEAR),
    as.character(NEXT_FLOOR),
    getwd(),
    P_INPUTS,
    "smr_design_inputs",
    "phase_rules",
    OUTDIR,
    file.path(OUTDIR, "project_progression_output_final.xlsx"),
    file.path(OUTDIR, "project_progression_output_bundle_final.rds"),
    paste(R.version$major, R.version$minor, sep = "."),
    R.version$platform
  )
)

# -------------------- EXPORT CLEAN EXCEL OUTPUT --------------------

xlsx_path <- file.path(
  OUTDIR,
  "project_progression_output_final.xlsx"
)

xlsx_contents <- list(
  baseline_draws       = mc_baseline_project_progression,
  accelerated_draws    = mc_accelerated_project_progression,
  summary_quantiles    = summary_quantiles,
  summary_descriptives = summary_descriptives,
  run_metadata         = run_metadata
)

writexl::write_xlsx(xlsx_contents, path = xlsx_path)

# -------------------- EXPORT CLEAN BUNDLE RDS --------------------

model_bundle <- list(
  mc_baseline_project_progression = mc_baseline_project_progression,
  mc_accelerated_project_progression = mc_accelerated_project_progression,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  smr_specs = smr_specs,
  phase_rules = phase_rules,
  smr_master = smr_master,
  phase_params = phase_params,
  param_map = param_map,
  design_list = design_list,
  run_metadata = run_metadata
)

bundle_path <- file.path(
  OUTDIR,
  "project_progression_output_bundle_final.rds"
)

saveRDS(model_bundle, file = bundle_path)

# -------------------- README --------------------

readme_path <- file.path(OUTDIR, "README.md")

readme_lines <- c(
  "# Project progression Monte Carlo outputs",
  "",
  "This folder contains the clean final outputs from the project progression module.",
  "",
  "## Files",
  "",
  "- `project_progression_output_final.xlsx`: human-readable Monte Carlo output workbook.",
  "- `project_progression_output_bundle_final.rds`: compact R bundle containing raw draws, summaries, model inputs and run metadata.",
  "",
  "## Excel sheets",
  "",
  "- `baseline_draws`: design-level Monte Carlo draws under the Baseline scenario.",
  "- `accelerated_draws`: design-level Monte Carlo draws under the Accelerated scenario.",
  "- `summary_quantiles`: P10, P50 and P90 FOAK year by design and scenario.",
  "- `summary_descriptives`: descriptive statistics for FOAK year by design and scenario.",
  "- `run_metadata`: seed, simulation count, input workbook and run information.",
  "",
  "## Run settings",
  "",
  paste0("- Simulations per design: ", SIMS),
  paste0("- Seed: ", SEED),
  paste0("- Cut year: ", CUT_YEAR),
  paste0("- Next phase floor: ", NEXT_FLOOR),
  paste0("- Run timestamp: ", run_timestamp)
)

writeLines(readme_lines, readme_path)

# -------------------- CONSOLE OUTPUT --------------------

cat("\n==================== PROJECT PROGRESSION COMPLETE ====================\n")
cat("Module: 01_project_progression\n")
cat("Run type:", RUN_TYPE, "\n")
cat("Baseline rows:", nrow(mc_baseline_project_progression), "\n")
cat("Accelerated rows:", nrow(mc_accelerated_project_progression), "\n")
cat("Number of designs:", dplyr::n_distinct(smr_master$design), "\n")
cat("Simulations per design:", SIMS, "\n")
cat("Output directory:\n", OUTDIR, "\n")
cat("Excel saved:\n", xlsx_path, "\n")
cat("Bundle RDS saved:\n", bundle_path, "\n")
cat("README saved:\n", readme_path, "\n")
cat("================================================================================\n\n")

print(xlsx_path)
print(bundle_path)
print(readme_path)
