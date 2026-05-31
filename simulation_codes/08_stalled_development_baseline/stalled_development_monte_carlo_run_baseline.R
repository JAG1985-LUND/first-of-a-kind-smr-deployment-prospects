# ============================================================
# BASELINE-ONLY STALLED DEVELOPMENT PROJECT PROGRESSION MONTE CARLO
# FAIL-GATED • FULL CLEAN SCRIPT • LOGNORMAL EVERYWHERE
#
# GITHUB VERSION:
#   • 10,000 simulations for the full manuscript-scale run
#   • For a quick repository smoke test, set SIMS <- 25L
#
# INPUT WORKBOOK:
#   data/Simulation_Inputs/stalled_development_sensitivity_inputs_baseline.xlsx
#
# REQUIRED SHEETS:
#   1) stalled_development_bas_sample
#   2) phase_rules_stalled_develop_bas
#   3) design_phase_failures
#   4) failures_stalled_develop_bas
#
# OUTPUT:
#   outputs/simulation_outputs/stalled_development_baseline_sensitivity/
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(writexl)
  library(tidyr)
})

# -------------------- PATHS --------------------
P_INPUT <- "data/Simulation_Inputs/stalled_development_sensitivity_inputs_baseline.xlsx"

OUTDIR <- "outputs/simulation_outputs/stalled_development_baseline_sensitivity/"

# -------------------- GLOBALS --------------------
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 10000L
SEED       <- 42L
YEARS      <- 2026:2050
VERSION    <- "V4"

MODEL_TAG  <- "STALLED_DEVELOPMENT_BASELINE"
RUN_TAG    <- paste0("MC", SIMS)

set.seed(SEED)

if (!file.exists(P_INPUT)) {
  stop("Input workbook not found: ", P_INPUT, call. = FALSE)
}

required_sheets <- c(
  "stalled_development_bas_sample",
  "phase_rules_stalled_develop_bas",
  "design_phase_failures",
  "failures_stalled_develop_bas"
)

available_sheets <- readxl::excel_sheets(P_INPUT)
missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "Missing required sheet(s) in input workbook: ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M")

RUN_DIR <- file.path(
  OUTDIR,
  paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_", stamp)
)

LATEST_DIR <- file.path(
  OUTDIR,
  paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_latest")
)

dir.create(RUN_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LATEST_DIR, recursive = TRUE, showWarnings = FALSE)

# -------------------- STANDARDISERS --------------------
clean_names_simple <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

as_phase <- function(x) {
  stringr::str_to_lower(stringr::str_replace_all(stringr::str_trim(as.character(x)), "[ -]", "_"))
}

as_scen <- function(x) {
  stringr::str_to_lower(stringr::str_trim(as.character(x)))
}

norm_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, stringr::str_trim(x))
  x <- ifelse(tolower(x) %in% c("na", "n/a", "", "null"), NA_character_, x)
  x
}

extract_year <- function(x) {
  y <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
  if (is.na(y)) NA_integer_ else y
}

clamp01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  pmin(1, pmax(0, x))
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

as_bool_any <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- stringr::str_trim(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

# -------------------- LOAD SPECS --------------------
smr55_raw <- readxl::read_excel(P_INPUT, sheet = "stalled_development_bas_sample")
names(smr55_raw) <- clean_names_simple(names(smr55_raw))

smr55_specs <- smr55_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    phase = as_phase(phase),
    country = as.character(country),
    region = as.character(region),
    oecd = as_bool_any(oecd),
    national_champion = as_bool_any(national_champion),
    size = suppressWarnings(as.numeric(size)),
    cooling_type = as.character(cooling_type),
    water_cooled = as_bool_any(water_cooled),
    spectrum = as.character(spectrum),
    uranium_enrichment = as.character(uranium_enrichment),
    cd_start = suppressWarnings(as.numeric(cd_start)),
    bd_start = suppressWarnings(as.numeric(bd_start)),
    dd_start = suppressWarnings(as.numeric(dd_start)),
    licensing_start = suppressWarnings(as.numeric(licensing_start)),
    construction_start = suppressWarnings(as.numeric(construction_start)),
    operational_start = suppressWarnings(as.numeric(operational_start))
  )

stopifnot(nrow(smr55_specs) == dplyr::n_distinct(smr55_specs$design))

# -------------------- LOAD PHASE RULES --------------------
phase_rules_raw <- readxl::read_excel(P_INPUT, sheet = "phase_rules_stalled_develop_bas")
names(phase_rules_raw) <- clean_names_simple(names(phase_rules_raw))

phase_rules <- phase_rules_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    baseline_condition = norm_na(baseline_condition),
    phase_rule_baseline_licensing = norm_na(phase_rule_baseline_licensing)
  )

# -------------------- LOAD FAILURE RATES --------------------
fail_rates_raw <- readxl::read_excel(P_INPUT, sheet = "failures_stalled_develop_bas")
names(fail_rates_raw) <- clean_names_simple(names(fail_rates_raw))

fail_rates <- fail_rates_raw %>%
  dplyr::transmute(
    design = stringr::str_trim(as.character(design)),
    licensing_failure_baseline = clamp01(licensing_failure_baseline),
    construction_failure_baseline = clamp01(construction_failure_baseline)
  ) %>%
  dplyr::mutate(
    lic_surv_baseline = clamp01(1 - licensing_failure_baseline),
    con_surv_baseline = clamp01(1 - construction_failure_baseline)
  ) %>%
  dplyr::select(
    design,
    lic_surv_baseline,
    con_surv_baseline
  )

# -------------------- LOAD DESIGN-PHASE FAILURES --------------------
dpf_raw <- readxl::read_excel(P_INPUT, sheet = "design_phase_failures")
names(dpf_raw) <- clean_names_simple(names(dpf_raw))

dpf <- dpf_raw %>%
  dplyr::transmute(
    phase = as_phase(phase),
    scenario = as_scen(scenario),
    k = suppressWarnings(as.numeric(failures_k)),
    N = suppressWarnings(as.numeric(total_n))
  ) %>%
  dplyr::filter(scenario == "baseline") %>%
  dplyr::mutate(
    p_fail_mean = (k + 0.5) / (N + 1),
    p_surv_mean = clamp01(1 - p_fail_mean)
  )

req_phases <- c("conceptual_design", "basic_design", "detailed_design")
missing_phases <- setdiff(req_phases, unique(dpf$phase))

if (length(missing_phases) > 0) {
  stop(
    "Missing baseline phases in design_phase_failures: ",
    paste(missing_phases, collapse = ", "),
    call. = FALSE
  )
}

jeffreys_survival <- setNames(dpf$p_surv_mean, dpf$phase)

get_surv_phase <- function(phase, row) {
  phase <- as_phase(phase)
  
  if (phase %in% c("conceptual_design", "basic_design", "detailed_design")) {
    return(clamp01(jeffreys_survival[[phase]]))
  }
  
  if (phase == "licensing") {
    return(clamp01(row$lic_surv_baseline))
  }
  
  if (phase == "construction") {
    return(clamp01(row$con_surv_baseline))
  }
  
  1.0
}

# -------------------- BUILD MASTER --------------------
smr_master <- smr55_specs %>%
  dplyr::left_join(phase_rules, by = "design") %>%
  dplyr::left_join(fail_rates, by = "design")

miss_fail <- smr_master %>%
  dplyr::filter(is.na(lic_surv_baseline) | is.na(con_surv_baseline)) %>%
  dplyr::pull(design)

if (length(miss_fail) > 0) {
  stop(
    "Missing failure rates for designs: ",
    paste(unique(miss_fail), collapse = ", "),
    call. = FALSE
  )
}

# -------------------- PHASE PARAMETERS --------------------
phase_params <- tibble::tribble(
  ~phase,              ~mean,     ~sd,     ~min,  ~max,
  "conceptual_design",  6.66,     3.46,    2.0,   15.0,
  "basic_design",       4.76,     3.10,    2.0,   14.0,
  "detailed_design",    4.16,     2.09,    2.0,   9.0,
  "licensing",          1.46879,  0.2867,  2.0,   8.0,
  "construction",       1.64149,  0.2064,  3.0,   7.5
) %>%
  dplyr::mutate(phase = as_phase(phase))

param_map <- phase_params %>%
  split(.$phase) %>%
  purrr::map(~ list(
    mean = .x$mean[[1]],
    sd = .x$sd[[1]],
    min = .x$min[[1]],
    max = .x$max[[1]]
  ))

licensing_fasttrack_params <- list(
  mean = 0.8405,
  sd   = 0.1432,
  min  = 1.0,
  max  = 3.0
)

get_p <- function(phase) {
  phase <- as_phase(phase)
  if (!phase %in% names(param_map)) {
    stop("Missing phase params for: ", phase, call. = FALSE)
  }
  param_map[[phase]]
}

# -------------------- DISTRIBUTION HELPERS --------------------
lognorm_from_mean_sd <- function(mean, sd) {
  v <- log(1 + (sd^2) / (mean^2))
  list(meanlog = log(mean) - 0.5 * v, sdlog = sqrt(v))
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

draw_total_duration <- function(phase, elapsed) {
  phase <- as_phase(phase)
  p <- get_p(phase)
  
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  
  if (phase %in% c("licensing", "construction")) {
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

draw_fasttrack_licensing <- function(elapsed) {
  p <- licensing_fasttrack_params
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd)
}

# -------------------- CONDITION OVERRIDES --------------------
apply_next_override <- function(next_phase, cond) {
  cond <- norm_na(cond)
  if (is.na(cond)) return(NA_real_)
  
  if (next_phase == "basic_design" && cond == "force_bd_start_2026") return(2026.0)
  if (next_phase == "detailed_design" && cond == "force_dd_start_2026") return(2026.0)
  
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

# -------------------- BASELINE LICENSING RULE --------------------
licensing_duration_with_rules <- function(row, elapsed) {
  ctry <- as.character(row$country)
  base_rule <- norm_na(row$phase_rule_baseline_licensing)
  
  is_cnru <- ctry %in% c("CN", "RU")
  
  if (is_cnru) return(draw_fasttrack_licensing(elapsed))
  if (!is.na(base_rule) && stringr::str_to_lower(base_rule) == "accelerated") {
    return(draw_fasttrack_licensing(elapsed))
  }
  
  draw_total_duration("licensing", elapsed)
}

# -------------------- LICENSING FAILURE EXEMPTIONS --------------------
licensing_failure_exempt_designs <- c("BWRX-300")

# -------------------- SIMULATE ONE DESIGN TO FOAK --------------------
simulate_to_foak <- function(row) {
  
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
    return(tibble::tibble(
      design = row$design,
      status = "bad_phase",
      fail_phase = NA_character_,
      cd_start = row$cd_start,
      bd_start = row$bd_start,
      dd_start = row$dd_start,
      licensing_start = row$licensing_start,
      construction_start = row$construction_start,
      operational_start = row$operational_start,
      foak_year = NA_real_
    ))
  }
  
  cond <- norm_na(row$baseline_condition)
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  cd_e <- bd_e <- dd_e <- lic_e <- con_e <- NA_real_
  
  if (cur == "operational") {
    return(tibble::tibble(
      design = row$design,
      status = "already_operational",
      fail_phase = NA_character_,
      cd_start = cd_s,
      bd_start = bd_s,
      dd_start = dd_s,
      licensing_start = lic_s,
      construction_start = con_s,
      operational_start = op_s,
      foak_year = op_s
    ))
  }
  
  if (row$design == "CAREM" && cur == "construction") {
    p_con <- get_p("construction")
    total_con <- r_lnorm_trunc_cond_total(0, p_con$min, p_con$max, p_con$mean, p_con$sd)
    frac <- runif(1, 0.30, 0.40)
    remain <- (1 - frac) * total_con
    
    con_s <- 2026.0
    con_e <- 2026.0 + remain
    op_s  <- con_e
    
    ov_op <- apply_next_override("operational", cond)
    forced_op <- !is.na(ov_op)
    
    if (forced_op) {
      op_s  <- ov_op
      con_e <- op_s
    }
    
    if (!forced_op) {
      p_surv_con <- get_surv_phase("construction", row)
      if (runif(1) > p_surv_con) {
        return(tibble::tibble(
          design = row$design,
          status = "failed_construction",
          fail_phase = "construction",
          cd_start = cd_s,
          bd_start = bd_s,
          dd_start = dd_s,
          licensing_start = lic_s,
          construction_start = con_s,
          operational_start = NA_real_,
          foak_year = NA_real_
        ))
      }
    }
    
    return(tibble::tibble(
      design = row$design,
      status = "simulated",
      fail_phase = NA_character_,
      cd_start = cd_s,
      bd_start = bd_s,
      dd_start = dd_s,
      licensing_start = lic_s,
      construction_start = con_s,
      operational_start = op_s,
      foak_year = op_s
    ))
  }
  
  get_start <- function(phase) {
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design") return(bd_s)
    if (phase == "detailed_design") return(dd_s)
    if (phase == "licensing") return(lic_s)
    if (phase == "construction") return(con_s)
    if (phase == "operational") return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val) {
    if (phase == "basic_design") bd_s <<- val
    if (phase == "detailed_design") dd_s <<- val
    if (phase == "licensing") lic_s <<- val
    if (phase == "construction") con_s <<- val
    if (phase == "operational") op_s <<- val
  }
  
  set_end <- function(phase, val) {
    if (phase == "conceptual_design") cd_e <<- val
    if (phase == "basic_design") bd_e <<- val
    if (phase == "detailed_design") dd_e <<- val
    if (phase == "licensing") lic_e <<- val
    if (phase == "construction") con_e <<- val
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
      
      tot <- licensing_duration_with_rules(row, elapsed)
      en_draw <- st + tot
      
      if (elapsed > 0 && en_draw < CUT_YEAR) en_draw <- CUT_YEAR
      
      lic_exempt <- row$design %in% licensing_failure_exempt_designs
      
      if (!lic_exempt) {
        p_surv <- get_surv_phase("licensing", row)
        if (runif(1) > p_surv) {
          return(tibble::tibble(
            design = row$design,
            status = "failed_licensing",
            fail_phase = "licensing",
            cd_start = cd_s,
            bd_start = bd_s,
            dd_start = dd_s,
            licensing_start = lic_s,
            construction_start = con_s,
            operational_start = NA_real_,
            foak_year = NA_real_
          ))
        }
      }
      
      nxt <- "construction"
      nxt_start <- max(NEXT_FLOOR, en_draw)
      
      ov <- apply_next_override(nxt, cond)
      if (!is.na(ov)) nxt_start <- ov
      
      lic_e <- nxt_start
      con_s <- nxt_start
      
      next
    }
    
    tot <- draw_total_duration(ph, elapsed)
    en <- st + tot
    
    if (elapsed > 0 && en < CUT_YEAR) en <- CUT_YEAR
    
    nxt <- phases[i + 1]
    nxt_start <- max(NEXT_FLOOR, en)
    
    ov <- apply_next_override(nxt, cond)
    forced_transition <- !is.na(ov)
    
    if (forced_transition) {
      nxt_start <- ov
      en <- nxt_start
    }
    
    set_end(ph, en)
    
    skip_failure <- forced_transition
    
    if (!skip_failure) {
      p_surv <- get_surv_phase(ph, row)
      if (runif(1) > p_surv) {
        return(tibble::tibble(
          design = row$design,
          status = paste0("failed_", ph),
          fail_phase = ph,
          cd_start = cd_s,
          bd_start = bd_s,
          dd_start = dd_s,
          licensing_start = lic_s,
          construction_start = con_s,
          operational_start = NA_real_,
          foak_year = NA_real_
        ))
      }
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
    
    if (is.na(get_start(nxt))) set_start(nxt, nxt_start)
  }
  
  tibble::tibble(
    design = row$design,
    status = "simulated",
    fail_phase = NA_character_,
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

run_mc <- function(sims) {
  purrr::map_dfr(seq_len(sims), function(s) {
    purrr::map_dfr(design_list, ~ simulate_to_foak(.x[1, ])) %>%
      dplyr::mutate(sim = s) %>%
      dplyr::select(
        sim,
        design,
        status,
        fail_phase,
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

mc_baseline <- run_mc(SIMS)

# -------------------- SUMMARY TABLES --------------------
summary_quantiles <- mc_baseline %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    scenario = "baseline",
    n_total = dplyr::n(),
    n_success = sum(!is.na(foak_year)),
    success_rate = n_success / n_total,
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::select(
    design,
    scenario,
    n_total,
    n_success,
    success_rate,
    P10,
    P50,
    P90
  ) %>%
  dplyr::arrange(design)

summary_descriptives <- mc_baseline %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    scenario = "baseline",
    n = sum(is.finite(foak_year)),
    mean = safe_mean(foak_year),
    sd = safe_sd(foak_year),
    min = safe_min(foak_year),
    q1 = safe_q(foak_year, 0.25),
    p10 = safe_q(foak_year, 0.10),
    p50 = safe_q(foak_year, 0.50),
    q3 = safe_q(foak_year, 0.75),
    p90 = safe_q(foak_year, 0.90),
    max = safe_max(foak_year),
    .groups = "drop"
  ) %>%
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
  ) %>%
  dplyr::arrange(design)

run_metadata <- tibble::tibble(
  item = c(
    "timestamp",
    "version",
    "seed",
    "sims",
    "cut_year",
    "next_floor",
    "input_workbook",
    "sheet_stalled_development_bas_sample",
    "sheet_phase_rules_stalled_develop_bas",
    "sheet_design_phase_failures",
    "sheet_failures_stalled_develop_bas",
    "run_directory",
    "latest_directory",
    "model_type",
    "repository_note"
  ),
  value = c(
    stamp,
    VERSION,
    as.character(SEED),
    as.character(SIMS),
    as.character(CUT_YEAR),
    as.character(NEXT_FLOOR),
    P_INPUT,
    "stalled_development_bas_sample",
    "phase_rules_stalled_develop_bas",
    "design_phase_failures",
    "failures_stalled_develop_bas",
    RUN_DIR,
    LATEST_DIR,
    MODEL_TAG,
    "For a quick repository smoke test, set SIMS <- 25L; the manuscript-scale run uses SIMS <- 10000L."
  )
)

# -------------------- EXPORT EXCEL --------------------
xlsx_path_run <- file.path(
  RUN_DIR,
  paste0("SMR55_ProjectProgression_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_", stamp, ".xlsx")
)

xlsx_path_latest <- file.path(
  LATEST_DIR,
  paste0("SMR55_ProjectProgression_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_latest.xlsx")
)

xlsx_contents <- list(
  baseline_draws = mc_baseline,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  run_metadata = run_metadata
)

writexl::write_xlsx(xlsx_contents, path = xlsx_path_run)
writexl::write_xlsx(xlsx_contents, path = xlsx_path_latest)

# -------------------- SAVE R OBJECTS --------------------
saveRDS(mc_baseline, file = file.path(RUN_DIR, paste0("mc_baseline_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))
saveRDS(summary_quantiles, file = file.path(RUN_DIR, paste0("summary_quantiles_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))
saveRDS(summary_descriptives, file = file.path(RUN_DIR, paste0("summary_descriptives_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))
saveRDS(smr_master, file = file.path(RUN_DIR, paste0("smr_master_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))
saveRDS(phase_params, file = file.path(RUN_DIR, paste0("phase_params_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))
saveRDS(run_metadata, file = file.path(RUN_DIR, paste0("run_metadata_", MODEL_TAG, "_", VERSION, "_", stamp, ".rds")))

saveRDS(mc_baseline, file = file.path(LATEST_DIR, paste0("mc_baseline_", MODEL_TAG, "_", VERSION, "_latest.rds")))
saveRDS(summary_quantiles, file = file.path(LATEST_DIR, paste0("summary_quantiles_", MODEL_TAG, "_", VERSION, "_latest.rds")))
saveRDS(summary_descriptives, file = file.path(LATEST_DIR, paste0("summary_descriptives_", MODEL_TAG, "_", VERSION, "_latest.rds")))
saveRDS(smr_master, file = file.path(LATEST_DIR, paste0("smr_master_", MODEL_TAG, "_", VERSION, "_latest.rds")))
saveRDS(phase_params, file = file.path(LATEST_DIR, paste0("phase_params_", MODEL_TAG, "_", VERSION, "_latest.rds")))
saveRDS(run_metadata, file = file.path(LATEST_DIR, paste0("run_metadata_", MODEL_TAG, "_", VERSION, "_latest.rds")))

model_bundle <- list(
  mc_baseline = mc_baseline,
  summary_quantiles = summary_quantiles,
  summary_descriptives = summary_descriptives,
  smr_master = smr_master,
  smr55_specs = smr55_specs,
  phase_rules = phase_rules,
  fail_rates = fail_rates,
  dpf = dpf,
  phase_params = phase_params,
  licensing_fasttrack_params = licensing_fasttrack_params,
  param_map = param_map,
  design_list = design_list,
  run_metadata = run_metadata
)

saveRDS(
  model_bundle,
  file = file.path(RUN_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_bundle_", stamp, ".rds"))
)

saveRDS(
  model_bundle,
  file = file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_bundle_latest.rds"))
)

save(
  mc_baseline,
  summary_quantiles,
  summary_descriptives,
  smr55_specs,
  phase_rules,
  fail_rates,
  dpf,
  smr_master,
  phase_params,
  licensing_fasttrack_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(RUN_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_workspace_", stamp, ".RData"))
)

save(
  mc_baseline,
  summary_quantiles,
  summary_descriptives,
  smr55_specs,
  phase_rules,
  fail_rates,
  dpf,
  smr_master,
  phase_params,
  licensing_fasttrack_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_workspace_latest.RData"))
)

# -------------------- POST-RUN SUMMARY SWEEP --------------------
mc_baseline_full <- mc_baseline %>%
  dplyr::left_join(smr55_specs, by = "design")

design_summary <- summary_quantiles

build_ts <- function(mc_obj, grouping_vars = NULL) {
  
  df <- mc_obj %>%
    dplyr::mutate(
      foak_year_int = floor(foak_year),
      success = !is.na(foak_year_int)
    )
  
  results <- lapply(YEARS, function(y) {
    
    tmp <- df %>%
      dplyr::filter(success, foak_year_int <= y)
    
    if (!is.null(grouping_vars)) {
      tmp <- tmp %>%
        dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarise(total = dplyr::n(), .groups = "drop")
    } else {
      tmp <- tmp %>%
        dplyr::group_by(sim) %>%
        dplyr::summarise(total = dplyr::n(), .groups = "drop")
    }
    
    tmp$year <- y
    tmp
  })
  
  ts_full <- dplyr::bind_rows(results)
  
  if (!is.null(grouping_vars)) {
    ts_full %>%
      dplyr::group_by(year, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    ts_full %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  }
}

compute_2050 <- function(mc_obj, grouping_vars = NULL) {
  
  df <- mc_obj %>%
    dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050)
  
  if (!is.null(grouping_vars)) {
    df <- df %>%
      dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  } else {
    df <- df %>%
      dplyr::group_by(sim) %>%
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  }
  
  if (!is.null(grouping_vars)) {
    df %>%
      dplyr::group_by(dplyr::across(-c(sim, total))) %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    df %>%
      dplyr::summarise(
        P10 = safe_q(total, 0.10),
        P50 = safe_q(total, 0.50),
        P90 = safe_q(total, 0.90),
        mean = safe_mean(total)
      )
  }
}

baseline_global  <- build_ts(mc_baseline_full)
baseline_oecd    <- build_ts(mc_baseline_full, "oecd")
baseline_water   <- build_ts(mc_baseline_full, "water_cooled")
baseline_cooling <- build_ts(mc_baseline_full, "cooling_type")
baseline_region  <- build_ts(mc_baseline_full, "region")

baseline_2050_global <- compute_2050(mc_baseline_full)
baseline_2050_oecd   <- compute_2050(mc_baseline_full, "oecd")
baseline_2050_region <- compute_2050(mc_baseline_full, "region")

master_sweep_path_run <- file.path(
  RUN_DIR,
  paste0("SMR55_MASTER_SUMMARY_SWEEP_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_", stamp, ".xlsx")
)

master_sweep_path_latest <- file.path(
  LATEST_DIR,
  paste0("SMR55_MASTER_SUMMARY_SWEEP_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_latest.xlsx")
)

snapshot_2050_path_run <- file.path(
  RUN_DIR,
  paste0("SMR55_2050_SNAPSHOT_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_", stamp, ".xlsx")
)

snapshot_2050_path_latest <- file.path(
  LATEST_DIR,
  paste0("SMR55_2050_SNAPSHOT_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_latest.xlsx")
)

write_xlsx(
  list(
    design_level_summary = design_summary,
    baseline_global = baseline_global,
    baseline_oecd = baseline_oecd,
    baseline_water = baseline_water,
    baseline_cooling = baseline_cooling,
    baseline_region = baseline_region
  ),
  master_sweep_path_run
)

write_xlsx(
  list(
    design_level_summary = design_summary,
    baseline_global = baseline_global,
    baseline_oecd = baseline_oecd,
    baseline_water = baseline_water,
    baseline_cooling = baseline_cooling,
    baseline_region = baseline_region
  ),
  master_sweep_path_latest
)

write_xlsx(
  list(
    baseline_global_2050 = baseline_2050_global,
    baseline_oecd_2050 = baseline_2050_oecd,
    baseline_region_2050 = baseline_2050_region
  ),
  snapshot_2050_path_run
)

write_xlsx(
  list(
    baseline_global_2050 = baseline_2050_global,
    baseline_oecd_2050 = baseline_2050_oecd,
    baseline_region_2050 = baseline_2050_region
  ),
  snapshot_2050_path_latest
)

# -------------------- CONSOLE OUTPUT --------------------
cat("\n==================== ", RUN_TAG, " ", MODEL_TAG, " COMPLETE ====================\n", sep = "")
cat("Input workbook:", P_INPUT, "\n")
cat("Output directory:", OUTDIR, "\n")
cat("Version:", VERSION, "\n")
cat("Simulations:", SIMS, "\n")
cat("Baseline rows:", nrow(mc_baseline), "\n")
cat("Excel saved (run):\n", xlsx_path_run, "\n")
cat("Excel saved (latest):\n", xlsx_path_latest, "\n")
cat("Master summary sweep (run):\n", master_sweep_path_run, "\n")
cat("Master summary sweep (latest):\n", master_sweep_path_latest, "\n")
cat("2050 snapshot (run):\n", snapshot_2050_path_run, "\n")
cat("2050 snapshot (latest):\n", snapshot_2050_path_latest, "\n")
cat("Bundle RDS saved:\n", file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_bundle_latest.rds")), "\n")
cat("Workspace RData saved:\n", file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_workspace_latest.RData")), "\n")
cat("Repository note: for a quick smoke test, set SIMS <- 25L.\n")
cat("==========================================================================\n\n")

print(xlsx_path_run)
print(xlsx_path_latest)
print(master_sweep_path_run)
print(master_sweep_path_latest)
print(snapshot_2050_path_run)
print(snapshot_2050_path_latest)
print(file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_bundle_latest.rds")))
print(file.path(LATEST_DIR, paste0("SMR55_", RUN_TAG, "_", MODEL_TAG, "_", VERSION, "_workspace_latest.RData")))
