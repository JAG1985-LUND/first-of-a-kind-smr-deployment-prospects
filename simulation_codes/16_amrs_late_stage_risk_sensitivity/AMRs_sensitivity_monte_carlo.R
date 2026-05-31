# ============================================================
# SMR55 — AMRs SENSITIVITY MONTE CARLO
# GITHUB-COMPATIBLE VERSION
# FAIL-GATED • LOGNORMAL • ACCELERATED ONLY
#
# Save this script as: AMRs_sensitivity.R
# Place this script in the same folder as AMRs_sensitivity_inputs.xlsx.
#
# KEY LOGIC:
# - reads the GitHub/local AMRs sensitivity input workbook
# - expects four workbook tabs when available:
#     smr_design_inputs
#     phase_rules
#     design_phase_failures
#     amr_failure_rates
# - robustly detects sheets if Excel keeps temporary names such as Sheet1/Sheet2/Sheet3
# - runs only AMR / non-water-cooled designs
# - uses accelerated timing / project progression only
# - uses AMR counterfactual failure columns where present
# - if counterfactual or accelerated failure columns are missing, it falls back safely
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

# -------------------- PATHS (GITHUB / LOCAL FOLDER) --------------------
# Run this script from the folder containing the input workbook.
# Required: AMRs_sensitivity_inputs.xlsx
# Optional fallback files, used only if the relevant sheets are not in the main workbook:
#   - design_phase_failures.xlsx
#   - phase_rules.xlsx

P_INPUTS <- "AMRs_sensitivity_inputs.xlsx"
P_DPHASE_FALLBACK <- "design_phase_failures.xlsx"
P_RULES_FALLBACK  <- "phase_rules.xlsx"

OUTDIR <- "."

if (!file.exists(P_INPUTS)) {
  stop("Input workbook not found: ", P_INPUTS, call. = FALSE)
}

# -------------------- GLOBALS --------------------
CUT_YEAR   <- 2025L
NEXT_FLOOR <- 2026.0
SIMS       <- 10000L    # for a quick smoke test, temporarily set this to 25L
SEED       <- 42
YEARS      <- 2026:2050
RUN_TAG    <- "AMRS_SENSITIVITY"

set.seed(SEED)

if (!dir.exists(OUTDIR)) dir.create(OUTDIR, recursive = TRUE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M")
RUN_DIR    <- file.path(OUTDIR, paste0("SMR55_", RUN_TAG, "_", stamp))
LATEST_DIR <- file.path(OUTDIR, paste0("SMR55_", RUN_TAG, "_latest"))

dir.create(RUN_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LATEST_DIR, showWarnings = FALSE, recursive = TRUE)

# -------------------- STANDARDISERS --------------------
clean_colnames <- function(x){
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_+|_+$", "")
  x[is.na(x) | x == ""] <- paste0("blank_", which(is.na(x) | x == ""))
  make.unique(x, sep = "_")
}

clean_df_names <- function(df){
  names(df) <- clean_colnames(names(df))
  df
}

as_phase <- function(x) {
  x <- stringr::str_to_lower(stringr::str_replace_all(stringr::str_trim(as.character(x)), "[ -]", "_"))
  dplyr::case_when(
    x %in% c("conceptual", "concept", "conceptual_design_phase") ~ "conceptual_design",
    x %in% c("basic", "basic_design_phase") ~ "basic_design",
    x %in% c("detailed", "detail", "detailed_design_phase") ~ "detailed_design",
    x %in% c("under_licensing", "licenced", "licensed") ~ "licensing",
    x %in% c("under_construction") ~ "construction",
    x %in% c("operation", "operating", "operational") ~ "operational",
    TRUE ~ x
  )
}

as_scen <- function(x) stringr::str_to_lower(stringr::str_trim(as.character(x)))

norm_na <- function(x){
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, stringr::str_trim(x))
  x <- ifelse(tolower(x) %in% c("na", "n/a", "", "null"), NA_character_, x)
  x
}

extract_year <- function(x){
  y <- suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
  ifelse(is.na(y), NA_integer_, y)
}

clamp01 <- function(x){
  x <- suppressWarnings(as.numeric(x))
  pmin(1, pmax(0, x))
}

safe_q <- function(x, p){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

safe_mean <- function(x){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x){
  x <- x[is.finite(x)]
  if (length(x) <= 1) return(NA_real_)
  stats::sd(x)
}

safe_min <- function(x){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x){
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

as_bool_any <- function(x){
  if (is.logical(x)) return(x)
  x_chr <- stringr::str_trim(tolower(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y")  ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

design_key <- function(x){
  x <- toupper(gsub("[^A-Z0-9]+", "", trimws(as.character(x))))
  dplyr::case_when(
    x %in% c("HOLTECSMR300", "SMR300") ~ "SMR300",
    TRUE ~ x
  )
}

pick_col <- function(df, candidates, required = FALSE, label = "column"){
  candidates <- clean_colnames(candidates)
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[[1]])
  if (required) stop("Missing required ", label, ": ", paste(candidates, collapse = ", "), call. = FALSE)
  NA_character_
}

vec_chr <- function(df, candidates, default = NA_character_){
  col <- pick_col(df, candidates, required = FALSE)
  if (is.na(col)) return(rep(default, nrow(df)))
  as.character(df[[col]])
}

vec_num <- function(df, candidates, default = NA_real_){
  col <- pick_col(df, candidates, required = FALSE)
  if (is.na(col)) return(rep(default, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

vec_bool <- function(df, candidates, default = NA){
  col <- pick_col(df, candidates, required = FALSE)
  if (is.na(col)) return(rep(default, nrow(df)))
  as_bool_any(df[[col]])
}

choose_sheet <- function(path, preferred_names, required_all = character(), required_any = character(), prefer_more = character(), description = "input"){
  sheets <- readxl::excel_sheets(path)
  sheet_lc <- stringr::str_to_lower(stringr::str_trim(sheets))
  pref_lc <- stringr::str_to_lower(stringr::str_trim(preferred_names))
  direct <- match(pref_lc, sheet_lc, nomatch = 0)
  direct <- direct[direct > 0]
  if (length(direct) > 0) return(sheets[direct[[1]]])
  
  scores <- purrr::map_dbl(sheets, function(sh){
    hdr <- tryCatch({
      tmp <- readxl::read_excel(path, sheet = sh, n_max = 0, .name_repair = "minimal")
      clean_colnames(names(tmp))
    }, error = function(e) character())
    if (length(hdr) == 0) return(-Inf)
    if (length(required_all) > 0 && !all(clean_colnames(required_all) %in% hdr)) return(-Inf)
    score <- 0
    score <- score + sum(clean_colnames(required_any) %in% hdr) * 10
    score <- score + sum(clean_colnames(prefer_more) %in% hdr)
    score
  })
  
  if (all(!is.finite(scores)) || max(scores, na.rm = TRUE) <= 0) {
    stop("Could not identify the ", description, " sheet in: ", path, call. = FALSE)
  }
  
  picked <- sheets[which.max(scores)]
  message("Using sheet '", picked, "' for ", description, ".")
  picked
}

read_input_sheet <- function(path, preferred_names, required_all = character(), required_any = character(), prefer_more = character(), description = "input"){
  sh <- choose_sheet(
    path = path,
    preferred_names = preferred_names,
    required_all = required_all,
    required_any = required_any,
    prefer_more = prefer_more,
    description = description
  )
  readxl::read_excel(path, sheet = sh, .name_repair = "minimal") %>%
    clean_df_names()
}

# -------------------- LOAD UPDATED INPUT WORKBOOK --------------------
smr_raw <- read_input_sheet(
  P_INPUTS,
  preferred_names = c("smr_design_inputs", "smr_design_input", "smr_inputs", "smrs_55_clean"),
  required_all = c("phase"),
  required_any = c("smr", "design"),
  prefer_more = c("smr", "mw", "oecd", "sample", "cd_start", "bd_start", "dd_start", "licensing_start", "construction_start", "operational_start"),
  description = "SMR design inputs"
)

fail_raw <- read_input_sheet(
  P_INPUTS,
  preferred_names = c("amr_failure_rates", "failure_rates", "failures_non_water_cooled_counterfactual"),
  required_all = c("design"),
  required_any = c(
    "licensing_failure_counterfactual",
    "construction_failure_counterfactual",
    "licensing_failure_baseline",
    "licensing_failure_accelerated",
    "construction_failure_baseline",
    "construction_failure_accelerated"
  ),
  prefer_more = c("licensing_failure_counterfactual", "construction_failure_counterfactual", "licensing_failure_accelerated", "construction_failure_accelerated"),
  description = "AMR failure rates"
)

dpf_raw <- tryCatch(
  read_input_sheet(
    P_INPUTS,
    preferred_names = c("design_phase_failures", "phase_failures", "development_phase_failures"),
    required_all = c("phase", "scenario"),
    required_any = c("failures_k", "total_n", "failures", "total", "n"),
    prefer_more = c("failures_k", "total_n"),
    description = "design-phase failures"
  ),
  error = function(e) {
    if (file.exists(P_DPHASE_FALLBACK)) {
      message("Using fallback file '", P_DPHASE_FALLBACK, "' for design-phase failures.")
      readxl::read_excel(P_DPHASE_FALLBACK, .name_repair = "minimal") %>%
        clean_df_names()
    } else {
      stop(
        "Could not find design-phase failures in ", P_INPUTS,
        " and fallback file is missing: ", P_DPHASE_FALLBACK,
        call. = FALSE
      )
    }
  }
)

phase_rules_raw <- tryCatch(
  read_input_sheet(
    P_INPUTS,
    preferred_names = c("phase_rules", "rules", "project_phase_rules"),
    required_all = character(),
    required_any = c("baseline_condition", "accelerated_condition", "phase_rule_baseline_licensing", "phase_rule_accelerated_licensing"),
    prefer_more = c("design", "smr", "baseline_condition", "accelerated_condition"),
    description = "phase rules"
  ),
  error = function(e) {
    if (file.exists(P_RULES_FALLBACK)) {
      message("Using fallback file '", P_RULES_FALLBACK, "' for phase rules.")
      readxl::read_excel(P_RULES_FALLBACK, .name_repair = "minimal") %>%
        clean_df_names()
    } else {
      NULL
    }
  }
)

# -------------------- STANDARDISE SMR DESIGN INPUTS --------------------
standardise_smr_specs <- function(raw){
  raw <- clean_df_names(raw)
  
  design_col <- if ("smr" %in% names(raw)) "smr" else pick_col(raw, c("design", "smr"), required = TRUE, label = "design column")
  cooling_col <- dplyr::case_when(
    "cooling_type" %in% names(raw) ~ "cooling_type",
    "reactor_cooling_type" %in% names(raw) ~ "reactor_cooling_type",
    "cooling" %in% names(raw) ~ "cooling",
    "design" %in% names(raw) && design_col != "design" ~ "design",
    TRUE ~ NA_character_
  )
  
  cooling_vec <- if (!is.na(cooling_col)) as.character(raw[[cooling_col]]) else rep(NA_character_, nrow(raw))
  water_vec <- vec_bool(raw, c("water_cooled", "water"), default = NA)
  water_vec <- ifelse(is.na(water_vec), stringr::str_detect(tolower(cooling_vec), "water"), water_vec)
  
  tibble::tibble(
    design = stringr::str_trim(as.character(raw[[design_col]])),
    design_key = design_key(raw[[design_col]]),
    phase  = as_phase(vec_chr(raw, c("phase", "current_phase"))),
    country = vec_chr(raw, c("country", "country_code")),
    region  = vec_chr(raw, c("region", "world_region")),
    oecd = vec_bool(raw, c("oecd", "oecd_membership")),
    national_champion = vec_bool(raw, c("national_champion")),
    size = vec_num(raw, c("size", "mw", "mwe", "capacity", "capacity_mwe")),
    cooling_type = cooling_vec,
    water_cooled = as.logical(water_vec),
    spectrum = vec_chr(raw, c("spectrum", "neutron_spectrum")),
    uranium_enrichment = vec_chr(raw, c("uranium_enrichment", "enrichment")),
    cd_start = vec_num(raw, c("cd_start", "conceptual_start", "conceptual_design_start", "conceptual_design_start_year")),
    bd_start = vec_num(raw, c("bd_start", "basic_start", "basic_design_start", "basic_design_start_year")),
    dd_start = vec_num(raw, c("dd_start", "detailed_start", "detailed_design_start", "detailed_design_start_year")),
    licensing_start = vec_num(raw, c("licensing_start", "licence_start", "license_start", "licensing_start_year")),
    construction_start = vec_num(raw, c("construction_start", "construction_start_year")),
    operational_start = vec_num(raw, c("operational_start", "operation_start", "operational_start_year", "foak_operation_year"))
  ) %>%
    dplyr::filter(!is.na(design), design != "")
}

smr55_specs_full <- standardise_smr_specs(smr_raw)

dup_specs <- smr55_specs_full %>%
  dplyr::count(design_key) %>%
  dplyr::filter(n > 1)

if (nrow(dup_specs) > 0) {
  stop("Duplicate design keys in SMR design inputs after harmonisation: ", paste(dup_specs$design_key, collapse = ", "), call. = FALSE)
}

# -------------------- STANDARDISE PHASE RULES --------------------
standardise_phase_rules <- function(raw, design_keys){
  if (is.null(raw)) {
    return(tibble::tibble(
      design_key = design_keys,
      baseline_condition = NA_character_,
      accelerated_condition = NA_character_,
      phase_rule_baseline_licensing = NA_character_,
      phase_rule_accelerated_licensing = NA_real_
    ))
  }
  
  raw <- clean_df_names(raw)
  design_col <- if ("smr" %in% names(raw)) "smr" else pick_col(raw, c("design", "smr"), required = TRUE, label = "phase-rules design column")
  
  tibble::tibble(
    design_key = design_key(raw[[design_col]]),
    baseline_condition = norm_na(vec_chr(raw, c("baseline_condition"))),
    accelerated_condition = norm_na(vec_chr(raw, c("accelerated_condition"))),
    phase_rule_baseline_licensing = norm_na(vec_chr(raw, c("phase_rule_baseline_licensing"))),
    phase_rule_accelerated_licensing = vec_num(raw, c("phase_rule_accelerated_licensing"))
  ) %>%
    dplyr::filter(!is.na(design_key), design_key != "") %>%
    dplyr::mutate(
      accelerated_condition = dplyr::if_else(
        design_key %in% c("ACP100", "BRESTOD300") &
          stringr::str_detect(accelerated_condition, "^force_operational_\\d{4}$"),
        NA_character_,
        accelerated_condition
      )
    )
}

phase_rules <- standardise_phase_rules(phase_rules_raw, smr55_specs_full$design_key)

dup_rules <- phase_rules %>%
  dplyr::count(design_key) %>%
  dplyr::filter(n > 1)

if (nrow(dup_rules) > 0) {
  stop("Duplicate design keys in phase rules after harmonisation: ", paste(dup_rules$design_key, collapse = ", "), call. = FALSE)
}

# -------------------- STANDARDISE AMR FAILURE RATES --------------------
standardise_failure_rates <- function(raw){
  raw <- clean_df_names(raw)
  design_col <- pick_col(raw, c("design", "smr"), required = TRUE, label = "failure-rates design column")
  
  lic_fail_acc <- if ("licensing_failure_counterfactual" %in% names(raw)) {
    vec_num(raw, c("licensing_failure_counterfactual"))
  } else if ("licensing_failure_accelerated" %in% names(raw)) {
    vec_num(raw, c("licensing_failure_accelerated"))
  } else {
    vec_num(raw, c("licensing_failure_baseline"))
  }
  
  con_fail_acc <- if ("construction_failure_counterfactual" %in% names(raw)) {
    vec_num(raw, c("construction_failure_counterfactual"))
  } else if ("construction_failure_accelerated" %in% names(raw)) {
    vec_num(raw, c("construction_failure_accelerated"))
  } else {
    vec_num(raw, c("construction_failure_baseline"))
  }
  
  lic_fail_base <- if ("licensing_failure_baseline" %in% names(raw)) {
    vec_num(raw, c("licensing_failure_baseline"))
  } else {
    lic_fail_acc
  }
  
  con_fail_base <- if ("construction_failure_baseline" %in% names(raw)) {
    vec_num(raw, c("construction_failure_baseline"))
  } else {
    con_fail_acc
  }
  
  tibble::tibble(
    design = stringr::str_trim(as.character(raw[[design_col]])),
    design_key = design_key(raw[[design_col]]),
    water = vec_bool(raw, c("water", "water_cooled")),
    cooling = vec_chr(raw, c("cooling", "cooling_type", "reactor_cooling_type")),
    phase = as_phase(vec_chr(raw, c("phase", "current_phase"))),
    country = vec_chr(raw, c("country", "country_code")),
    region = vec_chr(raw, c("region", "world_region")),
    licensing_failure_baseline = clamp01(lic_fail_base),
    licensing_failure_accelerated = clamp01(lic_fail_acc),
    construction_failure_baseline = clamp01(con_fail_base),
    construction_failure_accelerated = clamp01(con_fail_acc),
    lic_surv_baseline = clamp01(1 - lic_fail_base),
    lic_surv_accel = clamp01(1 - lic_fail_acc),
    con_surv_baseline = clamp01(1 - con_fail_base),
    con_surv_accel = clamp01(1 - con_fail_acc)
  ) %>%
    dplyr::filter(!is.na(design_key), design_key != "") %>%
    dplyr::select(
      design_key,
      licensing_failure_baseline,
      licensing_failure_accelerated,
      construction_failure_baseline,
      construction_failure_accelerated,
      lic_surv_baseline,
      lic_surv_accel,
      con_surv_baseline,
      con_surv_accel
    )
}

fail_rates <- standardise_failure_rates(fail_raw)

dup_fail <- fail_rates %>%
  dplyr::count(design_key) %>%
  dplyr::filter(n > 1)

if (nrow(dup_fail) > 0) {
  stop("Duplicate design keys in AMR failure rates after harmonisation: ", paste(dup_fail$design_key, collapse = ", "), call. = FALSE)
}

# -------------------- DEFINE AMR / NON-WATER SUBGROUP --------------------
subgroup_keys <- fail_rates %>%
  dplyr::distinct(design_key) %>%
  dplyr::pull(design_key)

missing_in_specs <- setdiff(subgroup_keys, smr55_specs_full$design_key)
if (length(missing_in_specs) > 0) {
  stop("These AMR sensitivity designs are in the failure-rate sheet but not in smr_design_inputs: ", paste(missing_in_specs, collapse = ", "), call. = FALSE)
}

smr55_specs <- smr55_specs_full %>%
  dplyr::filter(
    water_cooled == FALSE,
    design_key %in% subgroup_keys
  )

if (nrow(smr55_specs) == 0) stop("No AMR / non-water-cooled subgroup designs found after filtering.", call. = FALSE)

missing_rules <- setdiff(smr55_specs$design_key, phase_rules$design_key)
if (length(missing_rules) > 0) {
  phase_rules <- dplyr::bind_rows(
    phase_rules,
    tibble::tibble(
      design_key = missing_rules,
      baseline_condition = NA_character_,
      accelerated_condition = NA_character_,
      phase_rule_baseline_licensing = NA_character_,
      phase_rule_accelerated_licensing = NA_real_
    )
  )
}

subgroup_designs <- smr55_specs %>%
  dplyr::arrange(design) %>%
  dplyr::select(
    design, design_key, phase, country, region, oecd, national_champion, size,
    cooling_type, water_cooled, spectrum, uranium_enrichment,
    cd_start, bd_start, dd_start, licensing_start, construction_start, operational_start
  )

# -------------------- LOAD DESIGN-PHASE FAILURES --------------------
dpf_raw <- clean_df_names(dpf_raw)

dpf_acc <- tibble::tibble(
  phase = as_phase(vec_chr(dpf_raw, c("phase"))),
  scenario = as_scen(vec_chr(dpf_raw, c("scenario"))),
  k = vec_num(dpf_raw, c("failures_k", "failures", "k")),
  N = vec_num(dpf_raw, c("total_n", "total", "n", "total_n_1"))
) %>%
  dplyr::filter(scenario == "accelerated") %>%
  dplyr::mutate(
    p_fail_mean = (k + 0.5) / (N + 1),
    p_surv_mean = clamp01(1 - p_fail_mean)
  )

req_phases <- c("conceptual_design", "basic_design", "detailed_design")
miss_phases <- setdiff(req_phases, unique(dpf_acc$phase))
if (length(miss_phases) > 0) {
  stop("Missing accelerated phases in design_phase_failures: ", paste(miss_phases, collapse = ", "), call. = FALSE)
}

jeffreys_survival_acc <- setNames(dpf_acc$p_surv_mean, dpf_acc$phase)

get_surv_phase <- function(phase, row){
  phase <- as_phase(phase)
  
  if (phase %in% c("conceptual_design", "basic_design", "detailed_design")){
    p <- jeffreys_survival_acc[[phase]]
    return(clamp01(p))
  }
  
  if (phase == "licensing") return(clamp01(row$lic_surv_accel))
  if (phase == "construction") return(clamp01(row$con_surv_accel))
  
  1.0
}

# -------------------- BUILD MASTER --------------------
smr_master <- smr55_specs %>%
  dplyr::left_join(phase_rules, by = "design_key") %>%
  dplyr::left_join(fail_rates, by = "design_key")

miss_fail <- smr_master %>%
  dplyr::filter(is.na(lic_surv_accel) | is.na(con_surv_accel)) %>%
  dplyr::pull(design)

if (length(miss_fail) > 0) {
  stop("Missing accelerated AMR failure rates for subgroup designs: ", paste(unique(miss_fail), collapse = ", "), call. = FALSE)
}

# -------------------- ACCELERATED PHASE PARAMETERS --------------------
phase_params <- tibble::tribble(
  ~phase,              ~mean,    ~sd,     ~min,  ~max,
  "conceptual_design",  5.11,    1.95,    2.0,   10.0,
  "basic_design",       3.39,    1.03,    2.0,   5.0,
  "detailed_design",    3.36,    1.15,    2.0,   5.0,
  "licensing",          0.8405,  0.1432,  1.0,   3.0,
  "construction",       1.3768,  0.1242,  3.0,   5.0
) %>%
  dplyr::mutate(phase = as_phase(phase))

param_map <- phase_params %>%
  dplyr::mutate(key = phase) %>%
  split(.$key) %>%
  purrr::map(~ list(mean = .x$mean[[1]], sd = .x$sd[[1]], min = .x$min[[1]], max = .x$max[[1]]))

get_p <- function(phase){
  k <- as_phase(phase)
  if (!k %in% names(param_map)) stop("Missing accelerated phase params for: ", k, call. = FALSE)
  param_map[[k]]
}

# -------------------- DISTRIBUTION HELPERS --------------------
lognorm_from_mean_sd <- function(mean, sd){
  v <- log(1 + (sd^2) / (mean^2))
  list(meanlog = log(mean) - 0.5 * v, sdlog = sqrt(v))
}

r_lnorm_trunc_cond_total <- function(elapsed, a, b, meanlog, sdlog){
  elapsed <- max(0, elapsed)
  lo <- max(a, elapsed)
  if (lo >= b) return(lo)
  Fa <- plnorm(lo, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  if (is.na(Fa) || is.na(Fb) || Fb <= Fa) return(lo)
  qlnorm(runif(1, Fa, Fb), meanlog, sdlog)
}

draw_total_duration <- function(phase, elapsed){
  phase <- as_phase(phase)
  p <- get_p(phase)
  
  if (!is.na(elapsed) && elapsed >= p$max) return(elapsed)
  
  if (phase %in% c("licensing", "construction")){
    return(r_lnorm_trunc_cond_total(elapsed, p$min, p$max, p$mean, p$sd))
  }
  
  lp <- lognorm_from_mean_sd(p$mean, p$sd)
  r_lnorm_trunc_cond_total(elapsed, p$min, p$max, lp$meanlog, lp$sdlog)
}

# -------------------- CONDITION OVERRIDES --------------------
apply_next_override <- function(next_phase, cond){
  cond <- norm_na(cond)
  if (is.na(cond)) return(NA_real_)
  
  if (next_phase == "basic_design" && cond == "force_bd_start_2026") return(2026.0)
  if (next_phase == "detailed_design" && cond == "force_dd_start_2026") return(2026.0)
  
  if (next_phase == "construction" && grepl("^construction_start_\\d{4}$", cond)){
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  if (next_phase == "operational" && grepl("^force_operational_\\d{4}$", cond)){
    y <- extract_year(cond)
    if (!is.na(y)) return(as.numeric(y))
  }
  
  NA_real_
}

# -------------------- CN/RU LICENSING RULE --------------------
licensing_duration_with_rules <- function(row, elapsed){
  ctry <- as.character(row$country)
  acc_rule <- suppressWarnings(as.numeric(row$phase_rule_accelerated_licensing))
  is_cnru <- ctry %in% c("CN", "RU")
  
  if (is_cnru || (!is.na(acc_rule) && acc_rule == 1)) return(1.0)
  draw_total_duration("licensing", elapsed)
}

# -------------------- LICENSING FAILURE EXEMPTIONS --------------------
licensing_failure_exempt_designs <- c("BWRX-300")

# -------------------- SIMULATE ONE DESIGN TO FOAK --------------------
simulate_to_foak <- function(row){
  
  phases <- c("conceptual_design", "basic_design", "detailed_design", "licensing", "construction", "operational")
  
  cur <- row$phase
  if (is.na(cur) || !cur %in% phases){
    return(tibble::tibble(
      design = row$design,
      scenario = "accelerated",
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
  
  cond <- norm_na(row$accelerated_condition)
  
  cd_s  <- row$cd_start
  bd_s  <- row$bd_start
  dd_s  <- row$dd_start
  lic_s <- row$licensing_start
  con_s <- row$construction_start
  op_s  <- row$operational_start
  
  cd_e <- bd_e <- dd_e <- lic_e <- con_e <- NA_real_
  
  if (cur == "operational"){
    return(tibble::tibble(
      design = row$design,
      scenario = "accelerated",
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
  
  if (row$design == "CAREM" && cur == "construction"){
    p_con <- get_p("construction")
    total_con <- r_lnorm_trunc_cond_total(0, p_con$min, p_con$max, p_con$mean, p_con$sd)
    frac <- runif(1, 0.40, 0.50)
    remain <- (1 - frac) * total_con
    
    con_s <- 2026.0
    con_e <- 2026.0 + remain
    op_s  <- con_e
    
    ov_op <- apply_next_override("operational", cond)
    forced_op <- !is.na(ov_op)
    
    if (forced_op){
      op_s  <- ov_op
      con_e <- op_s
    }
    
    if (!forced_op){
      p_surv_con <- get_surv_phase("construction", row)
      if (runif(1) > p_surv_con){
        return(tibble::tibble(
          design = row$design,
          scenario = "accelerated",
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
      scenario = "accelerated",
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
  
  get_start <- function(phase){
    if (phase == "conceptual_design") return(cd_s)
    if (phase == "basic_design")      return(bd_s)
    if (phase == "detailed_design")   return(dd_s)
    if (phase == "licensing")         return(lic_s)
    if (phase == "construction")      return(con_s)
    if (phase == "operational")       return(op_s)
    NA_real_
  }
  
  set_start <- function(phase, val){
    if (phase == "basic_design")      bd_s  <<- val
    if (phase == "detailed_design")   dd_s  <<- val
    if (phase == "licensing")         lic_s <<- val
    if (phase == "construction")      con_s <<- val
    if (phase == "operational")       op_s  <<- val
  }
  
  set_end <- function(phase, val){
    if (phase == "conceptual_design") cd_e  <<- val
    if (phase == "basic_design")      bd_e  <<- val
    if (phase == "detailed_design")   dd_e  <<- val
    if (phase == "licensing")         lic_e <<- val
    if (phase == "construction")      con_e <<- val
  }
  
  i0 <- match(cur, phases)
  
  for (i in i0:(length(phases) - 1)){
    
    ph <- phases[i]
    st <- get_start(ph)
    
    if (is.na(st)){
      st <- ifelse(ph == cur, CUT_YEAR, NEXT_FLOOR)
    }
    
    elapsed <- if (ph == cur && st <= CUT_YEAR) max(0, CUT_YEAR - st + 1) else 0
    
    if (ph == "licensing"){
      
      tot <- licensing_duration_with_rules(row, elapsed)
      en_draw <- st + tot
      if (elapsed > 0 && en_draw < CUT_YEAR) en_draw <- CUT_YEAR
      
      lic_exempt <- row$design %in% licensing_failure_exempt_designs
      
      if (!lic_exempt){
        p_surv <- get_surv_phase("licensing", row)
        if (runif(1) > p_surv){
          return(tibble::tibble(
            design = row$design,
            scenario = "accelerated",
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
    
    if (forced_transition){
      nxt_start <- ov
      en <- nxt_start
    }
    
    set_end(ph, en)
    
    skip_failure <- forced_transition
    
    if (!skip_failure){
      p_surv <- get_surv_phase(ph, row)
      if (runif(1) > p_surv){
        return(tibble::tibble(
          design = row$design,
          scenario = "accelerated",
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
    
    if (nxt == "operational"){
      ov2 <- apply_next_override("operational", cond)
      if (!is.na(ov2)){
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
    scenario = "accelerated",
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
design_list <- split(smr_master, smr_master$design_key)

run_mc <- function(sims){
  purrr::map_dfr(seq_len(sims), function(s){
    purrr::map_dfr(design_list, ~ simulate_to_foak(.x[1, ])) %>%
      dplyr::mutate(sim = s) %>%
      dplyr::select(
        sim, design, scenario, status, fail_phase,
        cd_start, bd_start, dd_start, licensing_start, construction_start, operational_start,
        foak_year
      )
  })
}

# -------------------- RUN MONTE CARLO --------------------
set.seed(SEED)
mc_accel <- run_mc(SIMS)

# -------------------- DESIGN-LEVEL SUMMARIES --------------------
summary_quantiles <- mc_accel %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(
    n_total = dplyr::n(),
    n_success = sum(!is.na(foak_year)),
    success_rate = n_success / n_total,
    P10 = safe_q(foak_year, 0.10),
    P50 = safe_q(foak_year, 0.50),
    P90 = safe_q(foak_year, 0.90),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "accelerated") %>%
  dplyr::select(design, scenario, n_total, n_success, success_rate, P10, P50, P90) %>%
  dplyr::arrange(design)

summary_descriptives <- mc_accel %>%
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
  dplyr::mutate(scenario = "accelerated") %>%
  dplyr::select(design, scenario, n, mean, sd, min, q1, p10, p50, q3, p90, max) %>%
  dplyr::arrange(design)

run_metadata <- tibble::tibble(
  item = c(
    "timestamp",
    "seed",
    "sims",
    "cut_year",
    "next_floor",
    "input_workbook",
    "design_phase_failures_fallback",
    "phase_rules_fallback",
    "run_directory",
    "latest_directory",
    "model_type"
  ),
  value = c(
    stamp,
    as.character(SEED),
    as.character(SIMS),
    as.character(CUT_YEAR),
    as.character(NEXT_FLOOR),
    P_INPUTS,
    P_DPHASE_FALLBACK,
    P_RULES_FALLBACK,
    RUN_DIR,
    LATEST_DIR,
    RUN_TAG
  )
)

# -------------------- EXPORT MAIN EXCEL --------------------
xlsx_path_run    <- file.path(RUN_DIR,    paste0("SMR55_AMRs_sensitivity_draws_", stamp, ".xlsx"))
xlsx_path_latest <- file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_draws_latest.xlsx")

xlsx_contents <- list(
  accelerated_draws    = mc_accel,
  summary_quantiles    = summary_quantiles,
  summary_descriptives = summary_descriptives,
  subgroup_designs     = subgroup_designs,
  smr_master           = smr_master,
  run_metadata         = run_metadata
)

writexl::write_xlsx(xlsx_contents, path = xlsx_path_run)
writexl::write_xlsx(xlsx_contents, path = xlsx_path_latest)

# -------------------- SAVE R OBJECTS --------------------
saveRDS(mc_accel,             file = file.path(RUN_DIR,    paste0("mc_accelerated_AMRs_sensitivity_", stamp, ".rds")))
saveRDS(summary_quantiles,    file = file.path(RUN_DIR,    paste0("summary_quantiles_AMRs_sensitivity_", stamp, ".rds")))
saveRDS(summary_descriptives, file = file.path(RUN_DIR,    paste0("summary_descriptives_AMRs_sensitivity_", stamp, ".rds")))
saveRDS(smr_master,           file = file.path(RUN_DIR,    paste0("smr_master_AMRs_sensitivity_", stamp, ".rds")))
saveRDS(phase_params,         file = file.path(RUN_DIR,    paste0("phase_params_AMRs_sensitivity_", stamp, ".rds")))
saveRDS(run_metadata,         file = file.path(RUN_DIR,    paste0("run_metadata_AMRs_sensitivity_", stamp, ".rds")))

saveRDS(mc_accel,             file = file.path(LATEST_DIR, "mc_accelerated_AMRs_sensitivity_latest.rds"))
saveRDS(summary_quantiles,    file = file.path(LATEST_DIR, "summary_quantiles_AMRs_sensitivity_latest.rds"))
saveRDS(summary_descriptives, file = file.path(LATEST_DIR, "summary_descriptives_AMRs_sensitivity_latest.rds"))
saveRDS(smr_master,           file = file.path(LATEST_DIR, "smr_master_AMRs_sensitivity_latest.rds"))
saveRDS(phase_params,         file = file.path(LATEST_DIR, "phase_params_AMRs_sensitivity_latest.rds"))
saveRDS(run_metadata,         file = file.path(LATEST_DIR, "run_metadata_AMRs_sensitivity_latest.rds"))

model_bundle <- list(
  mc_accel             = mc_accel,
  summary_quantiles    = summary_quantiles,
  summary_descriptives = summary_descriptives,
  subgroup_designs     = subgroup_designs,
  smr_master           = smr_master,
  smr55_specs_full     = smr55_specs_full,
  phase_rules          = phase_rules,
  fail_rates           = fail_rates,
  dpf_acc              = dpf_acc,
  phase_params         = phase_params,
  param_map            = param_map,
  design_list          = design_list,
  run_metadata         = run_metadata
)

saveRDS(model_bundle, file = file.path(RUN_DIR,    paste0("SMR55_AMRs_sensitivity_bundle_", stamp, ".rds")))
saveRDS(model_bundle, file = file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_bundle_latest.rds"))

save(
  mc_accel,
  summary_quantiles,
  summary_descriptives,
  subgroup_designs,
  smr55_specs_full,
  phase_rules,
  fail_rates,
  dpf_acc,
  smr_master,
  phase_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(RUN_DIR, paste0("SMR55_AMRs_sensitivity_workspace_", stamp, ".RData"))
)

save(
  mc_accel,
  summary_quantiles,
  summary_descriptives,
  subgroup_designs,
  smr55_specs_full,
  phase_rules,
  fail_rates,
  dpf_acc,
  smr_master,
  phase_params,
  param_map,
  design_list,
  run_metadata,
  file = file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_workspace_latest.RData")
)

# -------------------- POST-RUN SUMMARY SWEEP --------------------
mc_accel_enriched <- mc_accel %>%
  dplyr::left_join(
    smr55_specs %>%
      dplyr::select(design, design_key, country, region, oecd, cooling_type, water_cooled, size),
    by = "design"
  )

build_ts <- function(mc_obj, grouping_vars = NULL){
  df <- mc_obj %>%
    dplyr::mutate(
      foak_year_int = floor(foak_year),
      success = !is.na(foak_year_int)
    )
  
  results <- lapply(YEARS, function(y){
    tmp <- df %>%
      dplyr::filter(success, foak_year_int <= y)
    
    if (!is.null(grouping_vars)){
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
  
  if (!is.null(grouping_vars)){
    ts_full %>%
      dplyr::group_by(year, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    ts_full %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  }
}

compute_2050 <- function(mc_obj, grouping_vars = NULL){
  df <- mc_obj %>%
    dplyr::mutate(success_2050 = !is.na(foak_year) & foak_year <= 2050)
  
  if (!is.null(grouping_vars)){
    df <- df %>%
      dplyr::group_by(sim, dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  } else {
    df <- df %>%
      dplyr::group_by(sim) %>%
      dplyr::summarise(total = sum(success_2050), .groups = "drop")
  }
  
  if (!is.null(grouping_vars)){
    df %>%
      dplyr::group_by(dplyr::across(-c(sim, total))) %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total),
        .groups = "drop"
      )
  } else {
    df %>%
      dplyr::summarise(
        P10  = safe_q(total, 0.10),
        P50  = safe_q(total, 0.50),
        P90  = safe_q(total, 0.90),
        mean = safe_mean(total)
      )
  }
}

accelerated_global  <- build_ts(mc_accel_enriched)
accelerated_oecd    <- build_ts(mc_accel_enriched, "oecd")
accelerated_region  <- build_ts(mc_accel_enriched, "region")
accelerated_country <- build_ts(mc_accel_enriched, "country")
accelerated_cooling <- build_ts(mc_accel_enriched, "cooling_type")

accelerated_2050_global  <- compute_2050(mc_accel_enriched)
accelerated_2050_oecd    <- compute_2050(mc_accel_enriched, "oecd")
accelerated_2050_region  <- compute_2050(mc_accel_enriched, "region")
accelerated_2050_country <- compute_2050(mc_accel_enriched, "country")
accelerated_2050_cooling <- compute_2050(mc_accel_enriched, "cooling_type")

master_sweep_path_run    <- file.path(RUN_DIR,    paste0("SMR55_AMRs_sensitivity_summary_", stamp, ".xlsx"))
master_sweep_path_latest <- file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_summary_latest.xlsx")

snapshot_2050_path_run    <- file.path(RUN_DIR,    paste0("SMR55_AMRs_sensitivity_2050_", stamp, ".xlsx"))
snapshot_2050_path_latest <- file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_2050_latest.xlsx")

writexl::write_xlsx(
  list(
    subgroup_designs    = subgroup_designs,
    design_level_summary = summary_quantiles,
    accelerated_global  = accelerated_global,
    accelerated_oecd    = accelerated_oecd,
    accelerated_region  = accelerated_region,
    accelerated_country = accelerated_country,
    accelerated_cooling = accelerated_cooling
  ),
  master_sweep_path_run
)

writexl::write_xlsx(
  list(
    subgroup_designs    = subgroup_designs,
    design_level_summary = summary_quantiles,
    accelerated_global  = accelerated_global,
    accelerated_oecd    = accelerated_oecd,
    accelerated_region  = accelerated_region,
    accelerated_country = accelerated_country,
    accelerated_cooling = accelerated_cooling
  ),
  master_sweep_path_latest
)

writexl::write_xlsx(
  list(
    accelerated_global_2050  = accelerated_2050_global,
    accelerated_oecd_2050    = accelerated_2050_oecd,
    accelerated_region_2050  = accelerated_2050_region,
    accelerated_country_2050 = accelerated_2050_country,
    accelerated_cooling_2050 = accelerated_2050_cooling
  ),
  snapshot_2050_path_run
)

writexl::write_xlsx(
  list(
    accelerated_global_2050  = accelerated_2050_global,
    accelerated_oecd_2050    = accelerated_2050_oecd,
    accelerated_region_2050  = accelerated_2050_region,
    accelerated_country_2050 = accelerated_2050_country,
    accelerated_cooling_2050 = accelerated_2050_cooling
  ),
  snapshot_2050_path_latest
)

# -------------------- CONSOLE OUTPUT --------------------
cat("\n==================== AMRs SENSITIVITY MONTE CARLO COMPLETE ====================\n")
cat("Designs in subgroup:", nrow(subgroup_designs), "\n")
cat("Accelerated rows:", nrow(mc_accel), "\n")
cat("SIMS:", SIMS, "\n")
cat("Main Excel (run):\n", xlsx_path_run, "\n")
cat("Main Excel (latest):\n", xlsx_path_latest, "\n")
cat("Summary sweep (run):\n", master_sweep_path_run, "\n")
cat("Summary sweep (latest):\n", master_sweep_path_latest, "\n")
cat("2050 snapshot (run):\n", snapshot_2050_path_run, "\n")
cat("2050 snapshot (latest):\n", snapshot_2050_path_latest, "\n")
cat("Bundle RDS (latest):\n", file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_bundle_latest.rds"), "\n")
cat("Workspace RData (latest):\n", file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_workspace_latest.RData"), "\n")
cat("============================================================================\n\n")

print(xlsx_path_run)
print(xlsx_path_latest)
print(master_sweep_path_run)
print(master_sweep_path_latest)
print(snapshot_2050_path_run)
print(snapshot_2050_path_latest)
print(file.path(LATEST_DIR, "SMR55_AMRs_sensitivity_bundle_latest.rds"))
