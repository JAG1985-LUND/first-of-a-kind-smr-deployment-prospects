rm(list = ls())
graphics.off()

setwd("C:/Users/jo3151go/Work Folders/Documents/GitHub/first-of-a-kind-smr-deployment-prospects")

SCRIPT_IN <- "simulation_codes/03_post_2025_design_additions/post_2025_design_additions.R"

SMOKE_DIR <- "_smoke_test/03_post_2025_design_additions"
SMOKE_SCRIPT <- file.path(SMOKE_DIR, "post_2025_design_additions_SMOKE_10.R")

dir.create(SMOKE_DIR, recursive = TRUE, showWarnings = FALSE)

code <- readLines(SCRIPT_IN, warn = FALSE)

code <- gsub(
  pattern = "^\\s*SIMS\\s*<-\\s*[0-9]+L?\\s*$",
  replacement = "SIMS <- 10L",
  x = code
)

writeLines(code, SMOKE_SCRIPT)

smoke_env <- new.env(parent = globalenv())

sys.source(
  file = SMOKE_SCRIPT,
  envir = smoke_env
)

smoke_env$PATH_MODE
smoke_env$P_INPUT
smoke_env$OUTDIR
smoke_env$RUN_DIR
smoke_env$SIMS

list.files(smoke_env$RUN_DIR)
