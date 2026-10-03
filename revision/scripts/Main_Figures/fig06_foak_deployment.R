# ------------------------------------------------------------
# REPOSITORY PATHS
# ------------------------------------------------------------

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
      "fig06_source_data_foak_deployment.xlsx"
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
          "fig06_source_data_foak_deployment.xlsx\n",
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
  "fig06_source_data_foak_deployment.xlsx"
)


OUT_DIR <- file.path(
  ROOT_DIR,
  "revision",
  "outputs",
  "Main_Figures"
)


dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


if (!file.exists(FILE)) {
  stop(
    "Input file not found: ",
    FILE
  )
}


available_sheets <- excel_sheets(FILE)


required_sheets <- c(
  "global_outlook",
  "baseline_counterfactuals",
  "accelerated_counterfactuals"
)


missing_sheets <- required_sheets[
  !required_sheets %in%
    available_sheets
]


if (length(missing_sheets) > 0) {
  
  stop(
    "Missing required sheet(s) in Figure 6 source workbook:\n",
    paste(
      missing_sheets,
      collapse = "\n"
    ),
    "\n\nAvailable sheets are:\n",
    paste(
      available_sheets,
      collapse = "\n"
    )
  )
}