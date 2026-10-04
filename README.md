# First-of-a-kind SMR deployment prospects

This repository contains data, code and selected outputs supporting the manuscript **“First-of-a-Kind deployment prospects for small modular reactors”** by J. A. Gordon, T. Kazlou, J. Jewell and A. Cherp.

The repository provides the R scripts, source-data workbooks, simulation inputs and selected model outputs used to reproduce the manuscript figures, extended data figures, supplementary figures and Monte Carlo simulation analyses.

## Revision history

The repository was initially prepared for the original manuscript submission in June 2026 and was updated for the revised submission in September–October 2026.

In the original submission, the two model scenarios were denoted **Baseline** and **Accelerated**. For the revised submission, these were renamed **Reference** and **Proliferation**, respectively, to better reflect their analytical interpretation. The underlying scenario definitions and modelling logic are unchanged.

Files added or updated for the revised submission use the revised **Reference** and **Proliferation** terminology. Earlier repository history and some original simulation-code filenames may therefore contain the **Baseline** and **Accelerated** terminology.

## Repository structure

The repository preserves materials from both the original submission and the revised manuscript.

### Original submission materials

The following top-level directories contain materials associated with the original June 2026 submission and are retained for computational provenance:

- `data/` contains the original-submission source-data workbooks and Monte Carlo simulation inputs.
- `scripts/` contains the original-submission figure-reproduction scripts.
- `outputs/` contains selected outputs generated during the original analysis.
- `simulation_codes/` contains the underlying Monte Carlo simulation modules. These modules are retained as the computational basis of the study and are documented separately in `simulation_codes/README.md`.

These directories should not be interpreted as the primary figure-reproduction materials for the revised manuscript.

### Revised submission materials

The `revision/` directory contains the materials corresponding to the revised manuscript:

- `revision/data/` contains revised figure source data.
- `revision/scripts/` contains the R scripts used to reproduce the revised main, extended data and supplementary figures.
- `revision/outputs/` contains the corresponding revised figure outputs.

Readers seeking to reproduce the figures appearing in the revised manuscript should therefore use the materials under `revision/`.

`CITATION.cff` provides citation information for this repository.

## Simulation modules

The `simulation_codes/` folder contains **20 numbered modules**. These cover the reference project-progression and pipeline-evolution analyses, post-2025 design additions, active-pipeline sensitivity analyses, post-2025 sensitivity analyses, an AMR-specific late-stage-risk sensitivity, and regional FOAK-competition analyses.

The regional FOAK-competition analysis is represented by two modules. Module 19 evaluates regional competition for the active 2025 pipeline under the Reference and Proliferation scenarios. Module 20 extends the analysis under the Proliferation scenario by integrating the active 2025 pipeline with post-2025 design additions.

Each simulation-code subfolder includes a dedicated `README.md` explaining the purpose of the module, required input workbook and expected outputs. The corresponding input workbooks are stored in `data/Simulation_Inputs/`.

The production simulations use **10,000 Monte Carlo runs**. For testing only, the scripts can be temporarily run with a smaller value of `SIMS`, for example `SIMS <- 10L` or `SIMS <- 25L`.

## Reproducing the analyses

The scripts are written for R and use common packages including `readxl`, `dplyr`, `stringr`, `purrr`, `tibble`, `writexl`, `tidyr` and `ggplot2`, with additional packages used by individual figure scripts.

The GitHub-compatible revised figure-reproduction scripts are designed to detect the repository root and read input files from the relevant `revision/data/` subfolders. Revised figure outputs are written to the corresponding locations under `revision/outputs/`.

A typical workflow is:

1. Clone or download the repository.
2. Open the repository root as the working directory in R or RStudio.
3. To reproduce figures from the revised manuscript, run the relevant script from `revision/scripts/`. To reproduce the underlying Monte Carlo analyses, use the relevant module under `simulation_codes/`.
4. Check the corresponding `revision/outputs/` or simulation-output location for exported figures, workbooks or R objects.

The top-level `scripts/` directory is retained to document the original-submission figure-reproduction workflow and should not be used as the primary source for reproducing figures in the revised manuscript.

## Large simulation outputs

Large Monte Carlo output files are not stored directly in this repository because of their size. Selected outputs required to document and reproduce the manuscript analyses are retained here.

The larger Monte Carlo output files and sensitivity-analysis summaries are archived separately on Figshare:

**Gordon et al., _Monte Carlo outputs and sensitivity-analysis summaries for First-of-a-Kind small modular reactor deployment prospects_.**

https://doi.org/10.6084/m9.figshare.32576658

The Figshare archive complements the simulation code, source data and figure-reproduction materials provided in this repository.

## Citation

If using this repository, please cite the associated manuscript and the repository citation metadata in `CITATION.cff`.
