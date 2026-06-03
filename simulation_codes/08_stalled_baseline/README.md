# Stalled development baseline sensitivity Monte Carlo

This folder contains the Monte Carlo code used to simulate the baseline stalled development sensitivity.

This sensitivity complements the project-progression and pipeline-evolution modules by testing a pessimistic stalled-development case. The baseline stalled-development filter retains 46 active small modular reactor designs and excludes designs whose elapsed time in their current phase substantially exceeds empirically observed progression windows. The model then simulates FOAK deployment from this reduced active pipeline using the baseline project-progression assumptions.

## Source script

- `stalled_development_baseline_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/stalled_development_sensitivity_inputs_baseline.xlsx`

## Input workbook sheets

The input workbook contains four sheets:

- `stalled_development_bas_sample`
- `phase_rules_stalled_develop_bas`
- `design_phase_failures`
- `failures_stalled_develop_bas`

## Expected outputs

Running the script generates baseline stalled-development sensitivity outputs, including:

- baseline Monte Carlo draws
- design-level FOAK deployment summaries
- annual global FOAK deployment summaries
- annual OECD, water-cooled, cooling-type and regional summaries
- 2050 snapshot summaries
- run metadata documenting the seed, simulation count and input workbook

## Simulation count

The script is configured for the full manuscript-scale run with:

- `SIMS <- 10000L`

For a quick repository smoke test, users may temporarily set:

- `SIMS <- 25L`

## Output location

The script writes outputs to:

- `outputs/simulation_outputs/stalled_development_baseline_sensitivity/`

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.
