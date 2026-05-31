# Current-phase completion sensitivity Monte Carlo

This folder contains the Monte Carlo code used to simulate the current-phase completion sensitivity.

This sensitivity complements the project-progression and pipeline-evolution modules by testing a counterfactual in which each active small modular reactor design is guaranteed to survive the development phase it occupies at the 2025 cut-off. Designs still draw the normal duration for their current phase, and normal failure exposure resumes once they enter the next development phase.

## Source script

- `current_phase_completion_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/current_phase_completion_sensitivity_inputs.xlsx`

## Input workbook sheets

The input workbook contains four sheets:

- `smr_design_inputs`
- `phase_rules_completion`
- `design_phase_failures`
- `failure_rates_smr`

## Expected outputs

Running the script generates current-phase completion sensitivity outputs, including:

- baseline and accelerated Monte Carlo draws
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

- `outputs/simulation_outputs/current_phase_completion_sensitivity/`

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.
