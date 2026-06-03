# Stalled development accelerated sensitivity Monte Carlo

This folder contains the Monte Carlo code used to simulate the accelerated stalled development sensitivity.

This sensitivity complements the project-progression and pipeline-evolution modules by testing a more permissive stalled-development case. The accelerated stalled-development filter retains 56 active small modular reactor designs and excludes only designs whose elapsed time in their current phase exceeds the accelerated progression threshold. The model then simulates FOAK deployment from this reduced active pipeline using accelerated project-progression assumptions.

## Source script

- `stalled_development_accelerated_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/stalled_development_sensitivity_inputs_accelerated.xlsx`

## Input workbook sheets

The input workbook contains four sheets:

- `stalled_develop_accel_sample`
- `phase_rules_stalled_develop_acc`
- `design_phase_failures`
- `failures_stalled_develop_accel`

## Expected outputs

Running the script generates accelerated stalled-development sensitivity outputs, including:

- accelerated Monte Carlo draws
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

- `outputs/simulation_outputs/stalled_development_accelerated_sensitivity/`

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.
