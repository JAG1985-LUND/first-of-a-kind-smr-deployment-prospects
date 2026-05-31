# Late-stage risk sensitivity Monte Carlo

This folder contains the Monte Carlo code used to simulate the late-stage risk sensitivity.

This sensitivity complements the project-progression and pipeline-evolution modules by testing a counterfactual in which late-stage project risks are adjusted for designs reaching licensing and construction. The model retains the same project-progression structure as the main simulation, including phase-duration draws, failure gating and baseline/accelerated scenario logic.

## Source script

- `late_stage_risk_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/late_stage_risk_sensitivity_inputs.xlsx`

## Input workbook sheets

The input workbook contains four sheets:

- `smr_design_inputs`
- `phase_rules`
- `design_phase_failures`
- `late_stage_risk_failure_rates`

## Expected outputs

Running the script generates late-stage risk sensitivity outputs, including:

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

- `outputs/simulation_outputs/late_stage_risk_sensitivity/`

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.
