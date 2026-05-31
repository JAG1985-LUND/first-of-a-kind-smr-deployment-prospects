# Construction cascade sensitivity Monte Carlo

This folder contains the Monte Carlo code used to simulate the construction cascade sensitivity.

This sensitivity complements the project-progression and pipeline-evolution modules by testing a counterfactual in which construction-stage failure risks are adjusted to reflect cascading risks among designs that reach late-stage project development. The model retains the same project-progression structure as the main simulation, including phase-duration draws, failure gating and baseline/accelerated scenario logic.

## Source script

- `construction_cascade_monte_carlo.R`

## Input workbook

- `data/Simulation_Inputs/construction_cascade_sensitivity_inputs.xlsx`

## Input workbook sheets

The input workbook contains four sheets:

- `smr_design_inputs`
- `phase_rules`
- `design_phase_failures`
- `construction_cascade_failures`

## Expected outputs

Running the script generates construction cascade sensitivity outputs, including:

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

- `outputs/simulation_outputs/construction_cascade_sensitivity/`

Full large Monte Carlo output files are deposited on Figshare because they exceed practical GitHub size limits. This repository contains the input workbook and source code needed to reproduce the outputs.
