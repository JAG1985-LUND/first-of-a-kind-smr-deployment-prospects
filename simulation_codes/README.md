# Simulation codes

This folder contains the Monte Carlo simulation modules used to assess first-of-a-kind deployment prospects for small modular reactors.

The modules cover project progression, pipeline evolution, post-2025 design additions and scenario-based sensitivity analyses. Each numbered subfolder contains a dedicated R script and README describing the module, required input workbook and expected outputs.

## Module index

1. `01_project_progression` — project-progression Monte Carlo module for active designs.
2. `02_pipeline_evolution` — pipeline-evolution module combining active designs and wider deployment trajectories.
3. `03_post_2025_design_additions` — post-2025 design-addition module.
4. `04_current_phase_completion` — current-phase completion sensitivity for the active pipeline.
5. `05_oecd_advance` — OECD licensing-acceleration sensitivity for the active pipeline.
6. `06_construction_cascade` — construction-cascade sensitivity for the active pipeline.
7. `07_late_stage_risk` — late-stage risk sensitivity for the active pipeline.
8. `08_stalled_baseline` — stalled-development sensitivity under the Baseline scenario.
9. `09_stalled_accel` — stalled-development sensitivity under the Accelerated scenario.
10. `10_post_2025_cpc_baseline` — post-2025 current-phase completion sensitivity, Baseline scenario.
11. `11_post2025_cpc_accel` — post-2025 current-phase completion sensitivity, Accelerated scenario.
12. `12_post2025_oecd_adv_baseline` — post-2025 OECD-advance sensitivity, Baseline scenario.
13. `13_post_2025_oecd_adv_accel` — post-2025 OECD-advance sensitivity, Accelerated scenario.
14. `14_post2025_cascade_baseline` — post-2025 construction-cascade sensitivity, Baseline scenario.
15. `15_post2025_cascade_accel` — post-2025 construction-cascade sensitivity, Accelerated scenario.
16. `16_post2025_late_risk_baseline` — post-2025 late-stage risk sensitivity, Baseline scenario.
17. `17_post2025_late_risk_accel` — post-2025 late-stage risk sensitivity, Accelerated scenario.
18. `18_amrs_sensitivity` — AMR-specific late-stage risk sensitivity.

## Running the scripts

The production simulations use 10,000 Monte Carlo runs. For a quick local test, temporarily reduce `SIMS`, for example to `SIMS <- 10L` or `SIMS <- 25L`.

Input workbooks are stored in `data/Simulation_Inputs/`. Outputs are written to `outputs/Simulation_Outputs/`.
