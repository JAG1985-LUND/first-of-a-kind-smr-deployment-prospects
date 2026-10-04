# Simulation codes

This directory contains the Monte Carlo simulation modules used to assess first-of-a-kind (FOAK) deployment prospects for small modular reactors (SMRs).

The simulation framework progresses from design-level project progression to global pipeline evolution, post-2025 design additions, counterfactual sensitivity analyses, and regional competition effects. The numbered modules preserve the computational structure used to generate the underlying simulation results.

Each subfolder contains a dedicated R script and README describing the module, required input workbook, and expected outputs.

## Relationship to the revised manuscript

These simulation modules were developed during the original analysis and have been retained to preserve the computational provenance of the study. The underlying code has not been retrospectively renamed solely to reflect terminology changes introduced during peer review and manuscript revision.

In particular, some module names, variables, input files, and outputs retain the original scenario terminology:

- `Baseline` corresponds to the revised **Reference scenario**.
- `Accelerated` or `Accel` corresponds to the revised **Proliferation scenario**.

The revised Reference and Proliferation terminology is used in the manuscript, revised figures, and figure-reproduction scripts. Retaining the original terminology within the simulation modules preserves the connection between the original computational workflow and the revised presentation of the results.

Revised figure source data, outputs, and reproduction scripts are provided under `revision/`.

## Simulation architecture

The modules form a linked analytical workflow.

### Design-level project progression

`01_project_progression` models progression of individual active SMR designs through the development pathway. It provides the design- and vendor-level deployment simulations used for the vendor-specific analyses and associated deployment timelines, including Supplementary Figure S15.

This module represents project progression before the wider pipeline-evolution and post-2025 design-addition components are introduced.

### Global pipeline evolution

`02_pipeline_evolution` extends the project-level simulations to the wider global FOAK pipeline.

`03_post_2025_design_additions` incorporates additional designs entering the pipeline after 2025.

Together, these modules generate the core global FOAK deployment trajectories under the scenarios subsequently presented in the revised manuscript as the **Reference** and **Proliferation** scenarios. These global simulations underpin the central scenario results and associated supplementary global and regional/technology projections, including Supplementary Figures S10 and S11.

### Counterfactual and sensitivity analyses

Modules 4–17 apply alternative assumptions to the global pipeline framework to evaluate the sensitivity of FOAK deployment outcomes to specific development and deployment conditions.

The principal sensitivities are:

- current-phase completion;
- OECD licensing acceleration;
- construction cascade;
- late-stage development risk; and
- stalled development.

The corresponding post-2025 modules apply these sensitivities to the wider pipeline under the two principal scenarios.

These simulations underpin the global counterfactual and scenario-sensitivity analyses presented in the main manuscript and Supplementary Figures S8 and S9.

### AMR sensitivity

`18_amrs_sensitivity` applies the late-stage risk sensitivity specifically to advanced modular reactors (AMRs).

This analysis is reported in Supplementary Figure S14.

### Regional FOAK competition

`19_regional_foak_competition` extends the global FOAK framework by introducing regional competition effects, whereby progression probabilities are reduced as earlier FOAKs are deployed within the same regional market.

This module underpins the regional FOAK competition sensitivity reported in **Figure 9** of the revised manuscript.

The regional analysis is therefore an extension of the global simulation framework rather than a separate deployment model.

## Module index

1. `01_project_progression` — project-progression Monte Carlo module for active designs.
2. `02_pipeline_evolution` — global pipeline-evolution module.
3. `03_post_2025_design_additions` — post-2025 design-addition module.
4. `04_current_phase_completion` — current-phase completion sensitivity for the active pipeline.
5. `05_oecd_advance` — OECD licensing-acceleration sensitivity for the active pipeline.
6. `06_construction_cascade` — construction-cascade sensitivity for the active pipeline.
7. `07_late_stage_risk` — late-stage risk sensitivity for the active pipeline.
8. `08_stalled_baseline` — stalled-development sensitivity under the Baseline scenario (revised manuscript: Reference scenario).
9. `09_stalled_accel` — stalled-development sensitivity under the Accelerated scenario (revised manuscript: Proliferation scenario).
10. `10_post_2025_cpc_baseline` — post-2025 current-phase completion sensitivity, Baseline/Reference scenario.
11. `11_post2025_cpc_accel` — post-2025 current-phase completion sensitivity, Accelerated/Proliferation scenario.
12. `12_post2025_oecd_adv_baseline` — post-2025 OECD-advance sensitivity, Baseline/Reference scenario.
13. `13_post_2025_oecd_adv_accel` — post-2025 OECD-advance sensitivity, Accelerated/Proliferation scenario.
14. `14_post2025_cascade_baseline` — post-2025 construction-cascade sensitivity, Baseline/Reference scenario.
15. `15_post2025_cascade_accel` — post-2025 construction-cascade sensitivity, Accelerated/Proliferation scenario.
16. `16_post2025_late_risk_baseline` — post-2025 late-stage risk sensitivity, Baseline/Reference scenario.
17. `17_post2025_late_risk_accel` — post-2025 late-stage risk sensitivity, Accelerated/Proliferation scenario.
18. `18_amrs_sensitivity` — AMR-specific late-stage risk sensitivity; Supplementary Figure S14.
19. `19_regional_foak_competition` — regional FOAK competition sensitivity; revised Figure 9.

## Relationship between simulation modules and figure reproduction

The simulation modules in this directory reproduce the underlying Monte Carlo analyses.

The revised figure-reproduction scripts under `revision/scripts/` perform a different function: they reproduce the presentation of the manuscript figures from the corresponding revised source-data files.

Accordingly:

- `simulation_codes/` documents the underlying computational modelling;
- `revision/data/` contains the revised figure source data;
- `revision/scripts/` contains the revised figure-reproduction scripts; and
- `revision/outputs/` contains the corresponding revised figure outputs.

This separation preserves the original simulation workflow while providing a clean reproduction pathway for the figures appearing in the revised manuscript.

## Running the simulations

Production simulations use 10,000 Monte Carlo runs. For a quick local test, `SIMS` can be temporarily reduced, for example to:

`SIMS <- 10L`

or:

`SIMS <- 25L`

Input workbooks are stored in:

`data/Simulation_Inputs/`

Simulation outputs are written to:

`outputs/Simulation_Outputs/`
