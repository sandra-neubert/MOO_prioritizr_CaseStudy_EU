# Advancing multi-objective optimization in spatial planning

This repository contains the code for the EU case study presented in the 
manuscript **"Advancing multi-objective optimization in spatial planning"**. 
The study presents a generic and flexible multi-objective optimization framework,
which is implemented directly within the [prioritizr](https://prioritizr.net/) R package
and enables spatial planners to explicitly explore trade-offs and synergies 
across multiple objectives, rather than collapsing them into a single combined formulation. 
Importantly, it is not specific to this case study and can be applied to a wide range of 
terrestrial, marine, and freshwater spatial planning problems with multiple competing objectives.
By reducing technical barriers to multi-objective spatial optimization, the framework aims to make 
structured exploration of trade-offs and co-benefits more accessible to conservation scientists, planners, 
and practitioners.


Using a multiple-use case study in the European Union, 
the framework is applied in this repository to identify spatial solutions that 
simultaneously balance biodiversity restoration, 
with production, and broader policy goals aligned with the European Green Deal.


Raw input data are from [Chapman et al. (2025)](https://www.nature.com/articles/s41559-025-02671-1), whose underlying data and code are available at [https://doi.org/10.5281/zenodo.14720069](https://doi.org/10.5281/zenodo.14720069).

## Code structure

All scripts are in the `/scripts/` folder and should be run in order:

**`1-production-features.R`**: Formats production features and constructs NUTS2-level subnational targets for cropland (low, medium, high intensity), pasture (low, high intensity), and forestry (multifunctional, production) land-use types, used as constraints in the optimization.

**`2-input-data.R`**: Loads and prepares all input data, including planning units, zone-feature contribution matrices (`rij`), and biodiversity and production feature targets. Saves compiled inputs to `data/formatted-data/` for use in subsequent scripts.

**`3-problem-formulation.R`**: Formulates and solves the multi-objective spatial optimization problems using `prioritizr`, including weighted-sum and hierarchical formulations across a range of weight combinations to trace the Pareto front. Solver outputs are saved to `data/outputs/`.

**`4-format-solutions.R`**: Processes solver outputs — computes objective values and feature shortfalls, identifies Pareto-efficient solutions and elbow points, and formats results as spatial objects. Formatted outputs are saved to `data/outputs/formatted/`.

**`5-figures.R`**: Generates all main manuscript figures, including the Pareto front and spatial maps of selected solutions (restoration-focused, equal trade-offs, production-focused).

**`6-figures-supp.R`**: Generates supplementary figures.

## Solvers

Scripts use [Gurobi](https://www.gurobi.com/) as the default LP solver (proprietary; free academic licenses available). Problems can also be solved with open-source alternatives such as [HiGHS](https://highs.dev/); see the [prioritizr solver documentation](https://prioritizr.net/reference/solvers.html) for supported solvers.

## Data availability

Formatted data required to run the optimization (`3-problem-formulation.R`) is available in `data/formatted-data/`. Raw input data underlying steps 1–2 are from Chapman et al. (2025), available at [https://doi.org/10.5281/zenodo.14720069](https://doi.org/10.5281/zenodo.14720069).
