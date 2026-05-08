## Prelims

#install packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf, fst, purrr, patchwork, ggcorrplot, irr, remotes)

# The multi-objective framework used in this case study is currently available 
# in a developmental branch of prioritizr and will be included in the main package
# release soon. 
# Please install it directly from GitHub:
library(remotes)
remotes::install_github("prioritizr/prioritizr@new-multiobj_hierarchical-v2")

# Please note that a solver needs to be installed for solving optimization problems

# There are different solvers available and they vary depending on their accessibility, speed, and performance.
# We used the Gurobi solver. For intallation help, please see: https://prioritizr.net/articles/gurobi_installation_guide.html
# if you have access to the Gurobi solver, please run the following code:
# see here for the installation of the gurobi R package: https://docs.gurobi.com/projects/optimizer/en/current/reference/r/setup.html
# library(gurobi)

# Otherwise please use the free HiGHs solver (or one of the other solvers available wit prioritizr):
install.packages("highs") 
library(highs)

# Information on solvers can be found here: https://prioritizr.net/articles/solver_benchmarks.html