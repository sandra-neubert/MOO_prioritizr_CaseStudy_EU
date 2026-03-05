library(prioritizr)
library(fst)
library(gurobi)
library(tidyverse)
library(sf)

cores <- 3
fst::threads_fst(nr_of_threads = cores, reset_after_fork = NULL) # Set number for fst

# LOAD DATA
pu_in_EU <- read_csv("data/formatted-data/pu_in_EU.csv")

pu <- read_fst("data/formatted-data/pu_data.fst") |>
  left_join(pu_in_EU) |>
  rename(id = EU_id) |>
  dplyr::select(-c(pu, nuts2id)) |>
  drop_na(id)

rij_all <- readRDS("data/formatted-data/rij_all_sn.rds")
feat_all <- readRDS("data/formatted-data/feat_all_sn.rds")
targ_prod <- readRDS("data/formatted-data/targ_prod_sn.rds")
targ_restore <- readRDS("data/formatted-data/targ_restore_sn.rds")

cost_columns <- colnames(pu)[1:(ncol(pu)-1)]

z <- read_csv("data/formatted-data/zone_id.csv") |>
  mutate(name = paste0("z", id)) |>
  relocate(id, 1) |>
  dplyr::select(-zone)

# CONSTRAINTS
manual_bounded_constraints <- read_csv("data/formatted-data/manual_bounded_constraints_CLC_adj.csv")

# restoration
pu_restoration_budget_data <- read_csv("data/formatted-data/linear_constraints/pu_restoration_budget_data.csv") |>
  rename(pu = layer) |>
  left_join(pu_in_EU) |>
  mutate(pu = EU_id) |>
  drop_na(pu) |>
  dplyr::select(-c(id, cost, nuts2id, EU_id)) |>
  mutate(z8 = 0)

# restoration budgets
restoration <- pu_restoration_budget_data |>
  dplyr::select(-c(pu)) |>
  as.matrix() |>
  replace_na(0)

# Restoration budget breakdown
pu_restore_natural_budget_data <- read_csv("data/formatted-data/linear_constraints/pu_restore_natural_budget_data.csv")

pu_restore_forest_budget_data <- read_csv("data/formatted-data/linear_constraints/pu_restore_forest_budget_data.csv")

pu_restore_crop_budget_data <- read_csv("data/formatted-data/linear_constraints/pu_restore_crop_budget_data.csv")

pu_wetland_rest_budget_data <- read_csv("data/formatted-data/linear_constraints/pu_wetland_rest_budget_data.csv")

restoration_crop <- pu_restore_crop_budget_data |>
  dplyr::select(-c(pu)) |>
  as.matrix() |>
  replace_na(0)

restoration_forest <- pu_restore_forest_budget_data |>
  dplyr::select(-c(pu)) |>
  as.matrix() |>
  replace_na(0)

restoration_natural <- pu_restore_natural_budget_data |>
  dplyr::select(-c(pu)) |>
  as.matrix() |>
  replace_na(0)

restoration_wetland <- pu_wetland_rest_budget_data |>
  dplyr::select(-c(pu, NUTS_ID)) |>
  as.matrix() |>
  replace_na(0)

# area constraint
restoration_constraint <- 0.141

# SINGLE-OBJECTIVE PROBLEMS
# 1. Production objective
p_prod <- problem(x = pu,
                  features = feat_all,
                  zones = z,
                  cost_column = cost_columns,
                  rij = rij_all) |>
  add_min_shortfall_objective(nrow(pu))

p_prod_solve <- p_prod |>
#  add_binary_decisions() |>
  add_proportion_decisions() |>
  add_manual_targets(targ_prod) |>
  add_gurobi_solver(gap = 0.2, threads = cores,
                    verbose = TRUE)

#s_prod <- solve(p_prod_solve)

# 2. Restoration objective
p_rest <- problem(x = pu,
                  features = feat_all,
                  zones = z,
                  cost_column = cost_columns,
                  rij = rij_all) |>
  add_min_shortfall_objective(nrow(pu))

p_rest_clean <- p_rest |>
  # add_binary_decisions() |>
  add_proportion_decisions() |>
  add_manual_targets(targ_restore) |>
  add_gurobi_solver(gap = 0.2, threads = cores,
                    verbose = TRUE) |>
  add_manual_bounded_constraints(manual_bounded_constraints)

# area constraint
restoration_constraint <- 0.141

# no more than a bit over the restoration goal of ~14%
p_rest_solve <- p_rest_solve |>
  add_linear_constraints(threshold = nrow(pu)*restoration_constraint*1.01,
                         sense = "<=",
                         data = restoration)

# no less than restoration 25% restoration target
p_rest_solve <- p_rest_solve |>
  add_linear_constraints(threshold = nrow(pu)*restoration_constraint*0,
                         sense = ">=",
                         data = restoration)

p_rest_solve <- p_rest_solve |>
  add_linear_constraints(threshold = nrow(pu)*0.05,
                         sense = ">=",
                         data = restoration_crop) |>
  add_linear_constraints(threshold = nrow(pu)*0.05,
                         sense = ">=",
                         data = restoration_forest)|>
  add_linear_constraints(threshold = nrow(pu)*0.05,
                         sense = "<=",
                         data = restoration_natural)|>
  add_linear_constraints(threshold = nrow(pu)*0.0086,
                         sense = "<=",
                         data = restoration_wetland)


#s_rest <- solve(p_rest_solve)

# MULTI-OBJECTIVE PROBLEMS
# 1. weighted sum
set.seed(123)
weight_mat <- matrix(runif(100), ncol = 2)

t1 <- system.time({
  mp1 <-
    multi_problem(restore_obj = p_rest_solve, prod_obj = p_prod_solve) %>%
    add_weighted_sum_approach(
     weight_mat,
      verbose = TRUE
    ) %>%
    add_gurobi_solver(gap = 0.2, threads = cores,
                      verbose = TRUE)
  
  # solve problem
  ms1 <- solve(mp1)
})

t1

saveRDS(ms1, "data/outputs/multi_ws_rest_constr_sn.rds")

# 2. hierarchical
rel_tol_mat <- matrix(seq(0, 1, length.out = 50), ncol = 1)

t2 <- system.time({
  mp2 <-
    multi_problem(restore_obj = p_rest_solve, prod_obj = p_prod_solve) %>%
    add_rel_constraint_approach(
      rel_tol_mat,
      verbose = TRUE
    ) %>%
    add_gurobi_solver(gap = 0.2, threads = cores,
                      verbose = TRUE)
  
  # solve problem
  ms2 <- solve(mp2)
})

t2

saveRDS(ms2, "data/outputs/multi_hier_rest_constr_sn.rds")
