#source("scripts/0-preliminaries.R") # for package installation

library(prioritizr)
library(fst)
library(gurobi)
#library(highs) # highs or gurobi here, depending which solver is installed
library(tidyverse)
library(sf)
set.seed(123)

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
  add_manual_targets(targ_prod) 

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
  add_manual_bounded_constraints(manual_bounded_constraints)

restoration_values <- c(0.141)#, 0.30)

for (rc in restoration_values) {
  
  cat("Running scenario with restoration =", rc, "\n")
  
  # RESTORATION PROBLEM
  # overall restoration cap
  p_rest_solve <- p_rest_clean |>
    add_linear_constraints(
      threshold = nrow(pu) * rc * 1.01,
      sense = "<=",
      data = restoration
    )
  
  # lower bound (currently zero but kept for structure)
  p_rest_solve <- p_rest_solve |>
    add_linear_constraints(
      threshold = nrow(pu) * rc * 0,
      sense = ">=",
      data = restoration
    )
  
  # composition constraints
  p_rest_solve <- p_rest_solve |>
    add_linear_constraints(threshold = nrow(pu)*0.05,
                           sense = ">=",
                           data = restoration_crop) |>
    add_linear_constraints(threshold = nrow(pu)*0.05,
                           sense = ">=",
                           data = restoration_forest) |>
    add_linear_constraints(threshold = nrow(pu)*0.05,
                           sense = "<=",
                           data = restoration_natural) |>
    add_linear_constraints(threshold = nrow(pu)*0.0086,
                           sense = "<=",
                           data = restoration_wetland)
  
  cat("Running weighted sum...\n")

  weight_mat <- matrix(runif(100), ncol = 2)
  
  t_ws <- system.time({
    
    mp_ws <-
      multi_problem(restore_obj = p_rest_solve,
                    prod_obj = p_prod_solve) %>%
      add_wtd_sum_approach(
        weight_mat,
        verbose = TRUE
      ) %>%
      add_default_solver(gap = 0.2,
                        threads = cores,
                        verbose = TRUE)
    
    ms_ws <- solve(mp_ws)
  })
  
  saveRDS(
    ms_ws,
    paste0("data/outputs/multi_ws_rest_", rc, ".rds")
  )
  
  cat("Weighted sum done in", t_ws[3], "seconds\n")
  
  cat("Running hierarchical...\n")
  
  rel_tol_mat <- matrix(seq(0, 1, length.out = 50), ncol = 1)
  
  t_hier <- system.time({
    
    mp_hier <-
      multi_problem(restore_obj = p_rest_solve,
                    prod_obj = p_prod_solve) %>%
      add_hier_approach(
        rel_tol_mat,
        verbose = TRUE
      ) %>%
      add_default_solver(gap = 0.2,
                        threads = cores,
                        verbose = TRUE)
    
    ms_hier <- solve(mp_hier)
  })
  
  saveRDS(
    ms_hier,
    paste0("data/outputs/multi_hier_rest_", rc, ".rds")
  )
  
  cat("Hierarchical done in", t_hier[3], "seconds\n\n")
}