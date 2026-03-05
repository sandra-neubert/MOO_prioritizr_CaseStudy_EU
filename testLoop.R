restoration_values <- c(0.30)#0.141)#, 0.30)

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
  
  # cat("Running weighted sum...\n")
  # 
  # set.seed(123)
  # weight_mat <- matrix(runif(100), ncol = 2)
  # 
  # t_ws <- system.time({
  #   
  #   mp_ws <-
  #     multi_problem(restore_obj = p_rest_solve,
  #                   prod_obj = p_prod_solve) %>%
  #     add_weighted_sum_approach(
  #       weight_mat,
  #       verbose = TRUE
  #     ) %>%
  #     add_gurobi_solver(gap = 0.2,
  #                       threads = cores,
  #                       verbose = TRUE)
  #   
  #   ms_ws <- solve(mp_ws)
  # })
  # 
  # saveRDS(
  #   ms_ws,
  #   paste0("data/outputs/multi_ws_rest_", rc, ".rds")
  # )
  # 
  # cat("Weighted sum done in", t_ws[3], "seconds\n")

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
      add_gurobi_solver(gap = 0.2,
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