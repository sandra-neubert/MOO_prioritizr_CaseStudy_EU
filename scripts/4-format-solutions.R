library(tidyverse)
library(sf)
library(purrr)

# LOAD DATA
PU_sf <- readRDS("data/formatted-data/PU_sf_sn.rds")

# PREP FUNCTION
prep_solution_output <- function(s, PU_sf, mode = "dominant") {
  
  mode <- match.arg(mode)
  
  n_solutions <- max(
    str_extract(names(s), "(?<=solution_)\\d+") |> as.integer(),
    na.rm = TRUE
  )
  
  print(paste0("Number of solutions: ", n_solutions))
  
  zone_to_use <- function(z) {
    case_when(
      z >= 1  & z <= 11 ~ "restoration",
      z >= 12 & z <= 18 ~ "production",
      z >= 19 & z <= 25 ~ "conservation",
      z == 26           ~ "urban_area",
      TRUE              ~ "not_selected"
    )
  }
  
  # detect binary vs proportional
  sol_cols <- names(s)[str_detect(names(s), "^solution_\\d+_z\\d+$")]
  
  test_vals <- s %>%
    select(all_of(sol_cols)) %>%
    unlist() %>%
    unique()
  
  is_binary <- all(test_vals %in% c(0,1))
  
  print(paste("Binary input:", is_binary))
  
  # ---------------------------
  # extraction functions
  # ---------------------------
  
  extract_binary <- function(df, sol_id) {
    
    cols <- paste0("solution_", sol_id, "_z")
    
    df %>%
      mutate(
        !!paste0("solution_", sol_id) :=
          pmap_int(
            dplyr::select(., starts_with(cols)),
            ~{
              values <- c(...)
              idx <- which(values == 1)
              if (length(idx) == 0) return(0)
              idx[1]
            }
          )
      ) %>%
      select(id, !!paste0("solution_", sol_id)) %>%
      mutate(
        !!paste0("solution_", sol_id) :=
          zone_to_use(.data[[paste0("solution_", sol_id)]])
      )
  }
  
  extract_prop_dominant <- function(df, sol_id) {
    
    sol_cols <- paste0("solution_", sol_id, "_z")
    
    df %>%
      select(id, starts_with(sol_cols)) %>%
      pivot_longer(
        cols = starts_with(sol_cols),
        names_to = "zone",
        values_to = "value"
      ) %>%
      mutate(
        zone = as.integer(str_extract(zone, "\\d+$")),
        use  = zone_to_use(zone)
      ) %>%
      group_by(id, use) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      group_by(id) %>%
      summarise(
        !!paste0("solution_", sol_id) :=
          if (all(value == 0)) "not_selected"
        else use[which.max(value)],
        .groups = "drop"
      )
  }
  
  extract_prop_share <- function(df, sol_id) {
    
    sol_cols <- paste0("solution_", sol_id, "_z")
    
    df %>%
      select(id, starts_with(sol_cols)) %>%
      pivot_longer(
        cols = starts_with(sol_cols),
        names_to = "zone",
        values_to = "value"
      ) %>%
      mutate(
        zone = as.integer(str_extract(zone, "\\d+$")),
        use  = zone_to_use(zone)
      ) %>%
      group_by(id, use) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      group_by(id) %>%
      mutate(prop = value / sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      select(id, use, prop) %>%
      pivot_wider(
        names_from = use,
        values_from = prop,
        values_fill = 0
      ) %>%
      rename_with(~ paste0("solution_", sol_id, "_", .x), -id)
  }
  
  # choose extractor
  
  if (mode == "proportions") {
    
    if (is_binary) {
      stop("Proportional mode requested but solutions are binary.")
    }
    
    extractor <- extract_prop_share
    
  } else {
    
    extractor <- if (is_binary) extract_binary else extract_prop_dominant
  }
  
  # build solution table
  s_short <- reduce(
    seq_len(n_solutions),
    ~ left_join(.x, extractor(s, .y), by = "id"),
    .init = s %>% select(id)
  )
  
  s_sf <- PU_sf %>%
    left_join(s_short, by = "id")
  
  # objectives + selection
  obj_vals <- attr(s, "objective")
  
  obj_df <- as.data.frame(obj_vals)
  obj_df$solution_id <- seq_len(nrow(obj_df))
  
  sol_min_rest <- obj_df$solution_id[which.min(obj_df[[1]])]
  sol_min_prod <- obj_df$solution_id[which.min(obj_df[[2]])]
  
  obj_df$dist_to_center <- scale(obj_df[[1]])^2 + scale(obj_df[[2]])^2
  
  mid_solution <- obj_df %>%
    arrange(dist_to_center) %>%
    slice(2) %>%
    pull(solution_id)
  
  selected_solutions <- c(sol_min_rest, sol_min_prod, mid_solution)
  
  sel_df <- obj_df %>%
    filter(solution_id %in% selected_solutions) %>%
    arrange(match(solution_id, selected_solutions)) %>%
    mutate(
      sol_label = c("Restoration-focused",
                    "Production-focused",
                    "Equal weighting"),
      sol_label = factor(sol_label, levels = sol_label),
      sol_colour = c("#009E73", "#0072B2", "#D55E00")
    )
  
  return(list(
    obj_df = obj_df,
    sel_df = sel_df,
    s_sf = s_sf
  ))
}

#FORMAT DATA
s <- readRDS("data/outputs/ms3_test_sn.rds")
s_formatted <- prep_solution_output(s, PU_sf)
s_formatted_prop <- prep_solution_output(s, PU_sf, mode = "proportions")

saveRDS(s_formatted, "data/outputs/ms3_test_sn_formatted.rds")
saveRDS(s_formatted_prop, "data/outputs/ms3_test_sn_formatted_prop.rds")

