library(tidyverse)
library(sf)


# ## PRODUCTION FEATURES
reshape_production_budget_data <- function(
    file_path,
    dataset_name,
    nuts2_names,
    nuts2_targets_df, # e.g. nuts2_crop_low
    target_col = "value") {
  
  land_use <- gsub("^pu_|_budget_data$", "", dataset_name)

  zone_id <- production_zone_key[[paste0(land_use)]]
  stopifnot(!is.null(zone_id))

  # read once
  pu_budget <- read_csv(file_path)

  rij_list <- list()
  target_list <- list()

  print(dataset_name)

  for (i in seq_along(nuts2_names)) {
    print(i)

    nuts2 <- nuts2_names[[i]]

    # TARGETS
    nuts_target <- nuts2_targets_df |>
      filter(NUTS_ID == nuts2) |>
      mutate(
        target = .data[[target_col]]#ifelse(.data[[target_col]] < 1, 0, .data[[target_col]])
      ) |>
      pull(target)

    if (length(nuts_target) == 0 || nuts_target == 0 || is.na(nuts_target)) next

    feature_name <- paste0(land_use, "_", nuts2)

    target_list[[feature_name]] <- tibble(
      feature = feature_name,
      target  = nuts_target,
      sense   = ">=",
      type    = "absolute",
      zone    = list(zone_id)
    )

    # FEATURE MAT
    rij_nuts <- pu_budget |>
      dplyr::select(c("pu", "z12", "z13", "z14", "z15", "z16", "z17", "z18", "NUTS_ID")) %>%
      mutate(m = ifelse(NUTS_ID == nuts2, 1, 0)) |>
      mutate(across(starts_with("z"), ~ .x * m)) |>
      select(-m, -NUTS_ID) |>
      pivot_longer(
        cols = starts_with("z"),
        names_to = "zone",
        values_to = "amount"
      ) |>
      mutate(
        zone = as.integer(sub("z", "", zone)),
        species = feature_name
      ) |>
      filter(amount != 0) |>
      select(pu, species, zone, amount)

    rij_list[[feature_name]] <- rij_nuts
  }

  list(
    rij     = bind_rows(rij_list),
    targets = bind_rows(target_list)
  )
}

files <- list(
  pu_cropland_low_budget_data  = "data/formatted-data/linear_constraints/pu_crop_low_budget_data.csv",
  pu_cropland_med_budget_data  = "data/formatted-data/linear_constraints/pu_crop_med_budget_data.csv",
  pu_cropland_high_budget_data = "data/formatted-data/linear_constraints/pu_crop_high_budget_data.csv",
  pu_pasture_high_budget_data  = "data/formatted-data/linear_constraints/pu_pasture_high_budget_data.csv",
  pu_pasture_low_budget_data   = "data/formatted-data/linear_constraints/pu_pasture_low_budget_data.csv",
  pu_forest_multi_budget_data  = "data/formatted-data/linear_constraints/pu_forest_multi_budget_data.csv",
  pu_forest_prod_budget_data   = "data/formatted-data/linear_constraints/pu_forest_prod_budget_data.csv"
)

# corresponding NUTS2 target data frames (already read in)
targets_list <- list(
  pu_cropland_low_budget_data  = read_csv("data/formatted-data/linear_constraints/nuts_crop_low_ref_proportional.csv"),
  pu_cropland_med_budget_data  = read_csv("data/formatted-data/linear_constraints/nuts_crop_med_ref_proportional.csv"),
  pu_cropland_high_budget_data = read_csv("data/formatted-data/linear_constraints/nuts_crop_high_ref_proportional.csv"),
  pu_pasture_high_budget_data  = read_csv("data/formatted-data/linear_constraints/nuts_pasture_high_ref_proportional.csv"),
  pu_pasture_low_budget_data   = read_csv("data/formatted-data/linear_constraints/nuts_pasture_low_ref_proportional.csv"),
  pu_forest_multi_budget_data  = read_csv("data/formatted-data/linear_constraints/nuts_forest_multi_ref_proportional.csv"),
  pu_forest_prod_budget_data   = read_csv("data/formatted-data/linear_constraints/nuts_forest_prod_ref_proportional.csv") # ,
  # pu_wetland_rest_budget_data  = nuts2_wetland_rest
)

production_zone_key <- c(
  forest_multi = "z12",
  forest_prod = "z13",
  pasture_high = "z14",
  pasture_low = "z15",
  cropland_med = "z16",
  cropland_low = "z17",
  cropland_high = "z18"
)


# NUTS2 names
nuts2_names <- unique(targets_list$pu_cropland_low_budget_data$NUTS_ID)

# Loop over all files + targets
all_results <- lapply(names(files), function(dataset_name) {
  reshape_production_budget_data(
    file_path = files[[dataset_name]],
    dataset_name = dataset_name,
    nuts2_names = nuts2_names,
    nuts2_targets_df = targets_list[[dataset_name]],
    target_col = "value"
  )
})

saveRDS(all_results, "data/formatted-data/production_features_split_ref_sn.rds")
gc()
