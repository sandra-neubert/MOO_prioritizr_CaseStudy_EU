library(fst)
library(tidyverse)
library(sf)

cores <- 3
fst::threads_fst(nr_of_threads = cores, reset_after_fork = NULL) # Set number for fst


####### Data setup ##########

## PUs
pu_in_EU <- read_csv("data/formatted-data/pu_in_EU.csv")

pu <- read_fst("data/formatted-data/pu_data.fst") |>
  left_join(pu_in_EU) |>
  rename(id = EU_id) |>
  dplyr::select(-c(pu, nuts2id)) |>
  drop_na(id)

## FEATURES
# features are split by zone where
# zone 1-11: Restoration
# zone 12-18: Production - the features here only contain biodiversity features, NO PRODUCTION SPECIES
# zone 19-25: Conservation
rij <- read_fst("data/formatted-data/features_split.fst") |>
  rename(species = feature) |>
  # mutate(amount = round(amount)) |>
  left_join(pu_in_EU) |>
  mutate(pu = EU_id) |>
  dplyr::select(pu, species, zone, amount) |>
  drop_na(pu) |>
  mutate(amount = ifelse(amount < 0.001, 0, amount)) |>
  mutate(amount = replace_na(amount, 0))

feat_rij <- data.frame(
  id = unique(rij$species),
  prop = 1
)

# carbon_id
carbon_id <- data.frame(
  name = "carbon",
  id = 999999
)

feat <- feat_rij |>
  # left_join(species_id) |>
  mutate(name = as.factor(id)) |>
  dplyr::select(id, name, prop) |>
  drop_na(id) |>
  drop_na(name)

# production features
prod_feat <- readRDS("data/formatted-data/production_features_split_ref_sn.rds")

# Combine all results into one big rij and targets table
rij_production <- bind_rows(lapply(prod_feat, `[[`, "rij"))
targets_production <- bind_rows(lapply(prod_feat, `[[`, "targets"))

# change names to ID number
prod_feat_names <- tibble(
  feature = sort(
    unique(targets_production$feature)
  )
) |>
  mutate(id = seq_len(n()))

rij_production <- rij_production |>
  left_join(prod_feat_names, by = c("species" = "feature")) |>
  mutate(species = id) |>
  dplyr::select(-id)

targets_production <- targets_production |>
  left_join(prod_feat_names, by = "feature") |>
  mutate(feature = id) |>
  dplyr::select(
    feature,
    target,
    sense,
    type,
    zone
  )

# ZONES
# zone 1-11: Restoration
# zone 12-18: Production
# zone 19-25: Conservation
# additional: zone 26: lock-in urban areas
z <- read_csv("data/formatted-data/zone_id.csv") |>
  mutate(name = paste0("z", id)) |>
  relocate(id, 1) |>
  dplyr::select(-zone)

# BOUNDED CONSTRAINTS
# # zone ids for this.

zones <- read_csv("data/formatted-data/zone_id.csv") |>
  mutate(name = paste0("z", id))

# FEATURE TARGETS
names <- readRDS("data/formatted-data/SDMNameMatching.rds") |>
  mutate(speciesname = current_sname) |>
  rename(id = taxon_id) |>
  dplyr::select(id, speciesname)

names$speciesname <- sub(" ", "_", names$speciesname)

targs_existing <- read_csv("data/formatted-data/targets_split.csv") |>
  mutate(zone = list(z$name)) |>
  # dplyr::filter(feature %in% feat$name) |>
  mutate(
    target = ifelse(target > 100000, target / 100, target),
    target = ifelse(target < 0.001, 0, target)
  )

# to line up with rij spp
targs_default <- data.frame(
  feature = setdiff(feat$name, targs_existing$feature)
) |>
  mutate(
    target = 22,
    sense = ">=",
    type = "absolute",
    zone = list(targs_existing$zone[[1]])
  )

targs <- targs_existing |>
  mutate(feature = as.factor(feature)) |>
  bind_rows(targs_default) |>
  mutate(weight = 1) #|>
# dplyr::select(-id)

targs_rm <- data.frame(
  feature = setdiff(targs$feature, feat$id)
) |>
  mutate(remove = TRUE)

targs_biodiv_features <- targs |>
  left_join(targs_rm) |>
  mutate(remove = replace_na(remove, FALSE)) |>
  filter(remove == FALSE) |>
  dplyr::select(-remove)

targs_filtered <- targs |> dplyr::select(-weight)

# COMBINE Production info with existing info
targ_all <- rbind(targs_filtered, targets_production)
targ_prod <- rbind(targs_filtered |>
  dplyr::mutate(target = 0), targets_production) # all biodiversity set to zero
targ_restore <- rbind(targs_filtered, targets_production |>
  dplyr::mutate(target = 0)) # all prod set to zero

rij_all <- rbind(rij, rij_production)

feat_all <- data.frame(
  id = unique(rij_all$species),
  prop = 1
) |>
  # left_join(species_id) |>
  mutate(name = as.factor(id)) |>
  dplyr::select(id, name, prop) |>
  drop_na(id) |>
  drop_na(name)

missing_features <- setdiff( # there are currently some features missing from the rij matrix that are in the targets!!
  unique(targ_restore$feature),
  unique(feat_all$id)
)

# remove from targets for now
targ_prod <- targ_prod |>
  dplyr::filter(!feature %in% missing_features)

targ_restore <- targ_restore |>
  dplyr::filter(!feature %in% missing_features)

targ_all <- targ_all |>
  dplyr::filter(!feature %in% missing_features)

feat_all <- data.frame(
  id = unique(rij_all$species),
  prop = 1
) |>
  # left_join(species_id) |>
  mutate(name = as.factor(id)) |>
  dplyr::select(id, name, prop) |>
  drop_na(id) |>
  drop_na(name)

saveRDS(targ_all, "data/formatted-data/targ_all_sn.rds")
saveRDS(rij_all, "data/formatted-data/rij_all_sn.rds")
saveRDS(feat_all, "data/formatted-data/feat_all_sn.rds")
saveRDS(targ_prod, "data/formatted-data/targ_prod_sn.rds")
saveRDS(targ_restore, "data/formatted-data/targ_restore_sn.rds")
