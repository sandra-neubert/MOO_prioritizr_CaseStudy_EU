library(tidyverse)
library(patchwork)
library(sf)
library(irr)
library(ggcorrplot)

s_formatted <- readRDS("data/outputs/ms3_test_sn_formatted.rds")
obj_df <- s_formatted$obj_df
sel_df <- s_formatted$sel_df
s_sf <- s_formatted$s_sf

######## Pareto front plot
sol_order <- c("Restoration-focused", "Equal weighting", "Production-focused")

sel_df <- sel_df %>%
  mutate(
    sol_label = factor(sol_label, levels = sol_order)
  ) %>%
  arrange(sol_label)

pareto_plot <- ggplot(obj_df, aes(x = restore_obj, y = prod_obj)) +
  geom_point(color = "grey65", size = 2.5) +
  geom_point(
    data = sel_df,
    aes(color = sol_label),
    size = 3.5
  ) +
  scale_color_manual(
    values = setNames(sel_df$sol_colour, sel_df$sol_label),
    breaks = sol_order
  ) +
  labs(
    title = "Approximated pareto front with selected solutions",
    x = "Restoration objective (shortfall)",
    y = "Production objective (shortfall)",
    color = "Selected solutions"
  ) +
  theme_bw()

agg_cols <- grep("^solution_\\d+$", names(s_sf), value = TRUE)

s_sf <- s_sf %>%
  mutate(across(all_of(agg_cols),
                ~ factor(.x, levels = c("restoration",
                                        "production",
                                        "conservation",
                                        "urban_area",
                                        "not_selected"))))


fill_scale <- scale_fill_manual(
  values = c(
    "restoration"  = "#440154",
    "production"   = "#31688e",
    "conservation" = "#35b779",
    "urban_area"   = "#fde725",
    "not_selected" = "grey70"
  ),
  breaks = c("restoration","production","conservation","urban_area"),
  labels = c("Restoration","Production","Conservation","Urban areas"),
  name = "Aggregated zones",
  drop = FALSE
)

plot_solution_map <- function(sol_id, col, title) {
  
  sol_col <- paste0("solution_", sol_id)
  
  ggplot(s_sf) +
    geom_sf(aes(fill = .data[[sol_col]]), color = NA) +
    fill_scale +
    ggtitle(title) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.border = element_rect(
        colour = col,
        fill = NA,
        linewidth = 2
      ),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

maps <- Map(
  plot_solution_map,
  sol_id = sel_df$solution_id,
  col    = sel_df$sol_colour,
  title  = sel_df$sol_label
)

names(maps) <- sel_df$sol_label


final_plot <- pareto_plot /
  ((maps[["Restoration-focused"]] + theme(legend.position="none")) |
     (maps[["Equal weighting"]]) |
     (maps[["Production-focused"]] + theme(legend.position="none"))) +
  plot_annotation(tag_levels = 'A')

ggsave("figures/plot_pf_maps_constraints_sn.png",
       final_plot, width = 8.5, height = 6.5, dpi = 300)

###### Corr plot
sol_cols <- paste0("solution_", sel_df$solution_id)

# extract relevant columns from spatial object
sol_sf <- s_sf %>%
  st_drop_geometry()

sol_sf_list <- lapply(seq_along(sol_cols), function(i) {
  sol_sf %>%
    # st_drop_geometry() %>%
    dplyr::select(all_of(sol_cols[i])) %>%
    rename(solution_1 = !!sol_cols[i]) %>%
    mutate(solution_1 = as.factor(solution_1)) #%>%
  # st_as_sf()  # reattach sf class for compatibility
})

# Calc Cohen's Kappa matrix 
n <- length(sol_sf_list)
kappa_mat <- matrix(NA, nrow = n, ncol = n)

for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    df_pair <- bind_cols(
      sol_sf_list[[i]]$solution_1,
      sol_sf_list[[j]]$solution_1
    )
    colnames(df_pair) <- c("r1", "r2")
    
    kappa_mat[i, j] <- irr::kappa2(df_pair)$value
  }
}


label_lookup <- sel_df %>%
  mutate(sol_col = paste0("solution_", solution_id)) %>%
  select(sol_col, sol_label, sol_colour)

solution_labels <- label_lookup$sol_label

rownames(kappa_mat) <- solution_labels
colnames(kappa_mat) <- solution_labels

# plot

p_corr <- ggcorrplot(
  kappa_mat,
  outline.color = "grey50",
  lab = TRUE,
  lab_size = 4
) +
  scale_fill_gradient2(
    low = "#3B6FB6",
    mid = "white",
    high = "#B22222",
    limits = c(-1, 1),
    name = "Cohen's\nKappa"
  ) +
  scale_y_discrete(limits = rev) +
  # coord_fixed() +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      face = "bold"
    ),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = ""#"Spatial agreement between highlighted solutions"
  )


####### Bar Plot #####
area_df <- s_sf %>%
  st_drop_geometry() %>%
  dplyr::select(all_of(agg_cols)) %>%
  mutate(pu_area = as.numeric(st_area(s_sf)))  # area per PU

# Convert to long format 
area_long <- area_df %>%
  pivot_longer(cols = all_of(agg_cols),
               names_to = "solution",
               values_to = "use") %>%
  mutate(use = as.character(use))


# Summarise area share
area_summary <- area_long %>%
  group_by(solution, use) %>%
  summarise(
    total_area = sum(pu_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(solution) %>%
  mutate(
    share = total_area / sum(total_area)
  ) %>%
  ungroup()

# Clean solution names + extract ID
area_summary <- area_summary %>%
  mutate(
    solution_clean = str_remove(solution, "_agg"),
    solution_id = as.integer(str_extract(solution_clean, "\\d+"))
  )

area_summary <- area_summary %>%
  left_join(obj_df %>% select(solution_id, restore_obj),
            by = "solution_id")

# Order factor by increasing restoration objective 
area_summary <- area_summary %>%
  mutate(
    solution_clean = factor(
      solution_clean,
      levels = area_summary %>%
        distinct(solution_clean, restore_obj) %>%
        arrange(restore_obj) %>%
        pull(solution_clean)
    )
  )

# plot
highlight_df <- area_summary %>%
  distinct(solution_clean, solution_id) %>%
  left_join(
    sel_df %>%
      select(solution_id, sol_colour),
    by = "solution_id"
  ) %>%
  filter(!is.na(sol_colour)) %>%
  mutate(
    xmin = as.numeric(solution_clean) - 0.5,
    xmax = as.numeric(solution_clean) + 0.5,
    ymin = 0,
    ymax = 1
  )

p_area <- ggplot(area_summary,
                 aes(x = solution_clean,
                     y = share,
                     fill = use)) +
  geom_col() +
  fill_scale +
  geom_rect(
    data = highlight_df,
    aes(xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax),
    inherit.aes = FALSE,
    fill = NA,
    colour = highlight_df$sol_colour,
    linewidth = 1.2
  ) +
  theme_bw() +
  labs(
    x = "Scenarios (ordered by restoration objective)",
    y = "Relative share of total area",
    fill = "Use type"#,
    # title = "Allocation of land uses across scenarios"
  ) +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 0)),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


### Combine all plots

top_row <- pareto_plot   /
  ((maps[[2]] + theme(legend.position="none") +
      ggtitle("Production-focused")) |
     (maps[[1]]+ #guides(fill=guide_legend(ncol=2)) +
        ggtitle("Equal weighting")) |
     (maps[[3]] + theme(legend.position="none") + ggtitle("Restoration-focused"))
  ) +
  coord_fixed()

bottom_row <-
  ( (p_area + theme(legend.position = "none") +
       scale_y_continuous(
         expand = c(0, 0),
         limits = c(0, 1)
       )) |
      p_corr) +
  plot_layout(widths = c(3,1))

figure2 <- top_row / bottom_row + plot_annotation(tag_levels = 'A')

ggsave("figures/figure2_sn.png",
       figure2, width = 11, height = 10, dpi = 300)
