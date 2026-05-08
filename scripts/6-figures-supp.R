#source("scripts/0-preliminaries.R") # for package installation

library(tidyverse)
library(patchwork)
library(sf)

# LOAD DATA
scenario_name <- "multi_ws_rest_0.3"
s_formatted_prop <- readRDS(paste0("data/outputs/formatted/", scenario_name, "_formatted_prop.rds"))
s_sf <- s_formatted_prop$s_sf
sel_df <- s_formatted_prop$sel_df

# PREP DATA
solution_pattern <- paste0("^solution_(", paste(sel_df$solution_id, collapse = "|"), ")_")

s_sf <- s_sf %>%
  select(
    id,
    geometry,
    matches(solution_pattern)
  )

plot_data <- s_sf %>%
  pivot_longer(
    cols = -c(id, geometry),
    names_to = c("solution", "use"),
    names_pattern = "solution_(\\d+)_(.*)",
    values_to = "prop"
  ) %>%
  mutate(
    solution_id = as.integer(solution),
    use = case_when(
      use == "restoration"  ~ "Restoration",
      use == "production"   ~ "Production",
      use == "conservation" ~ "Conservation",
      use == "urban_area"   ~ "Urban area",
      TRUE ~ use
    )
  ) %>%
  left_join(sel_df, by = "solution_id")

plot_data$use <- factor(
  plot_data$use,
  levels = c("Restoration", "Production", "Conservation", "Urban area")
)

plot_data$sol_label <- factor(
  plot_data$sol_label,
  levels = c("Restoration-focused", "Equal trade-offs", "Production-focused")
)

panel_labels <- plot_data %>%
  distinct(use, sol_label) %>%
  arrange(use, sol_label) %>%
  mutate(panel = letters[1:n()])

# PLOT
supp_figure2 <- ggplot(plot_data) +
  geom_sf(aes(fill = prop), color = NA) +
  scale_fill_viridis_c(option = "plasma",
                       guide = guide_colourbar(
                         title.position = "top",
                         title.hjust = 0.5,
                         barwidth = unit(12, "cm"),
                         barheight = unit(0.6, "cm"),
                         frame.colour = "black",
                         ticks.colour = "black"
                       )) +
  facet_grid(use ~ sol_label) +
  geom_text(
    data = panel_labels,
    aes(label = panel),
    x = -Inf, y = Inf,
    hjust = -0.3, vjust = 1.3,
    inherit.aes = FALSE,
    fontface = "bold",
    size = 5.5
  ) +
  labs(fill = "Proportion") +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    # legend.box.background = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = ggplot2::element_text(size = 20, colour = "black"),
    axis.text = ggplot2::element_text(size = 15, colour = "black"),
    plot.title = ggplot2::element_text(size = 20, colour = "black")
  )

ggsave(paste0("figures/supp_fig2_", scenario_name, ".png"),
       supp_figure2,
       width = 12, height = 13, dpi = 300
)
