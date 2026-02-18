library(tidyverse)
library(patchwork)
library(sf)

source("")

# LOAD DATA
s_formatted_prop <- readRDS("data/outputs/ms3_test_sn_formatted_prop.rds")
s_sf <- s_formatted_prop$s_sf

#PLOT
plot_data <- s_sf %>%
  pivot_longer(
    cols = -c(id, geometry),
    names_to = c("solution", "use"),
    names_pattern = "solution_(\\d+)_(.*)",
    values_to = "prop"
  ) %>%
  mutate(solution_id = as.integer(solution)) %>%
  left_join(sel_df, by = "solution_id")

panel_labels <- plot_data %>%
  distinct(use, sol_label) %>%
  arrange(use, sol_label) %>%
  mutate(panel = letters[1:n()])

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

ggsave("figures/supp_figure2.png",
       supp_figure2,
       width = 12, height = 13, dpi = 300
)
