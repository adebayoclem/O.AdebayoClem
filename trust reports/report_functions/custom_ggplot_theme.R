custom_ggplot_theme <- function() {
  theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      strip.text.x = element_blank(),  # Hide facet labels (for the x-axis)
      strip.text.y = element_blank(),  # Hide facet labels (for the y-axis after coord_flip)
      strip.background = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "lightgrey"),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(1, "lines"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12)
    )
}