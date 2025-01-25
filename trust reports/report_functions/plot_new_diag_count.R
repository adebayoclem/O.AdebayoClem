library(ggplot2)

plot_new_diag_count <- function(data, title, x_label, y_label) {
  # Define color palette for the types of tests
  unique_tests <- unique(data$Test_Type)
  num_colors <- length(unique_tests)
  palette_colors <- blue_palette[1:num_colors]
  
  # Check if the data is sufficient for plotting
  if (nrow(data) == 0 || !all(c("Category", "n", "Test_Type", "Demographic") %in% names(data))) {
    message("No data available for plotting or missing necessary columns. Check data preparation and filters.")
    return(NULL)  # Exit the function if no data available or columns are missing
  }
  
  # Create the plot
  plot <- ggplot(data, aes(x = Category, y = n, fill = Test_Type)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_manual(values = palette_colors) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      strip.text.x = element_blank(),
      strip.text.y = element_blank(),
      strip.background = element_blank(),
      panel.grid.major.x = element_line(size = 0.5, color = "grey"),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(1, "lines")
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    facet_grid(Demographic ~ Test_Type, scales = "free_y", space = "free_y")
  
  print(plot)
}