library(ggplot2)
library(scales)

proportion_diagnosed_plot <- function(data, title, x_label, y_label) {
  
  # Ensure there are non-NA, non-zero values in the data
  valid_data <- data %>%
    filter(!is.na(Proportion) & Proportion > 0)
  
  # Check if there is any valid data left
  if (nrow(valid_data) == 0) {
    message()
    return(NULL)  # Exit the function if there's no data
  }  

  # Generate a color palette based on the number of unique Test_Type values
  unique_tests <- unique(data$Test_Type)
  num_colors <- length(unique_tests)
  palette_colors <- blue_palette[1:num_colors]
  
  # Create the plot
  plot <- ggplot(valid_data, aes(x = Category_Value, y = Proportion, fill = Test_Type)) +
    geom_bar(stat = "identity") +
    coord_flip() +
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
    facet_grid(Demographic ~ Test_Type, scales = "free_y", space = "free_y", drop = TRUE) +  # Avoid empty facets
    scale_fill_manual(values = palette_colors) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01))
  
  print(plot)
}