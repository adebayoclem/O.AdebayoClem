plot_attendees_demographics <- function(data, bbv_type, custom_title = NULL) {
  # Pivot the data longer
  data_long <- data %>%
    pivot_longer(cols = -Total_Attendees, names_to = "Demographic", values_to = "Category") %>%
    filter(!is.na(Category)) %>%
    filter(!(Category %in% c("Unknown Gender", "Unknown Ethnicity", "Other", "Unknown IMD", "Unknown Age Group")))
  
  # Check if data is empty after filters
  if (nrow(data_long) == 0) {
    message("No data available for plotting after filters. Returning NULL.")
    return(NULL)  # Exit the function if no data available for plotting
  }
  
  # Determine the plot title
  title <- if(is.null(custom_title)) {
    paste("All", bbv_type, "Attendees by Demographics with overall distribution")
  } else {
    custom_title
  }
  
  # Define the custom ggplot theme inside the function
  custom_ggplot_theme <- function() {
    theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_blank(),  # Remove facet headers
        strip.background = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5, color = "lightgrey"),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      )
  }
  
  # Plot the data
  ggplot(data_long, aes(x = Category, y = Total_Attendees, fill = Demographic)) +
    geom_bar(stat = "identity", fill = "#00A5DF", show.legend = FALSE) +
    coord_flip() +
    custom_ggplot_theme() +
    labs(
      title = title,
      x = "Demographic Category",
      y = "Total Attendees"
    ) +
    facet_grid(Demographic ~ ., scales = "free_y", space = "free") +
    scale_y_continuous(labels = scales::label_number())
}