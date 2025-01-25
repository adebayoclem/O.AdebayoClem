library(ggplot2)
library(dplyr)
library(scales) # Ensure scales is loaded for percent_format()

plot_proportions_tested <- function(data, bbv_type, custom_title = NULL) {
  # Filter the data
  filtered_data <- data %>%
    filter(!(Demographic == "IMD" & Category == "Unknown IMD"),
           !(Demographic == "Gender" & Category %in% c("Unknown Gender")),
           !(Demographic == "Ethnic Group" & Category %in% c("Unknown Ethnicity", "Other")),
           !(Demographic == "Age Group" & (is.na(Category) | Category == "NA" | Category == "Unknown Age Group")), # Added 'Unknown Age Group'
           Proportion_Tested >= 0)  # Ensure no negative proportions
  
  # Check if data is sufficient for plotting
  if (nrow(filtered_data) == 0) {
    message("No data available for plotting after filtering. Check data preparation and filters.")
    return(NULL)  # Exit the function if no data available for plotting
  }
  
  # Determine the plot title
  title <- if(is.null(custom_title)) {
    paste("Proportion of Attendees with a", bbv_type, "Blood Test by Demographics")
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
        strip.text.x = element_blank(),  # Hide facet labels
        strip.text.y = element_blank(),  # Hide facet labels (for y-axis after coord_flip)
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
  ggplot(filtered_data, aes(x = Category, y = Proportion_Tested)) +
    geom_bar(stat = "identity", fill = "#00A5DF", show.legend = FALSE) +
    coord_flip() +
    custom_ggplot_theme() +
    labs(
      title = title,
      x = "Demographic Category",
      y = "Percentage Tested (%)"
    ) +
    facet_grid(Demographic ~ ., scales = "free_y", space = "free") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) # Ensure scale starts at 0
}
