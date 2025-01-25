
demographics_plot <- function(data, demographic_var, plot_title) {
  # Filter out specific categories based on demographic variable
  if (demographic_var == "Gender") {
    data <- data %>% filter(Gender != "Unknown Gender")
  } else if (demographic_var == "IMD") {
    data <- data %>% filter(IMD != "Unknown IMD")
  } else if (demographic_var == "ethnic_group") {
    data <- data %>% filter(!ethnic_group %in% c("Unknown Ethnicity", "Other"))
  }
  if (nrow(data) == 0) return(data.frame())
  # Create the plot
  plot <- ggplot(data, aes(x = !!sym(demographic_var), y = Total_Attendees, fill = !!sym(demographic_var))) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Flip the axes
    theme_minimal() +
    labs(title = plot_title,
         x = demographic_var,
         y = "Total Attendees")
  
  return(plot)
}