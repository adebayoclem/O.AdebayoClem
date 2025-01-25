
bbv_prop_tested_plot <- function(data, title, x_label, y_label, blue_palette) {
  
  if (nrow(data) == 0) return(data.frame())
  # Filter the data
  filtered_data <- data %>%
    filter(!(Demographic == "IMD" & Category == "Unknown IMD"),
           !(Demographic == "Gender" & Category %in% c("Unknown Gender")),
           !(Demographic == "Ethnic Group" & Category %in% c("Unknown Ethnicity", "Other")))
  
  
  # Define the custom ggplot theme inside the function
  custom_ggplot_theme <- function() {
    theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5, color = "lightgrey"),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      )
  }
  
  # Plot the filtered data
  ggplot(filtered_data, aes(x = Category, y = Proportion_Tested, fill = Test_Type)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    coord_flip() +
    scale_fill_manual(values = blue_palette) +
    custom_ggplot_theme() +
    labs(title = title, x = x_label, y = y_label) +
    facet_grid(Demographic ~ Test_Type, scales = "free_y", space = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) # Use percent format for y-axis
}