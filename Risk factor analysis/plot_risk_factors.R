
plot_risk_factors <- function(data, risk_factor_col, plot_title) {
  ggplot(data, aes(x = var, y = !!sym(risk_factor_col))) +  # Use sym() to convert string to symbol
    geom_bar(stat = "identity", fill = "#00A5DF", show.legend = FALSE) +
    coord_flip() +
    custom_ggplot_theme() +
    labs(
      title = plot_title,
      x = "Demographic Category",
      y = "Total Patients"
    ) +
    facet_grid(breakdown ~ ., scales = "free_y", space = "free") +
    scale_y_continuous(labels = label_number(), breaks = pretty_breaks())
}