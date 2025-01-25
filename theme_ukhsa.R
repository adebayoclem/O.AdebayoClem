#' Theme UKHSA
#'
#' UKHSA theme for use with ggplot 2
#'
#' @param theme string, theme of chart, current options are "ukhsa" or "fingertips", default "ukhsa"
#' @param base_size base font size
#' @param base_family base font family, default ""
#' @param base_line_size base size for line elements, default base_size/22
#' @param base_rect_size base size for rect elements, base_size/22
#' @import ggplot2
#' @return A ggplot theme
#' @export
#'
#' @examples
#' library(ggplot2)
#' set.seed(12234)
#' df <- data.frame(area = paste("Area", 1:10),
#'                  val = runif(10),
#'                  significance = sample(c("Better", "Same", "Worse", "Not compared"),
#'                                        10, replace = TRUE))
#'
#' ggplot(df, aes(x = area, y = val, fill = significance)) +
#'   geom_col() +
#'   labs(title="Area vs Value")+
#'   theme_ukhsa(theme="ukhsa") +
#'   scale_fill_manual(values=ukhsa_colours(theme="ukhsa2"), name = "Significance colour")
theme_ukhsa <- function (theme = "ukhsa",
                         base_size = 22,
                         base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  #Create a list of the formatting options for the available themes
  ukhsa_key <- list(
    ukhsa = list(
      colour_title = "#007C91",
      colour_strip = "#00AB8E",
      colour_strip_background = "white",
      base_colour = "black",
      line_colour = "#666666",
      axis_line_colour = "black"
    ),
    fingertips = list(
      colour_title = "black",
      colour_strip = "white",
      colour_strip_background = "#02AE94",
      base_colour = "#11175E",
      line_colour = "#666666",
      axis_line_colour = "#666666"
    )
  )

  #Return error message if unknown theme is requested, otherwise modify "theme_grey" according to the selected theme
  if (!(theme %in% names(ukhsa_key))) {
    stop("name not in available pre-loaded themes")
  }
  half_line <- base_size / 2
  ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      line = element_line(
        colour = ukhsa_key[[theme]]$line_colour,
        size = 0.5,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "black",
        size = 0.5,
        linetype = 1
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        colour = ukhsa_key[[theme]]$base_colour,
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      axis.line = element_line(colour = ukhsa_key[[theme]]$axis_line_colour),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(margin = margin(t = 0.8 * half_line /
                                                   2),
                                 vjust = 0.5),
      axis.text.y = element_text(margin = margin(r = 0.8 * half_line /
                                                   2),
                                 hjust = 1),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.ticks = element_line(colour = ukhsa_key[[theme]]$axis_line_colour),
      axis.title.x = element_text(margin = margin(
        t = 0.8 * half_line,
        b = 0.8 * half_line /
          2
      ),
      face = "bold"),
      axis.title.y = element_text(
        angle = 90,
        margin = margin(r = 0.8 * half_line,
                        l = 0.8 * half_line /
                          2),
      face = "bold"),
      legend.background = element_blank(),
      legend.margin = margin(),
      legend.key = element_blank(),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size),
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0,
      face = "bold"),
      legend.title.align = NULL,
      legend.position = "right",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL,
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_line(),
      panel.grid.major.y = element_line(colour = "#ECECDE"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_rect(
        fill = ukhsa_key[[theme]]$colour_strip_background,
        colour = NA
      ),
      strip.text = element_text(
        colour = ukhsa_key[[theme]]$colour_strip,
        size = rel(1),
        face = "bold"
      ),
      strip.text.x = element_text(
        margin = margin(t = half_line,
                        b = half_line),
        hjust = 0.1
      ),
      strip.text.y = element_text(
        angle = -90,
        margin = margin(l = half_line,
                        r = half_line)
      ),
      strip.switch.pad.grid = unit(0.1, "cm"),
      strip.switch.pad.wrap = unit(0.1, "cm"),
      plot.background = element_blank(),
      plot.title = element_text(
        size = rel(1),
        margin = margin(b = half_line * 1.2),
        hjust = 0,
        colour = ukhsa_key[[theme]]$colour_title,
        lineheight = .8,
        face = "bold"
      ),
      plot.subtitle = element_text(size = rel(1),
                                   hjust = 0),
      plot.caption = element_text(hjust = 1),
      plot.margin = margin(half_line, half_line, half_line, half_line),
      complete = TRUE
    )
}
