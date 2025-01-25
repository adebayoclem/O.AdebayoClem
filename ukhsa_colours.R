#' UKHSA colours
#'
#' Returns a character vector containing hex colours based on the colour palette
#' requested
#'
#' @param theme string, defaults to "ukhsa1"; either "ukhsa1" (6 colours suitable
#' for text), "ukhsa2" (7 colours suitable for non-text uses), "high_contrast" (12
#' high contrast colours), "ukhsa1_expanded" (n colours extrapolated from ukhsa1),
#' "ukhsa2_expanded" (n colours extrapolated from ukhsa2),"ukhsa_teals" (n shades
#' of teal), "ukhsa_greens" (n shades of green), "ukhsa_blues" (n shades of blue)
#' or "ukhsa_greys" (n shades of grey).
#' @param n number, defaults to 2; number of colours required if theme is  uhsa1_expanded,
#' ukhsa2_expanded, ukhsa_teals, ukhsa_greens, ukhsa_blues or ukhsa_greys; has no
#' effect if theme is ukhsa1, ukhsa2 or high_contrast.
#' @param names logical, defaults to FALSE; TRUE will return a named character vector
#' if the theme is "ukhsa1" or "ukhsa2"; names are not available for the other palettes.
#' @importFrom grDevices colorRampPalette
#'
#' @return A character vector
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(ukhsacharts)
#'
#' categories <- c("Car", "Bike", "Horse", "Running")
#' df <- data.frame(category = categories,
#'                  value = runif(4))
#'
#' pal <- ukhsa_colours()
#' p <- ggplot(df, aes(x = category, y = value)) +
#'         geom_col(aes(fill = category)) +
#'         labs(title = "Methods of transport") +
#'         scale_fill_manual(values = pal)
#' p
#'
#' ## OR
#'
#' pal <- ukhsa_colours(theme="ukhsa2", names = TRUE)
#' pal <- pal[c("UKHSA teal","ocean","tangerine","sand")]
#' names(pal) <- categories
#' p <- ggplot(df, aes(x = category, y = value)) +
#'         geom_col(aes(fill = category)) +
#'         labs(title = "Methods of transport") +
#'         scale_fill_manual(values = pal)
#' p
ukhsa_colours <- function(theme = "ukhsa1",
                          n = 2,
                          names = FALSE)
{
  # Create a list of the different colour palettes
  colour_key  <- list(
    ukhsa1 = c(
      "#007C91",
      "#003B5C",
      "#582C83",
      "#1D57A5",
      "#8A1B61",
      "#E40046"
    ),
    ukhsa2 = c(
      "#007C91",
      "#00AB8E",
      "#00A5DF",
      "#84BD00",
      "#FF7F32",
      "#FFB81C",
      "#D5CB9F"
    ),
    ukhsa1_expanded = colorRampPalette(
      c(
        "#007C91",
        "#003B5C",
        "#582C83",
        "#1D57A5",
        "#8A1B61",
        "#E40046"
      ))(n),
    ukhsa2_expanded = colorRampPalette(
      c(
        "#007C91",
        "#00AB8E",
        "#00A5DF",
        "#84BD00",
        "#FF7F32",
        "#FFB81C",
        "#D5CB9F"
      ))(n),
    ukhsa_teals = colorRampPalette(c("#b3f4ff", "#00414d"))(n),
    ukhsa_greens = colorRampPalette(c("#b3fff2", "#004d40"))(n),
    ukhsa_blues = colorRampPalette(c("#b3e4ff", "#00314d"))(n),
    ukhsa_greys = colorRampPalette(c("#D3D3D3", "#000000"))(n),
    high_contrast = c(
        "#21569E",
        "#AAD874",
        "#000000",
        "#B96A3A",
        "#C54879",
        "#999999",
        "#6FB7E5",
        "#859C61",
        "#DCA138",
        "#632966",
        "#D3B2E0",
        "#EDDF5F"
    ))

  # Create a list of names of colours for ukhsa1 and ukhsa2
  ukhsa_colours_names <- list(
    ukhsa1 = c(
      "UKHSA teal",
      "midnight",
      "plum",
      "moonlight",
      "wine",
      "cherry"
    ),
    ukhsa2 = c(
      "UKHSA teal",
      "DHSC green",
      "ocean",
      "grass",
      "tangerine",
      "sunny",
      "sand"
    )
  )

  # Return error messages if a non-existent theme is requested or names is set to
  # TRUE and an expanded palette is requested
  if (!(theme %in% names(colour_key))) {
    stop("name not in available pre-loaded palettes")
  }
  if (names &
      (theme == "ukhsa1_expanded"|
       theme == "ukhsa2_expanded"|
       theme == "ukhsa_teals" |
       theme == "ukhsa_greens"|
       theme == "ukhsa_blues"|
       theme == "ukhsa_greys"|
       theme == "high_contrast")) {
    stop("names not available for the selected theme; names are only available for
         ukhsa1 and ukhsa2")
  }

  # Assign the requested list of colours to a new variable, "ukhsa_colours"
  ukhsa_colours <- colour_key[[theme]]

  #If names are requested, make the requested list of names the names of the colours
  # in "ukhsa_colours"
  if (names & (theme == "ukhsa1" | theme == "ukhsa2")) {
    ukhsa_colours_names <- ukhsa_colours_names[[theme]]
    names(ukhsa_colours) <- ukhsa_colours_names
  }

  #Return error messages if a nonsensical value of n is requested
  if (length(n) > 1) stop("n must have length of 1")
  if (n < 1) stop("n must be positive")
  if (n > length(ukhsa_colours)) {
    warning(
      paste(
        "warning,",
        n,
        "colours requested but only",
        length(ukhsa_colours),
        "available in the selected palette.",
        length(ukhsa_colours),
        "colours returned."
      )
    )

  }

  return(ukhsa_colours)
}
#####################################################################################
