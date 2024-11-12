#' Apply Zero Margins to ggplot Object
#'
#' This function takes a ggplot object and applies a theme that sets the plot margins to zero, removing any spacing around the plot.
#'
#' @param ggobj A ggplot object to which the zero-margin theme should be applied.
#'
#' @return A ggplot object with the updated theme applied.
#' @export
#'
#' @examples
#' p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
#' zero_margins(p)

zero_margins <- function(ggobj) {
  # Check if the input is a ggplot object
  if (!ggplot2::is.ggplot(ggobj))  {
    cli::cli_abort("The provided object is not a ggplot object. Please provide a valid ggplot object.")
  }
  
  # If it is a ggplot, apply the custom theme
  ggobj + ggplot2::theme(
    plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
  )
}
