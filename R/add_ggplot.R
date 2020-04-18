#' Create an HTML fragment for an embedded ggplot image
#'
#' Add an ggplot plot inside the body of the email with this helper function.
#'
#' @param plot_object The `ggplot2` plot object.
#' @param height The height of the output plot in inches.
#' @param width The width of the output plot in inches.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. Defaults to the `ggplot2` plot object's title, if exists.
#'   Override by passing a custom character string or `""` for no text.
#'
#' @examples
#' library(ggplot2)
#'
#' # Create a ggplot plot
#' plot <-
#'   ggplot(
#'     data = mtcars,
#'     aes(x = disp, y = hp,
#'         color = wt, size = mpg)) +
#'   geom_point()
#'
#' # Create an HTML fragment that
#' # contains an the ggplot as an
#' # embedded plot
#' plot_html <-
#'   add_ggplot(plot_object = plot)
#'
#' # Include the plot in the email
#' # message body by simply referencing
#' # the `plot_html` object
#' email <-
#'   compose_email(
#'     body = md(
#'       c(
#' "Hello!
#'
#' Here is a plot that will change
#' the way you look at cars forever.\n",
#' plot_html,
#' "Let me know what you think
#' about it!"
#'       )
#'     )
#'   )
#'
#' if (interactive()) email
#'
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the plot image should appear.
#' @export
add_ggplot <- function(plot_object,
                       width = 5,
                       height = 5,
                       alt = NULL) {

  # nocov start

  tmpfile <- tempfile("ggplot", fileext = ".png")

  # If the `ggplot2` package is available, then
  # use the `ggplot2::ggsave()` function
  if (requireNamespace("ggplot2", quietly = TRUE)) {

    ggplot2::ggsave(
      device = "png",
      plot = plot_object,
      filename = tmpfile,
      dpi = 200,
      width = width,
      height = height)

  } else {
    stop("Please ensure that the `ggplot2` package is installed before using `add_ggplot()`.",
         call. = FALSE)
  }

  on.exit(file.remove(tmpfile), add = TRUE)

  # Determine alt text
  alt_text <-
    if (is.null(alt)) {
      plot_object$labels$title
    } else {
      alt
    }

  image_html <-
    add_image(file = tmpfile, alt = alt_text, width = width * 100)

  image_html

  # nocov end
}
