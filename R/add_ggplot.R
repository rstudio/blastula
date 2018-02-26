#' Helper function for adding an ggplot
#'
#' Add an ggplot plot inside the body of the
#' email with this helper function.
#' @param plot_object the ggplot plot object.
#' @param height the height of the output
#' plot in inches.
#' @param width the width of the output plot
#' in inches.
#' @return a character object with an HTML
#' fragment that can be placed inside the
#' message body wherever the plot image
#' should appear.
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
#'   add_ggplot(
#'     plot_object = plot,
#'     height = 5,
#'     width = 7)
#'
#' # Include the plot in the email
#' # message body by simply referencing
#' # the `plot_html` object
#' email <-
#'   compose_email(
#'   body = "
#'   Hello!
#'
#'   Here is a very important plot \\
#'   that will change the way you \\
#'   look at cars forever.
#'
#'   {plot_html}
#'
#'   So useful, right?
#'   ") %>% preview_email()
#' @importFrom ggplot2 ggsave
#' @export
add_ggplot <- function(plot_object,
                       width,
                       height) {

  ggplot2::ggsave(
    device = "png",
    plot = plot_object,
    filename = "temp_ggplot.png",
    width = width,
    height = height)

  Sys.sleep(2)

  image_html <-
    add_image(file = "temp_ggplot.png")

  file.remove("temp_ggplot.png")

  image_html
}
