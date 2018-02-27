#' Helper function for adding an image
#'
#' Add an image inside the body of the
#' email with this helper function.
#' @param file a path to an image file.
#' @return a character object with an HTML
#' fragment that can be placed inside the
#' message body wherever the image should
#' appear.
#' @examples
#' # Create an HTML fragment that
#' # contains an image
#' img_file_path <-
#'   system.file(
#'     "img",
#'     "test_image.png",
#'     package = "blastula")
#'
#' img_file_html <-
#'   add_image(
#'     file = img_file_path)
#'
#' # Include the image in the email
#' # message body by simply referencing
#' # the `img_file_html` object
#' email <-
#'   compose_email(
#'     body = "
#'     Hello!
#'
#'     Take a look at this image:
#'
#'     {img_file_html}
#'
#'     Useful, right?
#'     ")
#' @importFrom glue glue
#' @importFrom knitr image_uri
#' @export
add_image <- function(file) {

  uri <- knitr::image_uri(f = file)

  glue::glue("<img src=\"{uri}\" width=\"520\"/>\n")
}
