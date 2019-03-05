#' Create an HTML fragment for an embedded image
#'
#' Add a local image inside the body of the email with this helper function.
#' @param file A path to an image file.
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the image should appear.
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
#'     Here is an image:
#'
#'     {img_file_html}
#'     ")
#' @importFrom glue glue
#' @export
add_image <- function(file) {

  # Construct a CID based on the filename
  # with a random string prepended to it
  cid <-
    paste0(
      sample(letters, 12) %>% paste(collapse = ""), "__",
      basename(file))

  # Create the image URI
  uri <- get_image_uri(file = file)

  # Generate the Base64-encoded image and place it
  # within <img> tags
  glue::glue("<img cid=\"{cid}\" src=\"{uri}\" width=\"520\"/>\n")
}
