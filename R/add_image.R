#' Create an HTML fragment for an embedded image
#'
#' Add a local image inside the body of the email with this helper function.
#'
#' @param file A path to an image file.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. `NULL` default produces blank (`""`) alt text.
#'
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the image should appear.
#'
#' @examples
#' # Create an HTML fragment that
#' # contains an image
#' img_file_path <-
#'   system.file(
#'     "example_files",
#'     "test_image.png",
#'     package = "blastula"
#'   )
#'
#' img_file_html <-
#'   add_image(file = img_file_path)
#'
#' # Include the image in the email
#' # message body by simply referencing
#' # the `img_file_html` object
#' email <-
#'   compose_email(
#'     body = md(
#'       c(
#' "Hello,
#'
#' Here is an image:\n",
#' img_file_html
#'       )
#'     )
#'   )
#'
#' if (interactive()) email
#'
#' @export
add_image <- function(file, alt = NULL) {

  # Construct a CID based on the filename
  # with a random string prepended to it
  cid <-
    paste0(
      sample(letters, 12) %>% paste(collapse = ""),
      "__",
      basename(file)
    )

  # Create the image URI
  uri <- get_image_uri(file = file)

  # Determine alt text
  alt_text <-
    if (is.null(alt)) {
      ""
    } else {
      alt
    }

  # Generate the Base64-encoded image and place it
  # within <img> tags
  paste0(
    "<img cid=\"", cid,
    "\" src=\"", uri,
    "\" width=\"520\" alt=\"", alt_text %>% htmltools::htmlEscape(attribute = TRUE), "\"/>\n"
  )
}

get_image_uri <- function(file) {

  image_raw <-
    readBin(
      con = file,
      what = "raw",
      n = file.info(file)$size
    )

  paste0(
    "data:", mime::guess_type(file = file), ";base64,",
    base64enc::base64encode(image_raw, 0)
  )
}
