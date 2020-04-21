#' Create an HTML fragment for an embedded image
#'
#' Add a local image inside the body of the email with this helper function.
#'
#' @param file A path to an image file.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. `NULL` default produces blank (`""`) alt text.
#' @param width The width to be used for the image, in pixels.
#' @param align The alignment to be used for the image. If not `"inline"` (the
#'   default), the image will appear in its own block, i.e. there will not be
#'   text to the left or right of it.
#' @param float The float value to be used for the image. If not `"none"`, text
#'   will flow around the image. You cannot specify non-default values for
#'   `align` and `float` in the same function call.
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
add_image <- function(file, alt = "", width = 520,
  align = c("inline", "left", "center", "right"),
  float = c("none", "left", "right")) {

  align <- match.arg(align)
  float <- match.arg(float)

  if (align != "inline" && float != "none") {
    stop("add_image() doesn't support align and float at the same time")
  }

  if (float == "none") {
    float <- NULL
  }

  # Create the image URI
  uri <- get_image_uri(file = file)

  img <- tags$img(src = uri, alt = alt, width = width, align = float,
    style = css(float = float))

  if (isTRUE(align %in% c("left", "center", "right"))) {
    img <- panel(outer_align = align, inner_align = align,
      img
    )
  }
  # Must return as HTML string since the result may be inlined into md()
  HTML(as.character(img))
}

get_image_uri <- function(file) {
  if (grepl("^(https?:)?//", file, ignore.case = TRUE)) {
    return(file)
  }

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
