#' Deploy a local image to Imgur and create an image tag
#'
#' Getting images into email message bodies (and expecting them to appear for
#' the recipient) can be a harrowing experience. External images (i.e.,
#' available at public URLs) work exceedingly well and most email clients will
#' faithfully display these images. With the `imgur_image()` function, we can
#' take a local image file or a `ggplot2` plot object and send it to the Imgur
#' service, and finally receive an image (`<img>`) tag that can be directly
#' inserted into an email message using `compose_email()`.
#'
#' To take advantage of this, we need to first have an account with Imgur and
#' then obtain a `Client-ID` key for the Imgur API. This can be easily done by
#' going to `https://api.imgur.com/oauth2/addclient` and registering an
#' application. Be sure to select the OAuth 2 authorization type without a
#' callback URL.
#'
#' @param image The path to the local image we would like to deploy to Imgur and
#'   for which we'd like an image tag.
#' @param client_id The Imgur Client ID value.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. `NULL` default produces blank (`""`) alt text.
#'
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the image should appear.
#'
#' @export
add_imgur_image <- function(image,
                            client_id = NULL,
                            alt = NULL) {

  # nocov start

  # Using this function requires the `xml2` package
  if (!requireNamespace("xml2", quietly = TRUE)) {

    stop("The `xml2` package is required for using the ",
         "`add_imgur_image()` function",
         call. = FALSE)
  }

  if (inherits(image, "ggplot")) {

    # If the `ggplot2` package is available, then
    # use the `ggplot2::ggsave()` function
    if (requireNamespace("ggplot2", quietly = TRUE)) {

      # Generate a unique filename for the temporary,
      # on-disk image
      file_name <-
        paste0(
          paste(
            sample(c(LETTERS, 0:9), 4),
            collapse = ""
          ),
          "_", Sys.time() %>% as.integer(),
          "_", "ggplot.png"
        )

      # Create SVG from ggplot
      ggplot2::ggsave(
        filename = file_name,
        plot = image,
        device = "png",
        dpi = 300,
        width = 5,
        height = 5
      )

      # Because the file is saved on disk,
      # We replace `image` with the path
      # (since `imgur_upload` only works with
      # file paths)
      image <- file_name

      # Upon exiting the function (either
      # through success or error, we remove
      # the on-disk image file)
      on.exit(file.remove(file_name), add = TRUE)
    }
  }

  # Upload the image to Imgur
  response_list <- imgur_upload(image, client_id)

  # Determine alt text
  alt_text <-
    if (is.null(alt)) {
      ""
    } else {
      alt
    }

  paste0(
    "<a href=\"#\"><img src=\"",
    response_list$link %>% htmltools::htmlEscape(attribute = TRUE),
    "\" alt=\"",
    alt_text %>% htmltools::htmlEscape(attribute = TRUE), "\"",
    "style=\"max-width: 512px; width: 100% !important; display: block; padding: 0;",
    "border: 0 !important;\" border=\"0\"></a>"
  )

  # nocov end
}
