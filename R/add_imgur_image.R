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
#' @param image The path to the local image we would like to deploy to Imgur and
#'   for which we'd like an image tag.
#' @param client_id The Imgur Client ID value.
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the image should appear.
#' @importFrom ggplot2 ggsave
#' @importFrom glue glue
#' @export
add_imgur_image <- function(image,
                            client_id = NULL) {

  if (inherits(image, "ggplot")) {

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
      height = 5)

    # Because the file is saved on disk,
    # We replace `image` with the path
    # (since `imgur_upload` only works with
    # file paths)
    image <- file_name

    # Upon exiting the function (either
    # through success or error, we remove
    # the on-disk image file)
    on.exit(file.remove(file_name))
  }

  # Upload the image to Imgur
  response_list <-
    imgur_upload(image, client_id)

  glue::glue(
    "<img src=\"{response_list$link}\" width=\"100%\">"
  ) %>%
    as.character()
}

