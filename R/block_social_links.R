#' Specify the components of a social link
#'
#' The `social_link()` function is used exclusively within
#' `block_social_links()` with as many calls as the number of social media links
#' required.
#'
#' @param service Either the name of a social media service or either of
#'   `website`, `email`, or `rss`.
#' @param link The relevant link to content on the `service`.
#' @param icon A link to an icon for the service. If `NULL`, then a link to a
#'   suitable image asset will be automatically created.
#' @param variant The variant of the icon used for the `service` if `icon` is
#'   set to `NULL`.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. `NULL` default produces blank (`""`) alt text.
#' @export
social_link <- function(service,
                        link,
                        icon = NULL,
                        variant = NULL,
                        alt = NULL) {

  # Lowercase the input given as `service`
  service <- tolower(service)

  # Generate a link to an image for the service
  if (is.null(icon)) {
    icon <- icon_for_social_service(service = service, variant = variant)
  }

  # Determine alt text
  alt_text <-
    if (is.null(alt)) {
      ""
    } else {
      alt
    }

  # Add the social link components to the
  # `social_link_item_list` object
  social_link_item_list <-
    list(
      service = service,
      link = link,
      icon = icon,
      variant = variant,
      alt = alt)

  # Apply the `social_link` class
  class(social_link_item_list) <- "social_link"

  social_link_item_list
}

#' A block of one, two, or three articles with a multicolumn layout
#'
#' With `block_social_links()`, we can create a block of social media links and
#' links to websites, email, or RSS feeds. The function can accept as many
#' `social_link()` calls as seen fit to email. Like all `block_*()` functions,
#' `block_social_links()` must be placed inside of `blocks()` and that object
#' can be provided to the `body` or `footer` argument of `compose_email()`.
#'
#' @param ... One or more calls to `social_link()`.
#' @export
block_social_links <- function(...) {

  x <- list(...)

  class(x) <- "block_social_links"

  x
}

#' @importFrom glue glue
#' @noRd
render_block_social_links <- function(x) {

  for (i in seq_along(x)) {

    link <- x[[i]]$link
    icon <- x[[i]]$icon
    alt <- x[[i]]$alt

    x[[i]] <-
      glue(
        "<a href=\"{link}\" style=\"text-decoration: underline; color: #999999; font-size: 12px; text-align: center;\"><img src=\"{icon}\" alt=\"{alt}\" width=\"44\" class=\"social-sharing-icon\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; height: 44px; margin: 0 2px;\"></a>&nbsp;"
      ) %>%
      as.character()
  }

  social_links <- x %>% unlist() %>% paste(collapse = "\n")

  glue::glue(social_link_block_template()) %>% as.character()
}

#' A template for a social link HTML fragment
#' @noRd
social_link_block_template <- function() {

  "              <tr>
                <td class=\"content-block\" style=\"font-family: Helvetica, sans-serif; vertical-align: top; padding-top: 0; padding-bottom: 24px; font-size: 12px; color: #999999; text-align: center;\" valign=\"top\" align=\"center\">
                  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"social-sharing\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: auto; margin: 0 auto; text-align: center;\" align=\"center\">
                    <tbody>
                      <tr>
                        <td style=\"font-family: Helvetica, sans-serif; vertical-align: top; font-size: 12px; color: #999999; text-align: center;\" valign=\"top\" align=\"center\">
                          {social_links}
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </td>
              </tr>"
}

#' @importFrom glue glue
#' @noRd
icon_for_social_service <- function(service,
                                    variant = NULL) {

  # If a hosted icon isn't availble for the
  # `service`, stop the function
  if (!(service %in% social_service_icons())) {
    stop("An icon for the given `service` is not available\n",
         " * look inside the article at `?social_link` to see which are available\n",
         call. = FALSE)
  }

  # Ensure that the icon variant is valid; if
  # nothing is provided for variant, default to `bw`
  if (!is.null(variant) && !(variant %in% social_service_icon_variants())) {
    stop("The given `variant` is not available for the social icons\n",
         " * look inside the article at `?social_link` to see which are available\n",
         call. = FALSE)
  } else if (is.null(variant)) {
    variant <- "bw"
  }

  social_icons_stub <-
    "raw.githubusercontent.com/rich-iannone/blastula/master/inst/social_icons"

  # Create the link to the hosted image asset
  glue::glue(
    "https://{social_icons_stub}/{service}-{variant}.png"
  ) %>%
    as.character()
}

social_service_icons <- function() {

  c(
    "website",
    "email",
    "rss",
    "twitter",
    "github",
    "facebook",
    "instagram",
    "linkedin",
    "youtube",
    "vimeo",
    "behance",
    "dribbble",
    "pinterest",
    "500px",
    "yelp",
    "tripadvisor",
    "wordpress",
    "blogger",
    "tumblr",
    "deezer",
    "soundcloud",
    "slideshare",
    "meetup",
    "etsy",
    "reddit"
  )
}
