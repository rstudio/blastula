#' Specify the components of a social link
#'
#' The `social_link()` function is used exclusively within
#' `block_social_links()` with as many calls as the number of social sharing
#' icons/links required. By providing a supported `service` name, a hosted icon
#' image can be used. A `link` must be provided; it will be part of social
#' sharing icon. All icons are rounded, transparent, and consist of a single
#' color, or level of gray.
#'
#' The following social sharing services have hosted icons available:
#' * Twitter - Micro-blogging internet service.
#' * GitHub - Web-based hosting service for software development projects using Git.
#' * Facebook - Global online social networking service.
#' * Instagram - Online photo-sharing and social networking service.
#' * LinkedIn - Social networking service for people in professional occupations.
#' * YouTube - Video-sharing service owned by Google.
#' * Vimeo - An ad-free open video platform.
#' * Behance - A site for self-promotion of design projects.
#' * Dribbble - Online community for showcasing user-made artwork.
#' * Pinterest - Photo-sharing and publishing website for discovering interesting things.
#' * 500px - Online platform for photographers to gain global exposure.
#' * Yelp - Local-search service powered by crowd-sourced reviews.
#' * TripAdvisor - Travel and restaurant website with reviews and accommodation bookings.
#' * WordPress - Blogging platform and content management system.
#' * Blogger - A blog-publishing service hosted by Google.
#' * Tumblr - Micro-blogging and social networking website.
#' * Deezer - Web-based music streaming service.
#' * SoundCloud - A music sharing website and publishing tool for music distribution.
#' * Meetup - A service used to organize online groups that host in-person events.
#' * Etsy - An e-commerce website focused on handmade or vintage items and supplies.
#' * Reddit - A social news aggregation, web content rating, and discussion website.
#' * Stack Overflow - Question and answer site for professional and enthusiast programmers.
#' * Youku - A video hosting service for user-made and professionally produced videos.
#' * Sina Weibo - Micro-blogging website and one of the biggest social media platforms in China.
#' * QQ - Instant messaging software service developed by Tencent.
#' * Douban - A Chinese social networking service with a reputation for high-quality content.
#'
#' @param service Either the name of a social sharing service or either of
#'   `website`, `email`, or `rss`.
#' @param link The relevant link to content on the `service`.
#' @param variant The variant of the icon to use. Options include `bw` (black
#'   and white, the default), `color`, `dark_gray`, `gray`, and `light_gray`.
#' @param alt Text description of image passed to the `alt` attribute inside of
#'   the image (`<img>`) tag for use when image loading is disabled and on
#'   screen readers. If not supplied, then the name of the `service` will be
#'   used as alt text.
#'
#' @examples
#' # Create an email message with some
#' # articles in the `body`; in the footer,
#' # add some social sharing icons linking
#' # to web content
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_title("Exciting Travel Destinations"),
#'         block_articles(
#'           article(
#'             image = "https://i.imgur.com/dxSXzGb.jpg",
#'             title = "Hong Kong",
#'             content =
#'               "Once home to fishermen and farmers,
#'               modern Hong Kong is a teeming,
#'               commercially-vibrant metropolis where
#'               Chinese and Western influences fuse."
#'           ),
#'           article(
#'             image = "https://i.imgur.com/bJzVIrG.jpg",
#'             title = "Australia",
#'             content =
#'               "Australia ranks as one of the best
#'               places to live in the world by all
#'               indices of income, human development,
#'               healthcare, and civil rights."
#'           )
#'         )
#'       ),
#'     footer =
#'       blocks(
#'         block_text("Thanks for reading! Find us here:"),
#'         block_social_links(
#'           social_link(
#'             service = "pinterest",
#'             link = "https://www.pinterest.ca/TravelLeisure/",
#'             variant = "color"
#'           ),
#'           social_link(
#'             service = "tripadvisor",
#'             link = "https://www.tripadvisor.ca/TravelersChoice",
#'             variant = "color"
#'           )
#'         )
#'       )
#'   )
#'
#' if (interactive()) email
#'
#' @export
social_link <- function(service,
  link,
  variant = NULL,
  alt = NULL) {

  # Lowercase the input given as `service`
  service <- tolower(service)

  # Generate a link to an image for the service
  icon <-
    icon_for_social_service(
      service = service,
      variant = variant
    )

  # Set default alt text if not provided
  if (is.null(alt)) {
    alt <- service
  }

  tags$a(.noWS = c("after-begin", "before-end"),
    href = link,
    tags$img(src = icon, alt = alt, width = "44", height = "44", style = "border: none;")
  )
}

#' A block of social sharing icons with links
#'
#' With `block_social_links()`, we can create a block of social sharing links
#' and links to websites, email, or RSS feeds. The function can accept as many
#' `social_link()` calls as seen fit to email. Like all `block_*()` functions,
#' `block_social_links()` must be placed inside of `blocks()` and the resultant
#' `blocks` object can be provided to the `body`, `header`, or `footer`
#' arguments of `compose_email()`.
#'
#' @param ... One or more calls to `social_link()`.
#'
#' @examples
#' # Create an email message with some
#' # articles in the `body`; in the footer,
#' # add some social sharing icons linking
#' # to web content using `block_social_links()`
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_title("Exciting Travel Destinations"),
#'         block_articles(
#'           article(
#'             image = "https://i.imgur.com/dxSXzGb.jpg",
#'             title = "Hong Kong",
#'             content =
#'               "Once home to fishermen and farmers,
#'               modern Hong Kong is a teeming,
#'               commercially-vibrant metropolis where
#'               Chinese and Western influences fuse."
#'           ),
#'           article(
#'             image = "https://i.imgur.com/bJzVIrG.jpg",
#'             title = "Australia",
#'             content =
#'               "Australia ranks as one of the best
#'               places to live in the world by all
#'               indices of income, human development,
#'               healthcare, and civil rights."
#'           )
#'         )
#'       ),
#'     footer =
#'       blocks(
#'         block_text("Thanks for reading! Find us here:"),
#'         block_social_links(
#'           social_link(
#'             service = "pinterest",
#'             link = "https://www.pinterest.ca/TravelLeisure/",
#'             variant = "color"
#'           ),
#'           social_link(
#'             service = "tripadvisor",
#'             link = "https://www.tripadvisor.ca/TravelersChoice",
#'             variant = "color"
#'           )
#'         )
#'       )
#'   )
#'
#' if (interactive()) email
#'
#' @export
block_social_links <- function(...) {
  panel(class = "message-block block_social", inner_align = "center", ...)
}

#' @noRd
icon_for_social_service <- function(service,
                                    variant = NULL) {

  # Normalize the provided `service` name
  service <-
    service %>%
    tolower() %>%
    tidy_gsub(" ", "")

  # Get a vector of names for all social services
  ss_names <-
    social_service_icons() %>%
    dplyr::pull(name) %>%
    tolower() %>%
    tidy_gsub(" ", "")

  # If a hosted icon isn't availble for the
  # `service`, stop the function
  if (!(service %in% ss_names)) {
    stop("An icon for the given `service` is not available\n",
         " * look inside the article at `?social_link` to see which are available\n",
         call. = FALSE)
  }

  # Ensure that the icon variant is valid; if
  # nothing is provided for variant, default to `bw`
  if (!is.null(variant) && !(variant %in% social_service_icon_variants)) {
    stop("The given `variant` is not available for the social icons\n",
         " * look inside the article at `?social_link` to see which are available\n",
         call. = FALSE)
  } else if (is.null(variant)) {
    variant <- "bw"
  }

  # Construct the link to the hosted image asset
  paste0("https://", social_icons_host_stub, "/", service, "-", variant, ".png")
}

social_icons_host_stub <-
  "raw.githubusercontent.com/rich-iannone/blastula/master/inst/social_icons"

#' @noRd
social_service_icons <- function() {

  dplyr::tribble(
    ~name,            ~description,
    "website",        "A link a website.",
    "email",          "An email address.",
    "RSS",            "An RSS feed.",
    "Twitter",        "Micro-blogging internet service.",
    "GitHub",         "Web-based hosting service for software development projects using Git.",
    "Facebook",       "Global online social networking service.",
    "Instagram",      "Online photo-sharing and social networking service.",
    "LinkedIn",       "Social networking service for people in professional occupations.",
    "YouTube",        "Video-sharing service owned by Google.",
    "Vimeo",          "An ad-free open video platform.",
    "Behance",        "A site for self-promotion of design projects.",
    "Dribbble",       "Online community for showcasing user-made artwork.",
    "Pinterest",      "Photo-sharing and publishing website for discovering interesting things.",
    "500px",          "Online platform for photographers to gain global exposure.",
    "Yelp",           "Local-search service powered by crowd-sourced reviews.",
    "TripAdvisor",    "Travel and restaurant website with reviews and accommodation bookings.",
    "WordPress",      "Blogging platform and content management system.",
    "Blogger",        "A blog-publishing service hosted by Google.",
    "Tumblr",         "Micro-blogging and social networking website.",
    "Deezer",         "Web-based music streaming service.",
    "SoundCloud",     "A music sharing website and publishing tool for music distribution.",
    "Meetup",         "A service used to organize online groups that host in-person events.",
    "Etsy",           "An e-commerce website focused on handmade or vintage items and supplies.",
    "Reddit",         "A social news aggregation, web content rating, and discussion website.",
    "Stack Overflow", "Question and answer site for professional and enthusiast programmers.",
    "Youku",          "A video hosting service for user-made and professionally produced videos.",
    "Sina Weibo",     "Micro-blogging website and one of the biggest social media platforms in China.",
    "QQ",             "Instant messaging software service developed by Tencent.",
    "Douban",         "A Chinese social networking service with a reputation for high-quality content.",
  )
}

social_service_icon_variants <-
  c(
    "color",
    "bw",
    "dark_gray",
    "gray",
    "light_gray"
  )
