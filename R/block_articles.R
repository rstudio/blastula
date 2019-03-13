#' Specify the components of an article
#'
#' The `article_items()` function is used exclusively within `block_articles()`,
#' and having one or two calls will arrange the article blocklets in a row (or
#' as a column at lower screen widths).
#'
#' @param image An optional URL pointing to an image resource.
#' @param title An optional title for the article.
#' @param content An optional paragraph of text for the article.
#' @examples
#' # Creating a block of two, side-by-side
#' # articles is possible with two calls
#' # of `article_items()`; remember to use
#' # `block_articles()` and put all of that
#' # in a `list()`
#' compose_email(
#'   blocks =
#'     list(
#'       block_articles(
#'         article_items(
#'           image = "https://i.imgur.com/dxSXzGb.jpg",
#'           title = "Hong Kong",
#'           content =
#'             "Once home to fishermen and farmers, \\
#'             modern Hong Kong is a teeming, \\
#'             commercially-vibrant metropolis where \\
#'             Chinese and Western influences fuse."
#'         ),
#'         article_items(
#'           image = "https://i.imgur.com/bJzVIrG.jpg",
#'           title = "Australia",
#'           content =
#'             "Australia ranks as one of the best \\
#'             places to live in the world by all \\
#'             indices of income, human development, \\
#'             healthcare and civil rights."
#'         )
#'       )
#'     )
#'   )
#' @export
article_items <- function(image = NULL,
                          title = NULL,
                          content = NULL) {

  if (is.null(image)) {
    image <- ""
  }

  if (is.null(title)) {
    title <- ""
  }

  if (is.null(content)) {
    content <- ""
  }

  # Add the article components to the
  # `article_item_list` object
  article_item_list <-
    list(
      image = image,
      title = title,
      content = content)

  # Apply the `article_items` class
  class(article_item_list) <- "article_items"

  article_item_list
}

#' A block of one or two articles with a multicolumn layout
#'
#' With `block_articles()`, we can create a single- or multi-column layout of
#' articles. The articles are responsive to the screen width, so side-by-side
#' articles will collapse and any of the optional images will resize
#' accordingly. The function can accept one or two `article_items()` calls, each
#' with varying amounts of text and imagery. Like all `block_*()` functions,
#' `block_articles()` must be placed in a list object and that list can only be
#' provided to the `blocks` argument of `compose_email()`.
#'
#' @param ... One or two calls to `article_items()`.
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article_items()` calls
#' # inside of `block_articles()`, itself
#' # placed in a `list()`
#' compose_email(
#'   blocks =
#'     list(
#'       block_articles(
#'         article_items(
#'           image = "https://i.imgur.com/XMU8yJa.jpg",
#'           title = "Taiwan",
#'           content =
#'             "It is a thriving mosaic of tradition, \\
#'             culture, and high-tech development, \\
#'             merging Eastern and Western influences."
#'         ),
#'         article_items(
#'           image = "https://i.imgur.com/aYOm3Tk.jpg",
#'           title = "Japan",
#'           content =
#'             "Japan is an archipelago consisting \\
#'             of 6,852 islands along East Asia's \\
#'             Pacific Coast."
#'         )
#'       )
#'     )
#'   )
#' @export
block_articles <- function(...) {

  x <- list(...)

  if (!all((lapply(x, class) %>% unlist()) %in% "article_items")) {

    stop("All objects provided to `block_articles()` must be of the ",
         "class `article_items`:\n",
         " * These objects are created by the `article_items()` function.",
         call. = FALSE)
  }

  if (length(x) > 3) {

    stop("There cannot be more than three `article_items` objects ",
         "provided to `block_articles()`.",
         call. = FALSE)
  }

  if (length(x) == 2) {
    return(block_article_2(items = x))
  }

  if (length(x) == 1) {
    return(block_article_1(items = x))
  }
}

#' Obtain an inlined HTML fragment for two side-by-side articles
#' @importFrom glue glue
#' @noRd
block_article_2 <- function(items) {

  if (items[[1]]$image == "") {
    x1_image <- ""
  } else {
    x1_image <-
      glue::glue(
        article_image_template(),
        image = items[[1]]$image
      )
  }

  if (items[[2]]$image == "") {
    x2_image <- ""
  } else {
    x2_image <-
      glue::glue(
        article_image_template(),
        image = items[[2]]$image
      )
  }

  x1_title <-
    glue::glue(
      article_title_template(),
      title = items[[1]]$title
    )

  x2_title <-
    glue::glue(
      article_title_template(),
      title = items[[2]]$title
    )

  x1_content <-
    glue::glue(
      article_content_template(),
      content = items[[1]]$content
    )

  x2_content <-
    glue::glue(
      article_content_template(),
      content = items[[2]]$content
    )

  glue::glue(
  "<tr>
  <td align=\"center\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
    <!--[if (gte mso 9)|(IE)]>
    <table align=\"left\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%\">
      <tr>
        <td align=\"left\" valign=\"top\" width=\"50%\">
          <![endif]-->
          <div class=\"span-3\" style=\"display: inline-block; margin-bottom: 24px; vertical-align: top; width: 100%; max-width: 298px;\">
            <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px; max-width: 298px;\" width=\"100%\">
              <tbody>
                {x1_image}
                {x1_title}
                {x1_content}
              </tbody>
            </table>
          </div>
          <!--[if (gte mso 9)|(IE)]>
        </td>
        <td align=\"left\" valign=\"top\" width=\"50%\">
          <![endif]-->
          <div class=\"span-3\" style=\"display: inline-block; margin-bottom: 24px; vertical-align: top; width: 100%; max-width: 298px;\">
            <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px; max-width: 298px;\" width=\"100%\">
              <tbody>
                {x2_image}
                {x2_title}
                {x2_content}
              </tbody>
            </table>
          </div>
          <!--[if (gte mso 9)|(IE)]>
        </td>
      </tr>
    </table>
    <![endif]-->
  </td>
</tr>"
  ) %>% as.character()
}

#' Obtain an inlined HTML fragment for a single, full-width article
#' @importFrom glue glue
#' @noRd
block_article_1 <- function(items) {

  if (items[[1]]$image == "") {
    x1_image <- ""
  } else {
    x1_image <-
      glue::glue(
        article_image_template_1(),
        image = items[[1]]$image)
  }

  x1_content <-
    glue::glue(
      article_content_template_1(),
      content = items[[1]]$content
    )

  glue::glue(
    "<tr>
  <td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 24px;\" valign=\"top\">
    <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
      <tbody>
        <tr>
          <td style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
            {x1_image}
            {x1_content}
          </td>
        </tr>
      </tbody>
    </table>
  </td>
</tr>") %>% as.character()
}

#' A template for an article image HTML fragment
#' @noRd
article_image_template <- function() {

"                <tr>
                  <td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
                    <img src=\"{image}\" alt=\"placeholder for image text\" width=\"250\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\">
                  </td>
                </tr>
  "
}

#' A template for an article image HTML fragment that spans the full width
#' @noRd
article_image_template_1 <- function() {

"            <p style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px;\">
              <img src=\"{image}\" alt=\"placeholder for image text\" width=\"552\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\">
            </p>
  "
}

#' A template for an article content HTML fragment, spanning the full width
#' @noRd
article_content_template_1 <- function() {

"            <p style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px;\">
              {content}
            </p>
  "
}

#' A template for an article title HTML fragment
#' @noRd
article_title_template <- function() {

"                <tr>
                  <td class=\"article-title\" style=\"font-family: Helvetica, sans-serif; vertical-align: top; font-size: 14px; font-weight: 800; line-height: 1.4em; padding-bottom: 8px;\" valign=\"top\">
                    <a href=\"\" target=\"_blank\" style=\"color: #222222; text-decoration: none; font-size: 14px; font-weight: 800; line-height: 1.4em;\">{title}</a>
                  </td>
                </tr>
  "
}

#' A template for article content as an HTML fragment
#' @noRd
article_content_template <- function() {

"                <tr>
                  <td class=\"article-content\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; font-weight: normal; padding-bottom: 8px;\" valign=\"top\">
                    {content}
                  </td>
                </tr>
  "
}
