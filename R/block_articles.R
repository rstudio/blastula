#' Specify the components of an article
#'
#' The `article()` function is used exclusively within `block_articles()`,
#' and having one, two, or three calls will arrange the articles in a row (or as
#' a column of articles at lower screen widths).
#'
#' @param image An optional URL pointing to an image resource.
#' @param title An optional title for the article. Markdown is supported.
#' @param content An optional paragraph of text for the article. Markdown is
#'   supported.
#' @param link An optional link to apply to the content elements.
#'
#' @examples
#' # We can define an article with a link
#' # to an image, title text, some content,
#' # and a link to relevant content
#' article <-
#'   article(
#'     image = "https://i.imgur.com/dxSXzGb.jpg",
#'     title = "Hong Kong",
#'     content =
#'       "Once home to fishermen and farmers, \\
#'       modern _Hong Kong_ is a teeming, \\
#'       commercially-vibrant metropolis where \\
#'       Chinese and Western influences fuse.",
#'     link = "http://www.discoverhongkong.com"
#'   )
#'
#' if (interactive()) article
#' @export
article <- function(image = NULL,
                    title = NULL,
                    content = NULL,
                    link = NULL) {

  if (is.null(image)) {
    image <- ""
  }

  if (is.null(title)) {
    title <- ""
  } else {
    title <- glue::glue(title) %>% as.character()
  }

  if (is.null(content)) {
    content <- ""
  } else {
    content <- glue::glue(content) %>% as.character()
  }

  if (is.null(link)) {
    link <- ""
  }

  # Add the article components to the
  # `article_item_list` object
  article_item_list <-
    list(
      image = image,
      title = title,
      content = content,
      link = link)

  # Apply the `article` class
  class(article_item_list) <- "article"

  article_item_list
}

#' A block of one, two, or three articles with a multicolumn layout
#'
#' With `block_articles()`, we can create a single- or multi-column layout of
#' articles. The articles are responsive to the screen width, so side-by-side
#' articles will collapse and any of the optional images will resize
#' accordingly. The function can accept one to three `article()` calls, each
#' with varying amounts of text and imagery. Like all `block_*()` functions,
#' `block_articles()` must be placed inside of `blocks()` and the resultant
#' `blocks` object can be provided to the `body`, `header`, or `footer`
#' arguments of `compose_email()`.
#'
#' @param ... One, two, or three calls to `article()`.
#'
#' @examples
#' # Create a block of three, side-by-side
#' # articles with three `article()`
#' # calls inside of `block_articles()`,
#' # itself placed in `blocks()`
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_articles(
#'           article(
#'             image = "https://i.imgur.com/XMU8yJa.jpg",
#'             title = "Taiwan",
#'             content =
#'               "It is a thriving mosaic of tradition, \\
#'               culture, and high-tech development, \\
#'               merging Eastern and Western influences."
#'           ),
#'           article(
#'             image = "https://i.imgur.com/aYOm3Tk.jpg",
#'             title = "Japan",
#'             content =
#'               "Japan is an archipelago consisting \\
#'               of 6,852 islands along East Asia's \\
#'               Pacific Coast."
#'           ),
#'           article(
#'              image = "https://i.imgur.com/ekjFVOL.jpg",
#'              title = "Singapore",
#'              content =
#'                "Singapore is an island city-state \\
#'                in Southeast Asia. It's lies at the \\
#'                southern tip of the Malay Peninsula."
#'           )
#'         )
#'       )
#'     )
#'
#' if (interactive()) email
#'
#' @export
block_articles <- function(...) {

  x <- list(...)

  if (!all((lapply(x, class) %>% unlist()) %in% "article")) {

    stop("All objects provided to `block_articles()` must be of the ",
         "class `article`:\n",
         " * These objects are created by the `article()` function.",
         call. = FALSE)
  }

  if (length(x) > 3) {

    stop("There cannot be more than three `article` objects ",
         "provided to `block_articles()`.",
         call. = FALSE)
  }

  class(x) <- "block_articles"

  x
}

render_block_articles <- function(x) {

  if (length(x) == 3) {
    return(block_article_3(items = x))
  }

  if (length(x) == 2) {
    return(block_article_2(items = x))
  }

  if (length(x) == 1) {
    return(block_article_1(items = x))
  }
}

#' Obtain an inlined HTML fragment for three side-by-side articles
#'
#' @noRd
block_article_3 <- function(items) {

  if (items[[1]]$image == "") {
    x1_image <- ""
  } else {
    x1_image <-
      glue::glue(
        article_image_template_3(),
        image = items[[1]]$image,
        link = items[[1]]$link
      )
  }

  if (items[[2]]$image == "") {
    x2_image <- ""
  } else {
    x2_image <-
      glue::glue(
        article_image_template_3(),
        image = items[[2]]$image,
        link = items[[2]]$link
      )
  }

  if (items[[3]]$image == "") {
    x3_image <- ""
  } else {
    x3_image <-
      glue::glue(
        article_image_template_3(),
        image = items[[3]]$image,
        link = items[[3]]$link
      )
  }

  paragraph <- "<p style=\"margin: 0\">"

  x1_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[1]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[1]]$link
    )

  x2_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[2]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[2]]$link
    )

  x3_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[3]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[3]]$link
    )

  x1_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[1]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  x2_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[2]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  x3_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[3]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  block <-
    glue::glue(
"<tr>
<td align=\"center\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
<!--[if (gte mso 9)|(IE)]>
<table align=\"left\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%\">
<tr>
<td align=\"left\" valign=\"top\" width=\"33.333%\">
<![endif]-->
<div class=\"span-2\" style=\"display: inline-block; margin-bottom: 24px; vertical-align: top; width: 100%; max-width: 197px;\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" align=\"left\" width=\"100%\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px; max-width: 197px;\">
<tbody>
{x1_image}
{x1_title}
{x1_content}
</tbody>
</table>
</div>
<!--[if (gte mso 9)|(IE)]>
<table align=\"left\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%\">
<tr>
<td align=\"left\" valign=\"top\" width=\"33.333%\">
<![endif]-->
<div class=\"span-2\" style=\"display: inline-block; margin-bottom: 24px; vertical-align: top; width: 100%; max-width: 197px;\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" align=\"left\" width=\"100%\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px; max-width: 197px;\">
<tbody>
{x2_image}
{x2_title}
{x2_content}
</tbody>
</table>
</div>
<!--[if (gte mso 9)|(IE)]>
<table align=\"left\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" width=\"100%\">
<tr>
<td align=\"left\" valign=\"top\" width=\"33.333%\">
<![endif]-->
<div class=\"span-2\" style=\"display: inline-block; margin-bottom: 24px; vertical-align: top; width: 100%; max-width: 197px;\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" align=\"left\" width=\"100%\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px; max-width: 197px;\">
<tbody>
{x3_image}
{x3_title}
{x3_content}
</tbody>
</table>
</div>
<!--[if (gte mso 9)|(IE)]>
</td>
</tr>
</table>
<![endif]-->
</td>
</tr>") %>% as.character()

  class(block) <- "block_articles"

  block
}

#' Obtain an inlined HTML fragment for two side-by-side articles
#'
#' @noRd
block_article_2 <- function(items) {

  if (items[[1]]$image == "") {
    x1_image <- ""
  } else {
    x1_image <-
      glue::glue(
        article_image_template_2(),
        image = items[[1]]$image,
        link = items[[1]]$link
      )
  }

  if (items[[2]]$image == "") {
    x2_image <- ""
  } else {
    x2_image <-
      glue::glue(
        article_image_template_2(),
        image = items[[2]]$image,
        link = items[[2]]$link
      )
  }

  paragraph <- "<p style=\"margin: 0\">"

  x1_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[1]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[1]]$link
    )

  x2_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[2]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[2]]$link
    )

  x1_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[1]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  x2_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[2]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  block <-
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
</tr>") %>% as.character()

  class(block) <- "block_articles"

  block
}

#' Obtain an inlined HTML fragment for a single, full-width article
#'
#' @noRd
block_article_1 <- function(items) {

  if (items[[1]]$image == "") {
    x1_image <- ""
  } else {
    x1_image <-
      glue::glue(
        article_image_template_1(),
        image = items[[1]]$image,
        link = items[[1]]$link)
  }

  paragraph <- "<p style=\"margin: 0;\">"

  x1_title <-
    glue::glue(
      article_title_template(),
      title =
        items[[1]]$title %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph),
      link = items[[1]]$link
    )

  x1_content <-
    glue::glue(
      article_content_template(),
      content =
        items[[1]]$content %>%
        commonmark::markdown_html() %>%
        tidy_gsub("<p>", paragraph)
    )

  block <-
    glue::glue(
"<tr>
<td align=\"center\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"article\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; padding-left: 24px; padding-right: 24px;\" width=\"100%\">
<tbody>
{x1_image}
{x1_title}
{x1_content}
</tbody>
</table>
</td>
</tr>") %>% as.character()

  class(block) <- "block_articles"

  block
}

#' A template for an article image HTML fragment (three across)
#' @noRd
article_image_template_3 <- function() {

"<tr>
<td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"149\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</td>
</tr>
"
}

#' A template for an article image HTML fragment (two across)
#' @noRd
article_image_template_2 <- function() {

"<tr>
<td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"250\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</td>
</tr>
"
}

#' A template for an article image HTML fragment (one across)
#' @noRd
article_image_template_1 <- function() {

"<tr>
<td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"552\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</td>
</tr>
"
}

#' A template for an article title HTML fragment
#' @noRd
article_title_template <- function() {

"<tr>
<td class=\"article-title\" style=\"font-family: Helvetica, sans-serif; vertical-align: top; font-size: 14px; font-weight: 800; line-height: 1.4em; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\" style=\"color: #222222; text-decoration: none; font-size: 14px; font-weight: 800; line-height: 1.4em;\">{title}</a>
</td>
</tr>
"
}

#' A template for an article content HTML fragment
#' @noRd
article_content_template <- function() {

"<tr>
<td class=\"article-content\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; font-weight: normal; padding-bottom: 8px;\" valign=\"top\">
{content}
</td>
</tr>
"
}

# nocov start

#' Print a block of articles
#'
#' This facilitates printing of a block of articles to the Viewer.
#' @param x an object of class \code{block_articles}.
#' @keywords internal
#' @export
print.block_articles <- function(x, ...) {

  x %>%
    render_block_articles() %>%
    htmltools::HTML() %>%
    htmltools::html_print()
}

#' Print an article component in the console
#'
#' This facilitates printing of an article object to the console.
#' @param x an object of class \code{article}.
#' @keywords internal
#' @export
print.article <- function(x, ...) {

  glue::glue("
image: {x$image}
title: {x$title}
content: {x$content}
link: {x$link}
"
  ) %>%
    as.character() %>%
    cat()
}

# nocov end
