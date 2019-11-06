#' Specify the components of an article
#'
#' The `article()` function is used exclusively within `block_articles()`,
#' and having one, two, or three calls will arrange the articles in a row (or as
#' a column of articles at lower screen widths).
#'
#' @param image An optional URL pointing to an image resource.
#' @param title An optional title for the article.
#' @param content An optional paragraph of text for the article.
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
#'       modern Hong Kong is a teeming, \\
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

  # Normalize inputs to empty strings if any are `NULL`
  image <- image %||% ""
  title <- title %||% ""
  content <- content %||% ""
  link <- link %||% ""

  # Add the article components to the
  # `article_item_list` object
  article_item_list <-
    list(
      image = image,
      title = title,
      content = content,
      link = link
    )

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
#'               "It is a thriving mosaic of tradition,
#'               culture, and high-tech development,
#'               merging Eastern and Western influences."
#'           ),
#'           article(
#'             image = "https://i.imgur.com/aYOm3Tk.jpg",
#'             title = "Japan",
#'             content =
#'               "Japan is an archipelago consisting
#'               of 6,852 islands along East Asia's
#'               Pacific Coast."
#'           ),
#'           article(
#'              image = "https://i.imgur.com/ekjFVOL.jpg",
#'              title = "Singapore",
#'              content =
#'                "Singapore is an island city-state
#'                in Southeast Asia. It's lies at the
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

  block <-
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
</tr>"

  for (i in seq(items)) {

    if (items[[i]]$image == "") {
      image <- ""
    } else {
      image <-
        article_image_template_3 %>%
        tidy_gsub("\\{image\\}", items[[i]]$image %>% process_text()) %>%
        tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())
    }

    title <-
      article_title_template %>%
      tidy_gsub("\\{title\\}", items[[i]]$title %>% process_text()) %>%
      tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())

    content <-
      article_content_template_2 %>%
      tidy_gsub("\\{content\\}", items[[i]]$content %>% process_text())

    block <-
      block %>%
      tidy_gsub(paste0("\\{x", i, "_image\\}"), image) %>%
      tidy_gsub(paste0("\\{x", i, "_title\\}"), title) %>%
      tidy_gsub(paste0("\\{x", i, "_content\\}"), content)
  }

  class(block) <- "block_articles"

  block
}

#' Obtain an inlined HTML fragment for two side-by-side articles
#'
#' @noRd
block_article_2 <- function(items) {

  block <-
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

  for (i in seq(items)) {

    if (items[[i]]$image == "") {
      image <- ""
    } else {
      image <-
        article_image_template_2 %>%
        tidy_gsub("\\{image\\}", items[[i]]$image %>% process_text()) %>%
        tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())
    }

    title <-
      article_title_template %>%
      tidy_gsub("\\{title\\}", items[[i]]$title %>% process_text()) %>%
      tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())

    content <-
      article_content_template_2 %>%
      tidy_gsub("\\{content\\}", items[[i]]$content %>% process_text())

    block <-
      block %>%
      tidy_gsub(paste0("\\{x", i, "_image\\}"), image) %>%
      tidy_gsub(paste0("\\{x", i, "_title\\}"), title) %>%
      tidy_gsub(paste0("\\{x", i, "_content\\}"), content)
  }

  class(block) <- "block_articles"

  block
}

#' Obtain an inlined HTML fragment for a single, full-width article
#'
#' @noRd
block_article_1 <- function(items) {

  block <-
"<tr>
<td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 24px;\" valign=\"top\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
<tbody>
<tr>
<td style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
{x1_image}
{x1_title}
{x1_content}
</td>
</tr>
</tbody>
</table>
</td>
</tr>"

  for (i in seq(items)) {

    if (items[[i]]$image == "") {
      image <- ""
    } else {
      image <-
        article_image_template_1 %>%
        tidy_gsub("\\{image\\}", items[[i]]$image %>% process_text()) %>%
        tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())
    }

    title <-
      article_title_template %>%
      tidy_gsub("\\{title\\}", items[[i]]$title %>% process_text()) %>%
      tidy_gsub("\\{link\\}", items[[i]]$link %>% process_text())

    content <-
      article_content_template_1 %>%
      tidy_gsub("\\{content\\}", items[[i]]$content %>% process_text())

    block <-
      block %>%
      tidy_gsub(paste0("\\{x", i, "_image\\}"), image) %>%
      tidy_gsub(paste0("\\{x", i, "_title\\}"), title) %>%
      tidy_gsub(paste0("\\{x", i, "_content\\}"), content)
  }

  class(block) <- "block_articles"

  block
}

article_image_template_3 <-
"<tr>
<td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"149\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</td>
</tr>
"

article_image_template_2 <-
"<tr>
<td class=\"article-thumbnail\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"250\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</td>
</tr>
"

article_image_template_1 <-
"<p style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px;\">
<a href=\"{link}\" target=\"_blank\"><img src=\"{image}\" alt=\"image text\" width=\"552\" class=\"img-responsive img-block\" style=\"border: none; -ms-interpolation-mode: bicubic; max-width: 100%; display: block;\"></a>
</p>
"

article_title_template <-
"<tr>
<td class=\"article-title\" style=\"font-family: Helvetica, sans-serif; vertical-align: top; font-size: 14px; font-weight: 800; line-height: 1.4em; padding-bottom: 8px;\" valign=\"top\">
<a href=\"{link}\" target=\"_blank\" style=\"color: #222222; text-decoration: none; font-size: 14px; font-weight: 800; line-height: 1.4em;\">{title}</a>
</td>
</tr>
"

article_content_template_1 <-
"<p style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px;\">
{content}
</p>
"

article_content_template_2 <-
"<tr>
<td class=\"article-content\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; font-weight: normal; padding-bottom: 8px;\" valign=\"top\">
{content}
</td>
</tr>
"
