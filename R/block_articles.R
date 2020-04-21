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
article <- function(image = NULL, title = NULL, content = NULL, link = NULL,
  legacy_width = NULL) {
  maybe_link <- function(...) {
    if (is.null(link)) {
      tagList(...)
    } else {
      tags$a(.noWS = c("after-begin", "before-end"), href = link,
        style = css(text_decoration = "none"),
        ...
      )
    }
  }

  # This is a bit weird; we return a function instead of HTML. This is because
  # we don't have enough information to finish the render just yet, it depends
  # on how many other images we're going to be rendered next to.

  tagList(
    if (!is.null(image)) {
      tags$div(style = css(margin_bottom = "12px"),
        maybe_link(
          tags$img(
            src = image,
            width = "100%",
            # Height is hardcoded to 200 because on Outlook 2019 for Windows,
            # the image https://i.imgur.com/5aJawp2.jpg would show up as a
            # horizontal sliver if height was not included OR set to "auto" OR
            # set to 0. Bizarrely, if height is set to ANY POSITIVE VALUE, the
            # img displays at the CORRECT natural height (as if height="auto")
            # (and again, I only saw this with that particular image, though
            # other seemingly-similar images https://i.imgur.com/18fcpkZ.jpg
            # and https://i.imgur.com/gpVMFcW.jpg had no such problem.)
            height = 200,
            style = css(
              height = "auto !important"
            )
          )
        )
      )
    },
    if (!is.null(title)) {
      tags$h3(style = css(margin = 0), maybe_link(title))
    },
    if (!is.null(content)) {
      tags$div(content)
    }
  )
}

# To allow articles to be snapshot tested using testthat::verify_output
print.article <- function(x, ...) {
  print(x(NULL))
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

  pct <- round(100 / length(x))

  div(class = "message-block block_articles",
    tags$table(class = "articles", cellspacing = "12",
      tags$tr(
        lapply(x, function(article) {
          tags$td(class = "article", valign = "top", width = paste0(pct, "%"),
            article
          )
        })
      )
    )
  )
}
