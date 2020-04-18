#' A block of text
#'
#' With `block_text()` we can define a text area and this can be easily combined
#' with other `block_*()` functions. The text will take the entire width of the
#' block and will resize according to screen width. Like all `block_*()`
#' functions, `block_text()` must be placed inside of `blocks()` and the
#' resultant `blocks` object can be provided to the `body`, `header`, or
#' `footer` arguments of `compose_email()`.
#'
#' @param text Plain text or Markdown text (via [md()]).
#' @param align The text alignment to be used for this block of text. The
#'   default is `"center"`.
#'
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article()` calls
#' # inside of `block_articles()`, itself
#' # placed in `blocks()`; also, include some
#' # text at the top with `block_text()`
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_text(
#'           "These are two of the cities I visited this year.
#'           I liked them a lot, so, I'll visit them again!"),
#'         block_articles(
#'           article(
#'             image = "https://i.imgur.com/dig0HQ2.jpg",
#'             title = "Los Angeles",
#'             content =
#'               "I want to live in Los Angeles.
#'               Not the one in Los Angeles.
#'               No, not the one in South California.
#'               They got one in South Patagonia."
#'           ),
#'           article(
#'             image = "https://i.imgur.com/RUvqHV8.jpg",
#'             title = "New York",
#'             content =
#'               "Start spreading the news.
#'               I'm leaving today.
#'               I want to be a part of it.
#'               New York, New York."
#'           )
#'         )
#'       )
#'     )
#'
#' if (interactive()) email
#'
#' @export
block_text <- function(text, align = c("center", "left", "right", "justify")) {
  if (length(align) > 1) {
    align <- align[[1]]
  }

  tags$div(class = "message-block block_text", style = css(text_align = align),
    text
  )
}
