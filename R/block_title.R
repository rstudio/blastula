#' A block with large title text
#'
#' With `block_title()` we can define a title text area and this can be easily
#' combined with other `block_*()` functions. The title will take the entire
#' width of the block and will resize according to screen width. Like all
#' `block_*()` functions, `block_title()` must be placed inside of `blocks()`
#' and the resultant `blocks` object can be provided to the `body`, `header`, or
#' `footer` arguments of `compose_email()`.
#'
#' @param title Plain text or Markdown text (via [md()]) for the title.
#'
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article()` calls
#' # inside of `block_articles()`, itself
#' # placed in `blocks()`; also, include a
#' # title at the top with `block_title()`
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_title("Two Cities I Visited Recently"),
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
block_title <- function(title) {
  tags$h1(class = "message-block block_title",
    style = css(
      color = "#222222",
      font_weight = "300",
      line_height = "1.4",
      margin = "0",
      font_size = "36px",
      margin_bottom = "4px",
      text_align = "center"
    ),
    title
  )
}
