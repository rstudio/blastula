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
block_text <- function(text) {

  class(text) <- c("block_text", class(text))

  text
}

#' @noRd
render_block_text <- function(x, context = "body") {

  if (context == "body") {

    font_color <- "#000000"
    font_size <- 14
    margin_bottom <- 12
    padding <- 12

  } else if (context %in% c("header", "footer")) {

    font_color <- "#999999"
    font_size <- 12
    margin_bottom <- 12
    padding <- 10
  }

  paragraph <-
    text_line_template() %>%
    tidy_gsub("\\{font_color\\}", font_color %>% process_text()) %>%
    tidy_gsub("\\{font_size\\}", font_size %>% as.character() %>% process_text()) %>%
    tidy_gsub("\\{margin_bottom\\}", margin_bottom %>% as.character() %>% process_text()) %>%
    tidy_gsub("\\{padding\\}", padding %>% as.character() %>% process_text())

  text <-
    x %>%
    process_text() %>%
    tidy_gsub("<p>", paragraph)

  text_block_template() %>%
    tidy_gsub("\\{font_size\\}", font_size %>% as.character() %>% process_text()) %>%
    tidy_gsub("\\{padding\\}", padding %>% as.character() %>% process_text()) %>%
    tidy_gsub("\\{text\\}", text)
}

text_line_template <- function() {
  "<p class=\"align-center\" style=\"font-family: Helvetica, sans-serif; color: {font_color};font-size: {font_size}px; font-weight: normal; margin: 0; margin-bottom: {margin_bottom}px; text-align: center;\">"
}

#' A template for a text HTML fragment
#' @noRd
text_block_template <- function() {

"<tr>
<td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: {font_size}px; vertical-align: top; box-sizing: border-box; padding: {padding}px;\" valign=\"top\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
<tbody>
<tr>
<td style=\"font-family: Helvetica, sans-serif; font-size: {font_size}px; vertical-align: top;\" valign=\"top\">
{text}
</td>
</tr>
</tbody>
</table>
</td>
</tr>"
}
