#' A block of text
#'
#' With `block_text()` we can define a text area and this can be easily combined
#' with other `block_*()` functions. The text will take the entire width of the
#' block and will resize according to screen width. Like all `block_*()`
#' functions, `block_text()` must be placed inside of `blocks()` and that object
#' can be provided to the `body` or `footer` argument of `compose_email()`.
#'
#' @param ... Paragraphs of text.
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article()` calls
#' # inside of `block_articles()`, itself
#' # placed in `blocks()`; also, include some
#' # text at the top with `block_text()`
#' compose_email(
#'   body =
#'     blocks(
#'       block_text(
#'         "These are two of the cities I visited this year. \\
#'         I liked them a lot, so, I'll visit them again!"),
#'       block_articles(
#'         article(
#'           image = "https://i.imgur.com/dig0HQ2.jpg",
#'           title = "Los Angeles",
#'           content =
#'             "I want to live in Los Angeles. \\
#'             Not the one in Los Angeles. \\
#'             No, not the one in South California. \\
#'             They got one in South Patagonia."
#'         ),
#'         article(
#'           image = "https://i.imgur.com/RUvqHV8.jpg",
#'           title = "New York",
#'           content =
#'             "Start spreading the news. \\
#'             I'm leaving today. \\
#'             I want to be a part of it. \\
#'             New York, New York."
#'         )
#'       )
#'     )
#'   )
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @export
block_text <- function(...) {

  x <- list(...)

  text <-
    paste(x %>% unlist(), collapse = "\n") %>%
    commonmark::markdown_html() %>%
    tidy_gsub("<p>", "<p class=\"align-center\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px; text-align: center;\">")

  glue::glue(text_block_template()) %>% as.character()
}

#' A template for a text HTML fragment
#' @noRd
text_block_template <- function() {

"              <tr>
                <td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 24px;\" valign=\"top\">
                  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
                    <tbody>
                      <tr>
                        <td style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
                          {text}
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </td>
              </tr>"
}
