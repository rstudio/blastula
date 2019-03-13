#' A block with large title text
#'
#' With `block_text()` we can define a title text area and this can be easily
#' combined with other `block_*()` functions. The title will take the entire
#' width of the block and will resize according to screen width. Like all
#' `block_*()` functions, `block_title()` must be placed in a list object and
#' that list can only be provided to the `blocks` argument of `compose_email()`.
#'
#' @param title A single of paragraph of text.
#' @importFrom commonmark markdown_html
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article_items()` calls
#' # inside of `block_articles()`, itself
#' # placed in a `list()`; also, include a
#' # title at the top with `block_title()`
#' compose_email(
#'   blocks =
#'     list(
#'       block_title(
#'         "Two Cities I Visited Recently"),
#'       block_articles(
#'         article_items(
#'           image = "https://i.imgur.com/dig0HQ2.jpg",
#'           title = "Los Angeles",
#'           content =
#'             "I want to live in Los Angeles. \\
#'             Not the one in Los Angeles. \\
#'             No, not the one in South California. \\
#'             They got one in South Patagonia."
#'         ),
#'         article_items(
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
#' @export
block_title <- function(title) {

  text <-
    title %>%
    commonmark::markdown_html() %>%
    tidy_gsub("<p>", "<h1 class=\"align-center\" style=\"color: #222222; font-family: Helvetica, sans-serif; font-weight: 300; line-height: 1.4; margin: 0; font-size: 36px; margin-bottom: 24px; text-transform: capitalize; text-align: center;\">") %>%
    tidy_gsub("</p>\n", "</h1>")

  glue::glue(text_block_template()) %>% as.character()
}

#' A template for a title text HTML fragment
#' @noRd
title_block_template <- function() {

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
