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

  class(title) <- c("block_title", class(title))

  title
}

#' @noRd
render_block_title <- function(x, context = "body") {

  if (context == "body") {

    font_size <- 36
    font_color <- "#222222"
    margin_bottom <- 4
    padding <- 12

  } else if (context %in% c("header", "footer")) {

    font_size <- 20
    font_color <- "#999999"
    margin_bottom <- 0
    padding <- 10
  }

  title_line_rendered <-
    title_line_template %>%
    tidy_gsub("\\{font_color\\}", font_color %>% htmltools::htmlEscape(attribute = TRUE)) %>%
    tidy_gsub("\\{font_size\\}", font_size %>% htmltools::htmlEscape(attribute = TRUE)) %>%
    tidy_gsub("\\{margin_bottom\\}", margin_bottom %>% htmltools::htmlEscape(attribute = TRUE)) %>%
    tidy_gsub("\\{padding\\}", padding %>% htmltools::htmlEscape(attribute = TRUE)) %>%
    tidy_gsub("\\{title\\}", x %>% process_text())

  title_block_template %>%
    tidy_gsub("\\{padding\\}", padding %>% htmltools::htmlEscape(attribute = TRUE)) %>%
    tidy_gsub("\\{text\\}", title_line_rendered)

}

title_line_template <-
  "<h1 class=\"align-center\" style=\"color: {font_color}; font-family: Helvetica, sans-serif; font-weight: 300; line-height: 1.4; margin: 0; font-size: {font_size}px; margin-bottom: {margin_bottom}px; text-transform: capitalize; text-align: center;\">{title}</h1>"

title_block_template <-
"<tr>
<td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: {padding}px;\" valign=\"top\">
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
