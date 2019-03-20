#' A block with large title text
#'
#' With `block_title()` we can define a title text area and this can be easily
#' combined with other `block_*()` functions. The title will take the entire
#' width of the block and will resize according to screen width. Like all
#' `block_*()` functions, `block_title()` must be placed inside of `blocks()`
#' and the resultant `blocks` object can be provided to the `body`, `header`, or
#' `footer` arguments of `compose_email()`.
#'
#' @param ... Paragraphs of title text.
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article()` calls
#' # inside of `block_articles()`, itself
#' # placed in `blocks()`; also, include a
#' # title at the top with `block_title()`
#' compose_email(
#'   body =
#'     blocks(
#'       block_title(
#'         "Two Cities I Visited Recently"),
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
#' @export
block_title <- function(...) {

  x <- list(...)

  class(x) <- "block_title"

  x
}

#' @importFrom commonmark markdown_html
#' @importFrom glue glue
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

  paragraph <-
    glue::glue(
      "<h1 class=\"align-center\" style=\"color: {font_color}; font-family: Helvetica, sans-serif; font-weight: 300; line-height: 1.4; margin: 0; font-size: {font_size}px; margin-bottom: {margin_bottom}px; text-transform: capitalize; text-align: center;\">"
    ) %>%
    as.character()

  text <-
    paste(x %>% unlist(), collapse = "\n") %>%
    commonmark::markdown_html() %>%
    tidy_gsub("<p>", paragraph) %>%
    tidy_gsub("</p>\n", "</h1>")

  glue::glue(title_block_template()) %>% as.character()
}


#' A template for a title text HTML fragment
#' @noRd
title_block_template <- function() {

"              <tr>
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
}
