#' A spacer block
#'
#' With `block_spacer()` we can more easily define an area of whitespace in a
#' block-based layout. This function is meant to be easily combined with other
#' `block_*()` functions. Like all `block_*()` functions, `block_spacer()` must
#' be placed inside of `blocks()` and the resultant `blocks` object can be
#' provided to the `body`, `header`, or `footer` arguments of `compose_email()`.
#'
#' @examples
#' # Create a block of two, side-by-side
#' # articles with two `article()` calls
#' # inside of `block_articles()`, itself
#' # placed in `blocks()`; include some
#' # introductory text and place extra
#' # space around that text (with
#' # `block_spacer()`)
#' email <-
#'   compose_email(
#'     body =
#'       blocks(
#'         block_spacer(),
#'         block_text(
#'           "These are two of the cities I visited this year.
#'           I liked them a lot, so, I'll visit them again!"),
#'         block_spacer(),
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
block_spacer <- function() {

  x <- list(a = "&nbsp;")

  class(x) <- "block_spacer"

  x
}

#' @noRd
render_block_spacer <- function(context = "body") {

  if (context == "body") {

    padding <- 4

  } else if (context %in% c("header", "footer")) {

    padding <- 0
  }

  spacer_block_template() %>%
    tidy_gsub(
      "\\{padding\\}",
      padding %>% htmltools::htmlEscape(attribute = TRUE)
    )
}

#' A template for a spacer HTML fragment
#' @noRd
spacer_block_template <- function() {

"<tr>
<td class=\"wrapper\" style=\"vertical-align: top; box-sizing: border-box; padding: {padding}px;\" valign=\"top\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
<tbody>
<tr>
<td style=\"vertical-align: top;\" valign=\"top\">
&nbsp;
</td>
</tr>
</tbody>
</table>
</td>
</tr>"
}
