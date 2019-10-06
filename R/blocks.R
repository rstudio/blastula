#' An enclosure for all HTML block functions
#'
#' To contain all of the block-based HTML `block_*()` calls, we should use
#' the `blocks()` function. We can pass the resulting `blocks` object to either
#' of the `body`, `header`, and `footer` arguments of `compose_email()`.
#'
#' @param ... One or more `block_*()` calls.
#'
#' @examples
#' # This is an example of how a
#' # title and text looks in each of
#' # the three content areas
#' email <-
#'   compose_email(
#'     header =
#'       blocks(
#'         block_title("This is a Title in the **Header**"),
#'         block_text("This is text in the **Header**.")
#'       ),
#'     body =
#'       blocks(
#'         block_title("This is a Title in the **Body**"),
#'         block_text("This is text in the **Body**.")
#'       ),
#'     footer =
#'       blocks(
#'         block_title("This is a Title in the **Footer**"),
#'         block_text("This is text in the **Footer**.")
#'       )
#'   )
#'
#' if (interactive()) email
#'
#' @export
blocks <- function(...) {

  x <- list(...)

  # Should the blocks begin with articles, prepend an empty
  # block of text to serve as a spacer
  if (inherits(x[[1]], "block_articles")) {

    x <- prepend_list(x, list(block_text(" ")))
  }

  # Apply the `blocks` and `list` classes
  class(x) <- c("blocks", "list")
  x
}
