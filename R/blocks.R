#' An enclosure for all HTML block functions
#'
#' To contain all of the block-based HTML `block_*()` calls, we should use
#' the `blocks()` function. We can pass the resulting `blocks` object to either
#' of the `body`, `header`, and `footer` arguments of `compose_email()`.
#' @param ... One or more `block_*()` calls.
#' @export
blocks <- function(...) {

  x <- list(...)

  # Should the blocks begin with articles, prepend an empty
  # block of text to serve as a spacer
  if (inherits(x[[1]], "block_articles")) {

    x <- prepend_list(x, block_text(" "))
  }

  # Apply the `blocks` and `list` classes
  class(x) <- c("blocks", "list")
  x
}
