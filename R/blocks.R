#' An enclosure for all HTML block constructors
#'
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
