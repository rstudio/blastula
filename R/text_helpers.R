#' Interpret input text as Markdown-formatted text
#'
#' @param text The text that is understood to contain Markdown formatting.
#' @return A character object that is tagged for a Markdown-to-HTML
#'   transformation.
#' @return A rendered HTML object.
#'
#' @export
md <- function(text) {
  class(text) <- "from_markdown"
  text
}

process_markdown <- function(x, ...) {
  if (is.null(x)) {
    x
  } else if (inherits(x, "from_markdown")) {
    render_markdown(x, ...)
  } else if (is.list(x)) {
    x[] <- lapply(x, process_markdown, ...)
    x
  } else {
    x
  }
}

render_markdown <- function(x, ...) {
  HTML(commonmark::markdown_html(paste(collapse = "\n", as.character(x)), ...))
}
