#' Interpret input text as Markdown-formatted text
#'
#' @param text The text that is understood to contain Markdown formatting.
#' @return A character object that is tagged for a Markdown-to-HTML
#'   transformation.
#' @return A rendered HTML object.
#'
#' @export
md <- function(text) {
  HTML(commonmark::markdown_html(
    paste(collapse = "\n",
      as.character(text)
    )
  ))
}
