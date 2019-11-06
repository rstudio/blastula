#' Interpret input text as Markdown-formatted text
#'
#' @param text The text that is understood to contain Markdown formatting.
#' @return A character object that is tagged for a Markdown-to-HTML
#'   transformation.
#' @return a character object of class `from_markdown`.
#'
#' @export
md <- function(text) {

  if (!inherits(text, "character")) {
    stop("The input text must be of class `\"character\"`.",
         call. = FALSE)
  }

  # Apply the `from_markdown` class
  class(text) <- "from_markdown"
  text
}
