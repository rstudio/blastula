#' @importFrom htmltools tagList tags HTML div
NULL

#' Utility class for forming table-based containers (works better with Outlook
#' than divs, for example, this seems to be the only way to center a block).
#'
#' @noRd
panel <- function(
    ...,
    width = "100%",
    outer_align = NULL,
    inner_align = NULL,
    padding = NULL,
    margin = NULL,
    outer_class = NULL,
    inner_class = NULL,
    background_color = NULL,
    max_width = NULL
) {

  tags$table(
    width = width,
    align = outer_align,
    class = outer_class,
    style = css(
      margin = margin,
      background_color = background_color,
      max_width = htmltools::validateCssUnit(max_width)
    ),
    tags$tr(
      tags$td(align = inner_align, class = inner_class,
        style = css(
          padding = padding
        ),
        ...
      )
    )
  )
}
