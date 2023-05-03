#' Create an HTML fragment for a CTA button
#'
#' Create the HTML fragment for a call to action button. This can be used as
#' part of the email body but, since this HTML, it must be contained within
#' [md()]. There are options to specify the button text, the URL, and the
#' button's alignment.
#'
#' @param url A URL for the button.
#' @param text The text that is placed atop the CTA button.
#' @param align The alignment of the button inside the main content area.
#'   Options are `"center"` (the default), `"left"`, and `"right"`.
#'
#' @return A character object with an HTML fragment that can be placed inside
#'   the message body wherever the CTA button should appear.
#'
#' @examples
#' # Create the button as an HTML fragment
#' cta_button <-
#'   add_cta_button(
#'     url = "http://www.website.net",
#'     text = "Press This Button"
#'   )
#'
#' # Include the button in the email
#' # message body by using it as part of
#' # a vector inside of `md()`
#' email <-
#'   compose_email(
#'     body = md(
#'       c(
#'   "Pressing the button will take
#'   you to an example website",
#'   cta_button
#'       )
#'     )
#'   )
#'
#' if (interactive()) email
#'
#' @export
add_cta_button <- function(
    url,
    text,
    align = "center"
) {

  HTML(
    as.character(
      tags$table(
        align = align,
        tags$tr(
          tags$td(
            style = css(
              background_color = "#2288DD",
              border_radius = "6px",
              padding = "10px 20px"
            ),
            tags$a(
              style = css(
                color = "white",
                text_decoration = "none",
                font_weight = "bold"
              ),
              href = url,
              text
            )
          )
        )
      )
    )
  )
}
