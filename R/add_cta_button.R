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
#' compose_email(
#'   body = md(
#'     c(
#' "Pressing the button will take
#' you to an example website",
#' cta_button
#'     )
#'   )
#' )
#'
#' if (interactive()) email
#'
#' @export
add_cta_button <- function(url,
                           text,
                           align = "center") {

  paste0(
"
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"btn btn-primary\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; box-sizing: border-box;\">
<tbody>
<tr>
<td align=\"", align, "\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 15px;\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: auto;\">
<tbody>
<tr>
<td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; background-color: #3498db; border-radius: 5px; text-align: center;\"> <a href=\"", url, "\" target=\"_blank\" style=\"display: inline-block; color: #ffffff; background-color: #3498db; border: solid 1px #3498db; border-radius: 5px; box-sizing: border-box; cursor: pointer; text-decoration: none; font-size: 14px; font-weight: bold; margin: 0; padding: 12px 25px; text-transform: capitalize; border-color: #3498db;\">", text, "</a> </td>
</tr>
</tbody>
</table>
</td>
</tr>
</tbody>
</table>
"
)
}
