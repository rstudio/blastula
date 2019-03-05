#' Create an HTML fragment for a CTA button
#'
#' Add a call to action button inside the body of the email with this helper
#' function. There are options to specify the button text, the URL, and the
#' button's alignment.
#' @param url A URL for the button.
#' @param text The text that is placed atop the CTA button.
#' @param align The alignment of the button inside the main content area.
#'   Options are `center` (the default), `left`, and `right`.
#' @return a character object with an HTML fragment that can be placed inside
#'   the message body wherever the CTA button should appear.
#' @examples
#' # Create the button as an HTML fragment
#' cta_button <-
#'   add_cta_button(
#'     url = "http://www.website.net",
#'     text = "Press This Button")
#'
#' # Include the button in the email
#' # message body by simply referencing
#' # the `cta_button` object
#' email <-
#'   compose_email(
#'     body = "
#'     Hello!
#'
#'     Below is a call. It's a call \\
#'     to action. Press it!
#'
#'     {cta_button}
#'
#'     Cheers
#'     ")
#' @importFrom glue glue
#' @export
add_cta_button <- function(url,
                           text,
                           align = "center") {

  glue::glue(
    "
  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"btn btn-primary\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; box-sizing: border-box;\">
  <tbody>
  <tr>
  <td align=\"{align}\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 15px;\">
  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: auto;\">
  <tbody>
  <tr>
  <td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; background-color: #3498db; border-radius: 5px; text-align: center;\"> <a href=\"{url}\" target=\"_blank\" style=\"display: inline-block; color: #ffffff; background-color: #3498db; border: solid 1px #3498db; border-radius: 5px; box-sizing: border-box; cursor: pointer; text-decoration: none; font-size: 14px; font-weight: bold; margin: 0; padding: 12px 25px; text-transform: capitalize; border-color: #3498db;\">{text}</a> </td>
  </tr>
  </tbody>
  </table>
  </td>
  </tr>
  </tbody>
  </table>
 ")

}
