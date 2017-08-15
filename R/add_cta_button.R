#' Helper function for adding a CTA button
#' @description Add a call to action button
#' inside the body of the email with this
#' helper function. There are options to
#' specify the button text, the URL, and
#' the button's alignment.
#' @param url a URL for the button.
#' @param text the text that is placed atop
#' the CTA button.
#' @param align the alignment of the button
#' inside the main content area. Options are
#' \code{left} (the default), \code{right},
#' and \code{center}.
#' @return a character object with HTML code
#' that can be placed into the message body
#' wherever the CTA button should appear.
#' @examples
#' # Create the button as an HTML fragment
#' cta_button <-
#'   add_cta_button(
#'     url = "http://www.website.net",
#'     text = "Press This Button",
#'     align = "center")
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
#' @export add_cta_button

add_cta_button <- function(url,
                           text,
                           align = "left") {

  glue::glue(
    "
    <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"btn btn-primary\">
    <tbody>
    <tr>
    <td align=\"{align}\">
    <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\">
    <tbody>
    <tr>
    <td> <a href=\"{url}\" target=\"_blank\">{text}</a> </td>
    </tr>
    </tbody>
    </table>
    </td>
    </tr>
    </tbody>
    </table>
    ")
}
