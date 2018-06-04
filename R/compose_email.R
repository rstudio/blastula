#' Create the email message
#'
#' Create an email message. String
#' interpolation is possible for the text
#' comprising the email body, footer, and
#' preheader text. This is done by using
#' curly braces to enclose R code chunks.
#' Variables can be specified in the function
#' call (using named arguments with \code{...}),
#' and any variables not found in \code{...}
#' will be searched for in the global
#' environment.
#' @param body the main body of text for
#' the email message. Markdown can be used here
#' (along with string interpolation via curly
#' braces and named arguments) to construct the
#' main text.
#' @param footer the footer text for the email
#' message. As with the \code{body}, Markdown
#' and string interpolation can be used here.
#' @param .preheader_text text that appears
#' before the subject in some email clients.
#' This must be plaintext.
#' @param .title the title of the email message.
#' This is not the subject but the HTML title
#' text which may appear in limited
#' circumstances.
#' @param ... expression strings for string
#' interpolation for the \code{body},
#' \code{footer}, and \code{preheader_text}
#' string data.
#' @param .envir allows for setting the
#' environment.
#' @return an \code{email_message} object,
#' which can be used for previewing with
#' the \code{preview_email()} function or
#' for sending out actual emails with the
#' \code{send_email_out()} function.
#' @examples
#' # Create a simple email message using
#' # Markdown formatting
#' email <-
#'   compose_email(
#'   body = "
#'   Hello!
#'
#'   ## This a section heading
#'
#'   We can use Markdown formatting \\
#'   to **embolden** text or to add \\
#'   *emphasis*. This is exciting, \\
#'   right?
#'
#'   Cheers")
#'
#' # The email message can always be
#' # previewed using `preview_email()`
#' preview_email(email = email)
#'
#' # We can use string interpolation to
#' # add in R code or strings assigned
#' # to variables; variables can be
#' # obtained from the global workspace
#' # or from temporary variables in the
#' # function call
#' sender_name <- "Mike"
#'
#' email <-
#'   compose_email(
#'   body = "
#'   Hello!
#'
#'   I just wanted to let you \\
#'   know that the {thing} that \\
#'   asked me for is ready to \\
#'   pick up. So, come over and \\
#'   do that.
#'
#'   Cheers,
#'
#'   {sender_name}",
#'   thing = "report")
#' @importFrom glue glue
#' @importFrom commonmark markdown_html
#' @importFrom stringr str_replace_all
#' @importFrom htmltools HTML
#' @export
compose_email <- function(body = NULL,
                          footer = NULL,
                          .preheader_text = NULL,
                          .title = NULL,
                          ...,
                          .envir = new.env()) {

  if (!is.null(.preheader_text)) {
    preheader_text <-
      glue::glue(.preheader_text, ...)
  } else {
    preheader_text <- ""
  }

  if (!is.null(.preheader_text)) {
    title_text <-
      glue::glue(.title, ...)
  } else {
    title_text <- ""
  }

  if (!is.null(body)) {
    body_text <-
      glue::glue(body, ...)
  } else {
    body_text <- ""
  }

  if (!is.null(footer)) {
    footer_text <-
      glue::glue(footer, ...)
  } else {
    footer_text <- ""
  }

  html_preheader_text <-
    stringr::str_replace_all(
      commonmark::markdown_html(preheader_text), "\n", "")

  html_body_text <-
    stringr::str_replace_all(
      commonmark::markdown_html(body_text), "\n", "")

  html_footer_text <-
    stringr::str_replace_all(
      commonmark::markdown_html(footer_text), "\n", "")

# Generate the email message body
body <-
  glue(
  "
<!doctype html>
  <html>
  <head>
  <meta name=\"viewport\" content=\"width=device-width\">
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
  <title>{title_text}</title>
  <style>
  @media only screen and (max-width: 620px) {{
  table[class=body] h1 {{
  font-size: 28px !important;
  margin-bottom: 10px !important;
  }}
  table[class=body] p,
  table[class=body] ul,
  table[class=body] ol,
  table[class=body] td,
  table[class=body] span,
  table[class=body] a {{
  font-size: 16px !important;
  }}
  table[class=body] .wrapper,
  table[class=body] .article {{
  padding: 10px !important;
  }}
  table[class=body] .content {{
  padding: 0 !important;
  }}
  table[class=body] .container {{
  padding: 0 !important;
  width: 100% !important;
  }}
  table[class=body] .main {{
  border-left-width: 0 !important;
  border-radius: 0 !important;
  border-right-width: 0 !important;
  }}
  table[class=body] .btn table {{
  width: 100% !important;
  }}
  table[class=body] .btn a {{
  width: 100% !important;
  }}
  table[class=body] .img-responsive {{
  height: auto !important;
  max-width: 100% !important;
  width: auto !important;
  }}
  }}

  /* -------------------------------------
  PRESERVE THESE STYLES IN THE HEAD
  ------------------------------------- */
  @media all {{
  .ExternalClass {{
  width: 100%;
  }}
  .ExternalClass,
  .ExternalClass p,
  .ExternalClass span,
  .ExternalClass font,
  .ExternalClass td,
  .ExternalClass div {{
  line-height: 100%;
  }}
  .apple-link a {{
  color: inherit !important;
  font-family: inherit !important;
  font-size: inherit !important;
  font-weight: inherit !important;
  line-height: inherit !important;
  text-decoration: none !important;
  }}
  .btn-primary table td:hover {{
  background-color: #34495e !important;
  }}
  .btn-primary a:hover {{
  background-color: #34495e !important;
  border-color: #34495e !important;
  }}
  }}
  </style>
  </head>
  <body class=\"\" style=\"background-color: #f6f6f6; font-family: sans-serif; -webkit-font-smoothing: antialiased; font-size: 14px; line-height: 1.4; margin: 0; padding: 0; -ms-text-size-adjust: 100%; -webkit-text-size-adjust: 100%;\">
  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"body\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; background-color: #f6f6f6;\">
  <tr>
  <td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top;\">&nbsp;</td>
  <td class=\"container\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; display: block; Margin: 0 auto; max-width: 580px; padding: 10px; width: 580px;\">
  <div class=\"content\" style=\"box-sizing: border-box; display: block; Margin: 0 auto; max-width: 580px; padding: 10px;\">

  <!-- START CENTERED WHITE CONTAINER -->
  <span class=\"preheader\" style=\"color: transparent; display: none; height: 0; max-height: 0; max-width: 0; opacity: 0; overflow: hidden; mso-hide: all; visibility: hidden; width: 0;\">{preheader_text}</span>
  <table class=\"main\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; background: #ffffff; border-radius: 3px;\">

  <!-- START MAIN CONTENT AREA -->
  <tr>
  <td class=\"wrapper\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 20px;\">
  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\">
  <tr>
  <td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top;\">
  <p style=\"font-family: sans-serif; font-size: 14px; font-weight: normal; margin: 0; Margin-bottom: 15px;\">{html_body_text}</p>
  </td>
  </tr>
  </table>
  </td>
  </tr>

  <!-- END MAIN CONTENT AREA -->
  </table>

  <!-- START FOOTER -->
  <div class=\"footer\" style=\"clear: both; Margin-top: 10px; text-align: center; width: 100%;\">
  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\">
  <tr>
  <td class=\"content-block powered-by\" style=\"font-family: sans-serif; vertical-align: top; padding-bottom: 10px; padding-top: 10px; font-size: 12px; color: #999999; text-align: center;\">
  {html_footer_text}
  </td>
  </tr>
  </table>
  </div>
  <!-- END FOOTER -->

  <!-- END CENTERED WHITE CONTAINER -->
  </div>
  </td>
  <td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top;\">&nbsp;</td>
  </tr>
  </table>
  </body>
  </html>"
  )

  email_message <-
    list(
      html_str = as.character(body),
      html_html = body %>% htmltools::HTML())

  attr(email_message, "class") <- "email_message"

  email_message
}

