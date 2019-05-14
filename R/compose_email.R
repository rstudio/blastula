#' Create the email message body
#'
#' The `compose_email()` function allows us to easily create an email message.
#' We can use `glue`'s string interpolation semantics to incorporate external
#' objects or evaluate R code within the message body, the footer, and the
#' preheader text (using curly braces to enclose such R expressions). Local
#' variables can be specified in the function call (using named arguments with
#' `...`) and any variables not found in `...` will be searched for in the
#' global environment.
#' @param header,body,footer The three layout sections for an email message
#'   (ordered from top to bottom). Markdown text can be supplied to each of
#'   these. String interpolation is enabled via curly braces and named
#'   arguments in `...`. Alternatively, we can supply a set of `block_*()` calls
#'   enclosed within the [blocks()] function to take advantage of precomposed
#'   HTML blocks.
#' @param .title The title of the email message. This is not the subject but the
#'   HTML title text which may appear in limited circumstances.
#' @param .envir An opportunity to specify the environment. By default, this is
#'   the [parent.frame()].
#' @param ... Expression strings for string interpolation within the `header`,
#'   `body` and `footer`.
#' @return An `email_message` object.
#' @examples
#' # Create a simple email message using
#' # Markdown-formatted text in `body`
#' email <-
#'   compose_email(
#'   body = "
#'   Hello!
#'
#'   ## This a section heading
#'
#'   We can use Markdown formatting \\
#'   to **embolden** text or to add \\
#'   *emphasis*.
#'
#'   Cheers")
#'
#' # The email message can always be
#' # previewed by calling the object
#' email
#'
#' # We can use string interpolation to
#' # add in R code or strings assigned
#' # to variables; variables can be
#' # obtained from the global workspace
#' # or from temporary variables in the
#' # function call
#' sender_name <- "Shelly"
#'
#' email <-
#'   compose_email(
#'   body = "
#' Hello!
#'
#' I just wanted to let you \\
#' know that the {thing} that \\
#' asked me for is ready to \\
#' pick up. So, come over and \\
#' do that.
#'
#' Cheers,
#'
#' {sender_name}",
#'   thing = "report"
#'   )
#' @importFrom glue glue
#' @importFrom commonmark markdown_html
#' @importFrom stringr str_replace str_replace_all str_detect
#' @importFrom stringr str_extract str_extract_all
#' @importFrom htmltools HTML
#' @export
compose_email <- function(body = NULL,
                          header = NULL,
                          footer = NULL,
                          .title = NULL,
                          .envir = parent.frame(),
                          ...) {

  # Define the title text for the email; use an
  # empty string if not supplied
  if (!is.null(.title)) {
    title_text <-
      glue::glue(.title, ..., .envir = .envir)
  } else {
    title_text <- ""
  }

  # Define the email body section
  if (!is.null(body)) {

    if (inherits(body, "blocks")) {

      body <- render_blocks(blocks = body, context = "body")
      html_body_text <- paste(unlist(body), collapse = "\n")

    } else {

      body_text <-
        glue::glue(body, ..., .envir = .envir)

      html_body_text <-
        body_text %>%
        commonmark::markdown_html() %>%
        tidy_gsub("\n", "")

      html_body_text <-
        glue::glue(
          simple_body_block(),
          html_paragraphs = html_body_text
        )
    }

  } else {
    html_body_text <- ""
  }

  # Define the email footer section
  if (!is.null(footer)) {

    if (inherits(footer, "blocks")) {

      footer <- render_blocks(blocks = footer, context = "footer")
      html_footer <- paste(unlist(footer), collapse = "\n")

    } else {

      footer <-
        glue::glue(footer, ..., .envir = .envir)

      html_footer <-
        footer %>%
        commonmark::markdown_html() %>%
        tidy_gsub("\n", "")

      html_footer <-
        render_blocks(
          blocks =
            blocks(
              block_text(html_footer)),
          context = "footer"
        )[[1]]
    }

  } else {
    html_footer <- ""
  }

  # Define the email header section
  if (!is.null(header)) {

    if (inherits(header, "blocks")) {

      header <- render_blocks(blocks = header, context = "header")
      html_header <- paste(unlist(header), collapse = "\n")

    } else {

      header <-
        glue::glue(header, ..., .envir = .envir)

      html_header <-
        header %>%
        commonmark::markdown_html() %>%
        tidy_gsub("\n", "")

      html_header <-
        render_blocks(
          blocks =
            blocks(
              block_text(html_header)),
          context = "header"
        )[[1]]
    }

  } else {
    html_header <- ""
  }

  # Generate the email message body
  body <- glue::glue(bls_standard_template())

  # Add the HTML bodies (two variants) to the
  # `email_message` object
  email_message <-
    list(
      html_str = body %>% as.character(),
      html_html = body %>% htmltools::HTML(),
      attachments = list()
    )

  if (email_message$html_str %>%
      stringr::str_detect("<img cid=.*? src=\"data:image/(png|jpeg);base64,.*?\"")) {

    # Extract encoded images from body
    extracted_images <-
      email_message$html_str %>%
      stringr::str_extract_all(
        "<img cid=.*? src=\"data:image/(png|jpeg);base64,.*?\"") %>%
      unlist()

    # Obtain a vector of CIDs
    cid_names <- c()
    for (i in seq(extracted_images)) {

      cid_name <-
        extracted_images[i] %>%
        stringr::str_extract("cid=\".*?\"") %>%
        stringr::str_replace_all("(cid=\"|\")", "")

      cid_names <- c(cid_names, cid_name)
    }

    # Clean the Base64 image strings
    for (i in seq(extracted_images)) {
      extracted_images[i] <-
        gsub(
          ".{1}$", "",
          extracted_images[i] %>%
            stringr::str_replace(
              "<img cid=.*? src=\"data:image/(png|jpeg);base64,", "")
        )
    }

    # Create a list with a base64 image per list element
    extracted_images <- as.list(extracted_images)

    # Apply `cid_names` to the `extracted_images` list
    names(extracted_images) <- cid_names

    # Add the list of extracted images to the
    # `email_message` list object
    email_message <-
      c(email_message, list(images = extracted_images))

    # Replace `<img>...</img>` tags with CID values
    for (i in seq(extracted_images)) {
      email_message$html_str <-
        email_message$html_str %>%
        stringr::str_replace(
          pattern = "<img cid=.*? src=\"data:image/(png|jpeg);base64,.*?\"",
          replacement = paste0("<img src=\"cid:", cid_names[i], "\"")
        )
    }
  }

  # Apply the `email_message` and `blastula_message` classes
  attr(email_message, "class") <- c("blastula_message", "email_message")

  email_message
}

#' Template for a simple block of HTML in the body
#' @noRd
simple_body_block <- function() {

"<tr>
<td class=\"wrapper\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 20px;\">
<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\">
<tbody>
<tr>
<td style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\">
<p style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px;\">{html_paragraphs}</p>
</td>
</tr>
</tbody>
</table>
</td>
</tr>"
}
