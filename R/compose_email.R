#' Create the email message body
#'
#' The `compose_email()` function allows us to easily create an email message
#' body. We can use `glue`'s string interpolation semantics to incorporate
#' external objects or evaluate R code within the message body, the footer, and
#' the preheader text (using curly braces to enclose such R expressions). Local
#' variables can be specified in the function call (using named arguments with
#' `...`) and any variables not found in `...` will be searched for in the
#' global environment.
#' @param body The main body of text for the email message. Markdown can be used
#'   here (along with string interpolation via curly braces and named arguments)
#'   to construct the main text.
#' @param header,footer The header and footer text for the email message, As
#'   with the `body`, Markdown and string interpolation can be used here.
#' @param blocks A list of `block_*()` calls that enable the insertion of
#'   precomposed HTML blocks.
#' @param .preheader_text The text that appears before the subject in some email
#'   clients. This must be plaintext.
#' @param .title The title of the email message. This is not the subject but the
#'   HTML title text which may appear in limited circumstances.
#' @param .envir An opportunity to specify the environment. By default, this is
#'   the `parent.frame()`.
#' @param ... Expression strings for string interpolation for the `body`,
#'   `footer`, and `preheader_text` string data.
#' @return An `email_message` object.
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
#' @importFrom stringr str_replace str_replace_all str_detect
#' @importFrom stringr str_extract str_extract_all
#' @importFrom htmltools HTML
#' @export
compose_email <- function(body = NULL,
                          header = NULL,
                          footer = NULL,
                          blocks = NULL,
                          .preheader_text = NULL,
                          .title = NULL,
                          .envir = parent.frame(),
                          ...) {

  if (!is.null(.preheader_text)) {
    preheader_text <-
      glue::glue(.preheader_text, ..., .envir = .envir)
  } else {
    preheader_text <- ""
  }

  if (!is.null(.preheader_text)) {
    title_text <-
      glue::glue(.title, ..., .envir = .envir)
  } else {
    title_text <- ""
  }

  if (!is.null(body)) {
    body_text <-
      glue::glue(body, ..., .envir = .envir)
  } else {
    body_text <- ""
  }

  if (!is.null(header)) {
    header <-
      glue::glue(header, ..., .envir = .envir)
  } else {
    header <- ""
  }

  if (!is.null(footer)) {
    footer_text <-
      glue::glue(footer, ..., .envir = .envir)
  } else {
    footer_text <- ""
  }

  if (!is.null(blocks)) {

    if (inherits(blocks, "list")) {
      blocks <- paste(unlist(blocks), collapse = "\n")
    }

    blocks <-
      glue::glue(blocks, ..., .envir = .envir)

  } else {
    blocks <- ""
  }

  html_preheader_text <-
    tidy_gsub(
      commonmark::markdown_html(preheader_text), "\n", "")

  html_body_text <-
    tidy_gsub(
      commonmark::markdown_html(body_text), "\n", "")

  html_header <-
    tidy_gsub(
      commonmark::markdown_html(header), "\n", "")

  html_footer <-
    tidy_gsub(
      commonmark::markdown_html(footer_text), "\n", "")

  # Generate the email message body
  body <-
    glue::glue(bls_standard_template())

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

  # Apply the `email_message` class
  attr(email_message, "class") <- "email_message"

  email_message
}
