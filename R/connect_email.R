#' Include a email message when publishing R Markdown documents in Connect
#'
#' @param email A rendered blastula email. Normally, we'd want to use an
#'   associated .Rmd file with the `blastula::blastula_email` R Markdown output
#'   format in the following call: `blastula::render_email(input = <email_document>.Rmd)`.
#' @param text Text for the message body.
#' @param subject An option to specify the the email subject while attaching the
#'   email object.
#' @param attachments A vector of attachments for the Connect email.
#' @param attach_output Should the rendered output of the main R Markdown
#'   document be included as an email attachment. By default, this is `FALSE`.
#' @param preview Should the email message display it's own preview window? If
#'   `TRUE` (the default), the rendered email message will be shown in the
#'   default browser.
#'
#' @export
connect_email <- function(email = NULL,
                          text = NULL,
                          subject = NULL,
                          attachments = NULL,
                          attach_output = FALSE,
                          preview = TRUE) {

  if (!is.null(email) && !inherits(email, "email_message")) {
    stop("A blastula email message object must be supplied.",
         call. = FALSE)
  }

  if (is.null(email) && is.null(text)) {
    stop("Either a blastula email message object or `text` must be provided",
         call. = FALSE)
  }

  # If both an `email` object and `text` are provided, nullify
  # the `text` and give preference to the HTML email
  if (!is.null(email) && !is.null(text)) {
    text <- NULL
  }

  if (!is.null(email)) {

    if (is.na(Sys.getenv("RSC_REPORT_NAME", unset = NA))) {

      # warning("connect_email() has no effect outside of RStudio Connect")

      if (preview) {

        html_file <- tempfile(fileext = ".html")
        html <- email$html_html

        msg <- create_rmd_preview_message(subject = subject)

        html <-
          sub(
            "(<body(?!\\w)[^>]*>)", paste0("\\1", msg),
            html, perl = TRUE, ignore.case = TRUE
          )

        writeLines(html, html_file)
        utils::browseURL(html_file)

        # This sleep is necessary because knitting usually happens in a separate
        # process, and when that process terminates the temp file will be deleted
        Sys.sleep(5)
      }
    }

    # Set the RStudio Connect output metadata options for the email message
    # body and for the images therein
    rmarkdown::output_metadata$set(rsc_email_body_html = email$html_str)
    rmarkdown::output_metadata$set(rsc_email_images = email$images)
  }

  if (!is.null(text)) {
    rmarkdown::output_metadata$set(rsc_email_body_text = text)
  }

  # Set the email subject string if this is provided
  if (!is.null(subject)) {
    rmarkdown::output_metadata$set(rsc_email_subject = subject)
  }

  # Attach files via the `rsc_email_attachments` parameter
  if (!is.null(attachments)) {
    rmarkdown::output_metadata$set(rsc_email_attachments = attachments)
  }

  # Set the `rsc_email_suppress_report_attachment` parameter to
  # `TRUE` if we elect to exclude the default Connect attachment
  # of the rendered .Rmd
  if (isTRUE(attach_output)) {
    rmarkdown::output_metadata$set(rsc_email_suppress_report_attachment = FALSE)
  } else {
    rmarkdown::output_metadata$set(rsc_email_suppress_report_attachment = TRUE)
  }

  invisible()
}

create_rmd_preview_message <- function(subject = NULL) {

  if (!is.null(subject)) {
    subject_ln <-
      paste0(
        "<br><br><strong><span style=\"font-variant: small-caps;\">",
        "email subject: </span></strong>", subject, "<br>"
      )

  } else {
    subject_ln <- NULL
  }

  preview_msg <-
    paste0(
      "<div style=\"text-align: center; background:#fcfcfc\">",
      "<h2 style=\"margin-bottom: 0; padding-bottom: 0;\">",
      "This is an email preview for RStudio Connect</h2>",
      "<p style=\"text-align: center; background:#fcfcfc; ",
      "padding-top: 0; margin-top: 0;\">",
      "Use <code>connect_email(preview = FALSE)</code> ",
      "to attach without this preview.",
      subject_ln,
      "</p><hr></div>"
    )

  preview_msg
}

inherits_attachment_type <- function(attachments_list) {

  lapply(
    attachments_list, function(x) {
      inherits(x, "connect_report") ||
        inherits(x, "generated_files") ||
        inherits(x, "connect_files")
    }) %>%
    unlist() %>%
    all()
}

any_of_attachment_type <- function(attachments_list,
                                   attachment_type) {

  vapply(
    attachments_list,
    function(x) inherits(x, attachment_type),
    FUN.VALUE = logical(1)
  ) %>% any()
}

names_of_attachment_type <- function(attachments_list,
                                     attachment_type) {

  which_elements <-
    vapply(
      attachments_list,
      function(x) inherits(x, attachment_type),
      FUN.VALUE = logical(1)
    ) %>% which()

  attachments_list[which_elements] %>%
    unlist() %>%
    unname()
}
