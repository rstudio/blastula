#' Associate an email when publishing an R Markdown document to RStudio Connect
#'
#' This function is used to customize emails sent by RStudio Connect in
#' conjunction with publishing an R Markdown document. It associates a custom
#' email message with the main R Markdown document, which Connect can send to
#' selected recipients. The main input is a rendered email message, which can be
#' produced by either the [render_email()] or [render_connect_email()] function.
#'
#' Since this function needs to be invoked within an R Markdown document, the
#' chunk option `echo=FALSE` is useful here (so that viewers of the rendered
#' document don't have to unnecessarily read code related to email inclusion).
#' While the output is invisible, any errors related to rendering will be
#' visible to the author.
#'
#' @param email A rendered email message. Normally, we'd want to use an
#'   associated .Rmd file with the `blastula::blastula_email` R Markdown output
#'   format in [render_connect_email()] call (where its `input` is the email
#'   .Rmd file).
#' @param subject An option to specify the the email subject while attaching the
#'   email object.
#' @param attachments A vector of attachments for the Connect email. These files
#'   can be any of those deployed when publishing to RStudio Connect, and, any
#'   generated files (via R Markdown rendering).
#' @param attach_output Should the rendered output of the main R Markdown
#'   document be included as an email attachment? By default, this is `FALSE`.
#'   If `TRUE` the main R Markdown document will be attached first (before any
#'   files specified in `attachments`) and the filename will be preserved.
#' @param text Instead of using a rendered email document through the `email`
#'   option, we can use plain text here. If any text is provided here, input to
#'   `email` is ignored.
#' @param preview Should the email message display it's own preview window? If
#'   `TRUE` (the default), the rendered email message will be shown in the
#'   default browser.
#'
#' @export
attach_connect_email <- function(email = NULL,
                                 subject = NULL,
                                 attachments = NULL,
                                 attach_output = FALSE,
                                 text = NULL,
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

      if (isTRUE(preview)) {

        html_file <- ".rsc_email.html"
        html <- email$html_html

        msg <- create_rmd_preview_message(subject = subject)

        html <-
          sub(
            "(<body(?!\\w)[^>]*>)", paste0("\\1", msg),
            html, perl = TRUE, ignore.case = TRUE
          )

        writeLines(text = html, con = html_file)
        utils::browseURL(url = html_file)

        # This sleep is necessary because knitting usually happens in a separate
        # process, and when that process terminates the temp file will be deleted
        #Sys.sleep(5)
      }
    }

    # Set the RStudio Connect output metadata options for the email message
    # body and for the images therein
    rmarkdown::output_metadata$set(rsc_email_body_html = email$html_str)
    rmarkdown::output_metadata$set(rsc_email_images = email$images)
  }

  if (!is.null(text)) {

    text <- paste(text, collapse = "\n")

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

#' Suppress any scheduled emailing in RStudio Connect
#'
#' This function is useful for suppressing the scheduled emailing of a published
#' R Markdown document. It can be invoked anywhere in the R Markdown document
#' and is useful in a conditional statement, where the result of the condition
#' determines whether or not email suppression should occur.
#'
#' Since this function needs to be invoked within an R Markdown document, the
#' chunk option `echo=FALSE` is useful here (so that viewers of the rendered
#' document don't have to unnecessarily read code related to email suppression).
#' While the output is invisible, any errors related to the use of this function
#' will be visible to the author.
#'
#' @param suppress A logical value for whether email suppression should occur
#'   after publication. By default, this is `TRUE`.
#' @export
suppress_scheduled_email <- function(suppress = TRUE) {

  rmarkdown::output_metadata$set(rsc_email_suppress_scheduled = suppress)
}

#' Utility function to generate the preview message for a Connect Email
#'
#' @noRd
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
