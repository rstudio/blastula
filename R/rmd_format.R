#' Include a email message when publishing R Markdown documents in Connect
#'
#' @param email A rendered blastula email. Normally, we'd want to use an
#'   associated .Rmd file with the `blastula::blastula_email` R Markdown output
#'   format in the following call: `blastula::render_email(input = <email_document>.Rmd)`.
#' @param subject An option to specify the the email subject while attaching the
#'   email object.
#' @param preview Should the email message display it's own preview window? If
#'   `TRUE` (the default), the rendered email message will be shown.
#' @param attachments A list of attachments for the Connect email.
#'
#' @export
connect_email <- function(email,
                          subject = NULL,
                          preview = TRUE,
                          attachments = NULL) {

  # Create a list of names for any attachments that are either
  # generated from any rendered R Markdown documents or already
  # available in Connect; also supply a logical value for
  # whether the default attachment (the main .Rmd render)
  # should be suppressed
  attachment_namelist <-
    list(
      suppress_report_attachment = TRUE,
      generated_files = character(0),
      connect_files = character(0)
    )

  # If `report_rmd()` is by itself, set the `FALSE` value
  # in `attachment_namelist`
  if (length(attachments) == 1 &&
      inherits(attachments, "connect_report")) {
    attachment_namelist$suppress_report_attachment <- FALSE
  }

  # Get a vector of attachments for `rsc_email_attachments` and a logical
  # value for `rsc_email_suppress_report_attachment`
  if (is.list(attachments) &&
      all((lapply(attachments, FUN = length) %>% unlist()) == 1) &&
      inherits_attachment_type(attachments_list = attachments)) {

    if (attachments %>% any_of_attachment_type("connect_report")) {
      attachment_namelist$suppress_report_attachment <- FALSE
    } else {
      attachment_namelist$suppress_report_attachment <- TRUE
    }

    if (attachments %>% any_of_attachment_type("generated_files")) {

      attachment_namelist$generated_files <-
        attachments %>% names_of_attachment_type("generated_files")
    }

    if (attachments %>% any_of_attachment_type("connect_files")) {

      attachment_namelist$connect_files <-
        attachments %>% names_of_attachment_type("connect_files")
    }
  }

  if (!inherits(email, "email_message")) {
    stop("blastula::connect_email() requires a blastula email message object")
  }

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

  # Set the email subject string if this is provided; this
  # option eliminates the need to use
  # ---
  # rmd_output_metadata:
  #   rsc_email_subject: <subject>
  # ---
  # in the YAML front matter
  if (!is.null(subject)) {
    rmarkdown::output_metadata$set(rsc_email_subject = subject)
  }

  # Set the `rsc_email_suppress_report_attachment` parameter to
  # `TRUE` if we elect to exclude the default Connect attachment
  # of the rendered .Rmd
  if (attachment_namelist$suppress_report_attachment) {
    rmarkdown::output_metadata$set(rsc_email_suppress_report_attachment = TRUE)
  }

  # Attach files via the `rsc_email_attachments` parameter
  if (length(attachment_namelist$generated_files) > 0 |
      length(attachment_namelist$connect_files) > 0) {

    attached_files <-
      c(
        attachment_namelist$generated_files %>% unlist(),
        attachment_namelist$connect_files %>% unlist()
      )

    rmarkdown::output_metadata$set(rsc_email_attachments = attached_files)
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
