#' R Markdown render function for the `blastula_email` output format
#'
#' @param input The input file to be rendered. This should be an R Markdown
#'   document (.rmd) with the output specified as `output:
#'   blastula::blastula_email`.
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting (we can use `new.env()` to guarantee an empty, new
#'   environment).
#' @param quiet An option to suppress printing of the pandoc command line. By
#'   default, this is set to `TRUE`.
#'
#' @export
render_email <- function(input,
                         envir = parent.frame(),
                         quiet = TRUE) {

  output_file <- tempfile(pattern = "email", fileext = ".html")
  on.exit(unlink(output_file))

  rmarkdown::render(
    input = input,
    output_file = output_file,
    envir = envir,
    output_options = list(self_contained = TRUE),
    quiet = quiet
  )

  email_obj <- cid_images(output_file)
  email_obj
}

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
                          attachments = report_rmd()) {

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

  rsc_report_name <- Sys.getenv("RSC_REPORT_NAME")
  rsc_report_url <- Sys.getenv("RSC_REPORT_URL")

  print(rsc_report_name)
  print(rsc_report_url)

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

#' The R Markdown `blastula_email` output format
#'
#' @param toc If you would like an automatically-generated table of contents in
#'   the output email, choose `TRUE`. By default, this is `FALSE` where no table
#'   of contents will be generated.
#' @param toc_depth The depth of headers to include in the table of contents (
#'   should `toc` be set to `TRUE`).
#' @param toc_float An option to float the table of contents to the left of the
#'   main document content. By default, this is `FALSE`.
#' @param number_sections Sections can be sequentially numbered if this is set
#'   to `TRUE`. By default, this is `FALSE`.
#' @param section_divs This wraps sections in `<section>` tags and attaches
#'   identifiers to the enclosing `<section>`s. This is set to `TRUE`.
#' @param fig_width,fig_height The figure width and height in units of inches.
#' @param fig_retina The scaling factor for retina displays. The default value
#'   is `2`, which is the preferred choice for most retina displays. This can be
#'   set to `NULL` to prevent retina scaling. Note that this will always be
#'   `NULL` if `keep_md` is set to `TRUE`.
#' @param fig_caption An option to render figures with captions. By default,
#'   this is set to `TRUE`.
#' @param dev The R graphics device for figures. By default, this is the `png`
#'   device.
#' @param smart An option to produce typographically correct output. This will
#'   convert straight quotes to curly quotes, `---` to em dashes, `--` to en
#'   dashes, and instances of `...` to ellipses. By default, this is `TRUE`.
#' @param self_contained Should a self-contained output file be generated. By
#'   default, this is `TRUE`. The standalone HTML file will have no external
#'   dependencies, it will use URIs to incorporate the contents of linked
#'   scripts, stylesheets, images, and videos.
#' @param template The Pandoc template to use for rendering. This is the
#'   `"blastula"` template by default.
#' @param includes A named list of additional content to include within the
#'   document. This is typically created using the [rmarkdown::includes()]
#'   function. By default, this is set to `NULL`.
#' @param keep_md Should you need the keep the intermediate Markdown (.md) file,
#'   set this to `TRUE`. By default, the .md file is not kept.
#' @param md_extensions Markdown extensions to be added or removed from the
#'   default definition or R Markdown.
#' @param ... Specify other options in [rmarkdown::html_document()].
#'
#' @export
blastula_email <- function(toc = FALSE,
                           toc_depth = 3,
                           toc_float = FALSE,
                           number_sections = FALSE,
                           section_divs = TRUE,
                           fig_width = 5.35,
                           fig_height = 5,
                           fig_retina = 2,
                           fig_caption = TRUE,
                           dev = "png",
                           smart = TRUE,
                           self_contained = TRUE,
                           template = "blastula",
                           includes = NULL,
                           keep_md = FALSE,
                           md_extensions = NULL,
                           ...) {

  if (template == "blastula") {
    template <- system.file("rmd", "template.html", package = "blastula")
  }

  rmarkdown::html_document(
    toc = toc,
    toc_depth = toc_depth,
    toc_float = toc_float,
    number_sections = number_sections,
    section_divs = section_divs,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_retina = fig_retina,
    fig_caption = fig_caption,
    dev = dev,
    df_print = "default",
    code_folding = "none",
    code_download = FALSE,
    smart = smart,
    self_contained = self_contained,
    theme = "default",
    highlight = "default",
    mathjax = NULL,
    template = template,
    extra_dependencies = NULL,
    css = NULL,
    includes = includes,
    keep_md = keep_md,
    lib_dir = NULL,
    md_extensions = md_extensions,
    pandoc_args = NULL,
    ...
  )
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

#' @export
report_rmd <- function() {

  structure(
    class = c("connect_report"),
    list(
      name = "connect_report"
    )
  )
}

#' @export
generated_files <- function(files = NULL) {

  structure(
    class = c("generated_files"),
    list(
      name = files
    )
  )
}

#' @export
connect_files <- function(files = NULL) {

  structure(
    class = c("connect_files"),
    list(
      name = files
    )
  )
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
