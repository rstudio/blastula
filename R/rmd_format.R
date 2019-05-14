#' @export
render_email <- function(input_file, envir = parent.frame(), quiet = TRUE) {

  outfile <- tempfile(pattern = "email", fileext = ".html")
  on.exit(unlink(outfile))

  rmarkdown::render(
    input_file,
    output_file = outfile,
    envir = envir,
    output_options = list(self_contained = TRUE),
    quiet = quiet
  )

  email_obj <- cid_images(outfile)
  email_obj
}

#' Attach a Blastula email message object to the current render
#'
#' @export
attach_email <- function(message, preview = TRUE) {
  if (!inherits(message, "email_message")) {
    stop("blastula::attach_email() requires a blastula email message object")
  }

  if (is.na(Sys.getenv("RSC_REPORT_NAME", unset = NA))) {

    # warning("attach_email has no effect outside of RStudio Connect")

    if (preview) {

      html_file <- tempfile(fileext = ".html")
      html <- message$html_html

      msg <-
        paste0(
          "<div style=\"text-align: center; background:#fcfcfc\">\n",
          "<h2 style=\"margin-bottom: 0; padding-bottom: 0;\">",
          "This is an email preview</h2>\n",
          "<p style=\"text-align: center; background:#fcfcfc; ",
          "padding-top: 0; margin-top: 0;\">",
          "Use <code>attach_email(preview = FALSE)</code> ",
          "to attach without this preview.</p>\n",
          "</div>\n",
          "<hr>\n"
        )

      html <-
        sub(
          "(<body(?!\\w)[^>]*>)", paste0("\\1", msg),
          html, perl = TRUE, ignore.case = TRUE
        )

      writeLines(html, html_file)
      browseURL(html_file)

      # This sleep is necessary because knitting usually happens in a separate
      # process, and when that process terminates the temp file will be deleted
      Sys.sleep(5)
    }
  }

  rmarkdown::output_metadata$set(rsc_email_body_html = message$html_str)
  rmarkdown::output_metadata$set(rsc_email_images = message$images)

  invisible()
}

#' R Markdown output format for Blastula emails
#'
#' @export
blastula_email <- function(toc = FALSE, toc_depth = 3, toc_float = FALSE,
  number_sections = FALSE, section_divs = TRUE, fig_width = 7,
  fig_height = 5, fig_retina = 2, fig_caption = TRUE, dev = "png",
  df_print = "default", code_folding = c("none", "show", "hide"),
  code_download = FALSE, smart = TRUE, self_contained = TRUE,
  theme = "default", highlight = "default", mathjax = NULL,
  template = "blastula", extra_dependencies = NULL, css = NULL,
  includes = NULL, keep_md = FALSE, lib_dir = NULL,
  md_extensions = NULL, pandoc_args = NULL, ...) {

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
    df_print = df_print,
    code_folding = code_folding,
    code_download = code_download,
    smart = smart,
    self_contained = self_contained,
    theme = theme,
    highlight = highlight,
    mathjax = mathjax,
    template = template,
    extra_dependencies = extra_dependencies,
    css = css,
    includes = includes,
    keep_md = keep_md,
    lib_dir = lib_dir,
    md_extensions = md_extensions,
    pandoc_args = pandoc_args,
    ...
  )
}
