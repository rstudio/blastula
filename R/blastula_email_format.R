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
                           connect_footer = FALSE,
                           ...) {

  if (template == "blastula") {
    template <- system.file("rmd", "template.html", package = "blastula")
  }

  pandoc_args <- NULL
  if (connect_footer) {
    pandoc_args <- c(
      pandoc_args,
      "--variable=rsc-footer:1",
      # Note bash escaping is handled by R Markdown (via shQuote())
      paste0(
        "--variable=rsc-date-time:", add_readable_time(time = Sys.time())),
      paste0(
        "--variable=rsc-report-url:",
        Sys.getenv("RSC_REPORT_URL", unset = "https://connect.example.com/content/1234/")),
      paste0(
        "--variable=rsc-report-rendering-url:",
        Sys.getenv("RSC_REPORT_RENDERING_URL", unset = "https://connect.example.com/content/1234/_rev5678")),
      paste0(
        "--variable=rsc-report-subscription-url:",
        Sys.getenv("RSC_REPORT_SUBSCRIPTION_URL", unset = "")),
      paste0(
        "--variable=rsc-report-name:",
        Sys.getenv("RSC_REPORT_NAME", unset = ""))
    )
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
    pandoc_args = pandoc_args,
    ...
  )
}

bash_escape <- function(x) {
  gsub("([\" \\;])", "\\\\\\1", x)
}
