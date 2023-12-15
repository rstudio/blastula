# This function, to be used externally by scripts, will determine if
# the blastula package is available in the user system
blastula_pkg_available <- function() {
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop(
      "The `blastula` package is required for processing email ",
      "from Quarto documents."
    )
  }
}

jsonlite_pkg_available <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "The `jsonlite` package is required for processing email ",
      "from Quarto documents."
    )
  }
}

rmarkdown_pkg_available <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "The `rmarkdown` package is required for processing email ",
      "from Quarto documents."
    )
  }
}

# Combine `path` with `filename` and normalize the path
blastula_resource_filename <- function(path, filename) {

  if (is.null(path)) {
    path <- "."
  }

  as.character(
    fs::path_expand(
      fs::path_abs(
        path = filename,
        start = path
      )
    )
  )
}

get_quarto_report_render_html_path <- function() {
  system.file(
    "quarto_example_documents/quarto-report-render.html",
    package = "blastula"
  )
}

# Gets the HTML elements from CSS selector values
get_html_elements <- function(html, selector) {
  rvest::html_elements(html, css = selector)
}

detect_quarto_connect_json_file <- function(
    filename = "connect-email.json",
    path = NULL
) {

  filename <- blastula_resource_filename(path = path, filename = filename)

  if (!file.exists(filename)) {
    warning("The JSON file required for Connect emailing cannot be found.")
    return(invisible())
  }
}

get_html_email_fragment <- function(
    file,
    selector = "[class='email']"
) {

  html_file_lines <- readLines(con = file, warn = FALSE)
  html_file <- paste(html_file_lines, collapse = "\n")
  html_read <- xml2::read_html(html_file)

  html_email_fragment <-
    get_html_elements(
      html = html_read,
      selector = selector
    )

  html_email_fragment <- as.character(xml2::xml_children(html_email_fragment))
  html_email_fragment <- paste(html_email_fragment, collapse = "\n")

  html_email_fragment
}

write_blastula_email_input_file <- function(html_fragment) {

  output_file <- tempfile(pattern = "email", fileext = ".Rmd")

  writeLines(
    text = c(
      "---",
      "output: blastula::blastula_email",
      "---",
      "",
      "",
      html_fragment,
      ""
    ),
    con = output_file
  )

  invisible(output_file)
}

read_quarto_connect_json_file <- function(file) {
  jsonlite::fromJSON(txt = file)
}

write_quarto_connect_json_file <- function(obj, path) {
  jsonlite::write_json(obj, path)
}

finalize_quarto_connect_json_file <- function(
    input_json_file,
    output_json_file = NULL,
    rendered_email_obj
) {

  connect_email_obj <- read_quarto_connect_json_file(file = input_json_file)

  connect_email_obj <-
    c(
      connect_email_obj,
      list(
        rsc_email_body_html = rendered_email_obj[["html_str"]],
        rsc_email_images = rendered_email_obj[["images"]]
      )
    )

  if (is.null(output_json_file)) {
    output_json_file <- input_json_file
  }

  write_quarto_connect_json_file(connect_email_obj, path = output_json_file)
}
