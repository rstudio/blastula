# Ensure that certain packages are available
blastula_pkg_available()
jsonlite_pkg_available()
rmarkdown_pkg_available()

library(rmarkdown)
library(blastula)
library(jsonlite)

# Get the filename for the rendered-by-Quarto HTML
html_file <- list.files(path = ".", pattern = ".*\\.html")[1]

# Get the filename for the rendered-by-Quarto JSON
json_file <- list.files(path = ".", pattern = ".*\\.json")[1]

# Stop if any of `html_file` or `json_file` are of zero length
if (length(html_file) < 1 || length(html_file) < 1) {
  stop("There is no HTML or JSON file for which to generate a Connect email.")
}

# Stop if the JSON file doesn't contain identifying text


# Generate the fragment of HTML that only contains the emailable material
email_fragment <- get_html_email_fragment(file = html_file)

# Render the email fragment .Rmd and generate a list object with the
# needed components for Connect
rendered_email_obj <-
  render_connect_email(
    input = write_blastula_email_input_file(email_fragment),
    connect_footer = FALSE,
    envir = parent.frame(),
    quiet = TRUE,
    output_options = list(),
    render_options = list()
  )

finalize_quarto_connect_json_file(
  input_json_file = json_file,
  output_json_file = json_file,
  rendered_email_obj = rendered_email_obj
)
