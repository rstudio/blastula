# Ensure that certain packages are available
blastula_pkg_available()
jsonlite_pkg_available()
rmarkdown_pkg_available()

library(rmarkdown)
library(blastula)
library(jsonlite)

# Testing with two different HTML renders from Quarto
quarto_render_html_file <-
  system.file(
    "quarto_example_documents/quarto-report-render.html",
    package = "blastula"
  )

quarto_render_html_file_2 <-
  system.file(
    "quarto_example_documents/quarto-report-render-02.html",
    package = "blastula"
  )

# Generate the fragment of HTML that only contains the emailable material
email_fragment <- get_html_email_fragment(quarto_render_html_file_2)

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
  input_json_file = system.file(
    "quarto_example_documents/connect-email.json",
    package = "blastula"
  ),
  output_json_file = "finalized.json",
  rendered_email_obj = rendered_email_obj
)
