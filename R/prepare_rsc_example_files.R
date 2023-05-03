#' Prepare example files for RStudio Connect emailing with R Markdown
#'
#' A set of example files relevant to emailing with R Markdown in RStudio
#' Connect can be spawned in a specified location. There is a set of three files
#' that work together to provide a full report, an emailable version of that
#' report, and a file attachment; these files are:
#'
#' \itemize{
#' \item `"connect-example-main.Rmd"`: The main R Markdown document. Contains
#' a report template culminating in a final R code chunk that has calls to
#' [render_connect_email()] and [attach_connect_email()].
#' \item `"connect-example-email.Rmd"`: An R Markdown document that contains
#' the email message. It is associated with the main R Markdown document by
#' incorporating some of its content (i.e., by reusing chunk names and extending
#' assigned values). It uses the `blastula::blastula_email` output type in the
#' YAML front matter.
#' \item `"austin_home_sales.csv"`: A CSV file that will be included as an
#' attachment by way of the `attachments` argument in the
#' [attach_connect_email()] function call within the main R Markdown document.
#' }
#'
#' The main report and associated email can be published by opening
#' `"connect-example-main.Rmd"` and pressing the Publish button at the top-right
#' of the Editor pane (please ensure beforehand that you are set up work with
#' RStudio Connect). If asked `"What do you want to publish?"`, choose the first
#' option where only the `"connect-example-main"` document is published. All
#' three files should be checked in the final dialog box, press the `Publish`
#' button to publish to RStudio Connect.
#'
#' There is also the single `"connect-example-text-only.Rmd"` file that, when
#' published, serves as a mechanism to send a text-only email. The content of
#' the email is specified directly in the single [attach_connect_email()]
#' function call and all other text in the R Markdown file is disregarded.
#'
#' @param path The location to which the files (in a subdirectory named
#'   `"connect_examples"`) will be written. The path needs to exist but the
#'   aforementioned subdirectory is not required to be present.
#'
#' @export
prepare_rsc_example_files <- function(path = NULL) {

  subdir <- "connect_examples"

  # Write the output file names
  file_names <-
    c(
      "connect-example-main.Rmd",
      "connect-example-email.Rmd",
      "connect-example-text-only.Rmd",
      "austin_home_sales.csv"
    )

  file_paths <- file.path(subdir, file_names)

  if (!is.null(path)) {

    if (!dir.exists(path)) {
      stop("The provided `path` must already exist in the filesystem",
           call. = FALSE)
    }

    file_paths <- file.path(path, file_paths)

    dir.create(file.path(path, subdir))

    # Write the files
    for (i in seq(file_names)) {

      file.copy(
        from = system.file(file.path("examples", file_names[i]), package = "blastula"),
        to = file_paths[i])
    }

    path_desc <- paste0("`", file.path(path, subdir), "`")

  } else {

    dir.create(subdir)

    # Write the files
    for (i in seq(file_names)) {

      file.copy(
        from = system.file(file.path("examples", file_names[i]), package = "blastula"),
        to = file_paths[i])
    }

    path_desc <- paste0("the working directory (`", here::here(), "`)")
  }

  # Display a message
  message(paste0("The Connect example files were written to ", path_desc, "."))
}
