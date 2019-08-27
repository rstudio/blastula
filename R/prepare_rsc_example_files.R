#' @export
prepare_rsc_example_files <- function(path = NULL) {

  subdir <- "connect_examples"

  # Write the output file names
  file_names <-
    c(
      "connect-example-main.Rmd",
      "connect-example-email.Rmd",
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
