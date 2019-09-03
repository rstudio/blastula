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
                         quiet = TRUE,
                         output_options = list(),
                         render_options = list()) {

  output_file <- tempfile(pattern = "email", fileext = ".html")
  on.exit(unlink(output_file))

  output_options$self_contained <- TRUE

  do.call(rmarkdown::render, c(
    list(
      input = input,
      output_file = output_file,
      envir = envir,
      output_options = output_options,
      quiet = quiet
    ),
    render_options
  ))

  email_obj <- cid_images(output_file)
  email_obj
}

#' @export
render_connect_email <- function(input,
                                 connect_footer = TRUE,
                                 envir = parent.frame(),
                                 quiet = TRUE,
                                 output_options = list(),
                                 render_options = list()) {

  if (is.null(output_options$connect_footer)) {
    output_options$connect_footer <- connect_footer
  }
  render_email(input, envir, quiet, output_options, render_options)
}
