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
