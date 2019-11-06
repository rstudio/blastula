#' R Markdown render functions for the `blastula_email` output format
#'
#' The `render_email()` and `render_connect_email()` functions both allow for
#' rendering an an email message. We can supply an R Markdown document (.Rmd)
#' with the output specified as `output: blastula::blastula_email`. While the
#' `render_email()` and `render_connect_email()` functions have similar
#' arguments, the `render_connect_email()` is preferred when publishing to the
#' RStudio Connect service. It allows for the inclusion of a predefined footer
#' that contains useful links for email recipients.
#'
#' @param input The input file to be rendered. This should be an R Markdown
#'   document (.Rmd) with the output specified as `output:
#'   blastula::blastula_email`.
#' @param envir The environment in which the code chunks are to be evaluated
#'   during knitting.
#' @param quiet An option to suppress printing of the command line output from
#'   Pandoc during rendering. By default, this is set to `TRUE`.
#' @param output_options,render_options Lists of options can be used to augment
#'   the rendering of the email message. The `output_options` list will be
#'   passed as the `output_options` argument of `rmarkdown::render()`. The
#'   `render_options` list is for providing additional arguments to
#'   `rmarkdown::render()`. By default, both lists are empty.
#' @param connect_footer Should a prepared footer message with links be included
#'   in the rendered email? This argument is only available in the
#'   `render_connect_email()` function and is set to `TRUE` by default.
#'
#' @name render_email
NULL

# nocov start

#' @rdname render_email
#' @export
render_email <- function(input,
                         envir = parent.frame(),
                         quiet = TRUE,
                         output_options = list(),
                         render_options = list()) {

  output_file <- tempfile(pattern = "email", fileext = ".html")
  on.exit(unlink(output_file), add = TRUE)

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

#' @rdname render_email
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

# nocov end

