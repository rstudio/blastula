#' Minify an email
#'
#' Minify a blastula `email_message` object.
#'
#' The [minification](https://en.wikipedia.org/wiki/Minification_(programming))
#' relies on the binary
#' [**minify**](https://github.com/tdewolff/minify/tree/master/cmd/minify)
#' which is cross-platform and works on Windows, macOS, Linux and BSD.
#' Pre-built binaries can be downloaded from
#' [here](https://github.com/tdewolff/minify/releases). Alternatively,
#' instructions to build minify from source are available
#' [here](https://github.com/tdewolff/minify/tree/master/cmd/minify#installation).
#'
#' @param email The email message object, as created by the [compose_email()]
#'   function. The object's class is `email_message`.
#' @param binary_loc An option to supply the location of the `minify`
#'   binary file should it not be on the system path or in the working
#'   directory.
#' @param minify_opts A list of additional options to pass to the `minify` CLI
#'   command. Only the following sensible subset of
#'   [all possible minify options](https://github.com/tdewolff/minify/tree/master/cmd/minify#usage)
#'   is supported, listed with their default values:
#'   - \code{`html-keep-conditional-comments` = FALSE}: Preserve all IE conditional comments
#'   - \code{`html-keep-default-attrvals` = FALSE}: Preserve default attribute values
#'   - \code{`html-keep-document-tags` = FALSE}: Preserve `<html>`, `<head>` and `<body>` tags
#'   - \code{`html-keep-end-tags` = FALSE}: Preserve all end tags
#'   - \code{`html-keep-quotes` = FALSE}: Preserve quotes around attribute values
#'   - \code{`html-keep-whitespace` = FALSE}: Preserve whitespace characters but still collapse multiple into one
#'   - \code{`css-decimals` = -1L}: Number of decimals to preserve in CSS numbers,
#'     `-1L` means all
#'   - \code{`svg-decimals` = -1L}: Number of decimals to preserve in SVG numbers,
#'     `-1L` means all
#'   - `verbose = FALSE`: Print informative messages about minification details.
#' @param echo If set to `TRUE`, the command to minify the `email_message`
#'   object's HTML via `minify` will be printed to the console. By default,
#'   this is `FALSE`.
#'
#' @examples
#' \donttest{# Create a simple test email
#' test_mail <- prepare_test_message()
#'
#' # Minify the test email
#' minify(test_mail)
#'
#' # The command used to minify can be printed
#' minify(email = test_mail,
#'        echo = TRUE)
#'
#' # We can also provide options to the
#' # underlying minify command
#' minify(email = test_mail,
#'        minify_opts = list(`html-keep-conditional-comments` = TRUE,
#'                           `html-keep-default-attrvals` = TRUE,
#'                           verbose = TRUE),
#'        echo = TRUE)
#' }
#'
#' @return An `email_message` object.
#' @export
minify <- function(email,
                   binary_loc = NULL,
                   minify_opts = NULL,
                   echo = FALSE) {

  # Verify that the `email` object
  # is of the class `email_message`
  if (!inherits(email, "email_message")) {
    stop("The object provided in `email` must be an ",
         "`email_message` object.\n",
         " * This can be created with the `compose_email()` function.",
         call. = FALSE)
  }

  # Determine the location of the `minify` binary
  if (is.null(binary_loc)) {
    binary_loc <- find_binary("minify")
    if (is.null(binary_loc)) {
      stop("The binary file `minify` is not in the system path or \n",
           "in the working directory:\n",
           " * download a pre-built binary from https://github.com/tdewolff/minify/releases\n",
           " * or follow the installation instructions at https://github.com/tdewolff/minify/tree/master/cmd/minify#installation",
           call. = FALSE)
    }
  }

  # Ensure provided minify options are valid
  # and collect arguments and options for for `processx::run()` as a list
  run_args <- character(0)

  if (!is.null(minify_opts)) {
    binary_opts <-
      c("html-keep-conditional-comments",
        "html-keep-default-attrvals",
        "html-keep-document-tags",
        "html-keep-end-tags",
        "html-keep-quotes",
        "html-keep-whitespace",
        "verbose") %>%
      intersect(y = names(minify_opts))

    int_opts <-
      c("css-decimals",
        "svg-decimals") %>%
      intersect(y = names(minify_opts))

    invalid_opts_i <- which(
      !(names(minify_opts) %in% c(binary_opts, int_opts))
    )

    if (length(invalid_opts_i) > 0) {
      stop("Unknown options provided in `minify_opts`: ", names(minify_opts[invalid_opts_i]),
           call. = FALSE)
    }

    if (length(binary_opts) > 0) {
      invalid_opts_i <- which(
        names(minify_opts) %in% binary_opts & !sapply(minify_opts[binary_opts], is.logical)
      )

      if (length(invalid_opts_i) > 0) {
        stop("The following `minify_opts` must be of type logical: ",
             names(minify_opts[invalid_opts_i]),
             call. = FALSE)
      }

      run_args <- c(names(minify_opts[sapply(minify_opts, isTRUE)])) %>% paste0("--", .)
    }

    if (length(int_opts) > 0) {
      minify_opts[int_opts] <- as.integer(minify_opts[int_opts])

      invalid_opts_i <-
        names(minify_opts) %in% int_opts %>%
        magrittr::and(!sapply(minify_opts[int_opts],
                              function(x) x >= -1L)) %>%
        which()

      if (length(invalid_opts_i) > 0) {
        stop("The following `minify_opts` must be >= -1: ", names(minify_opts[invalid_opts_i]),
             call. = FALSE)
      }

      run_args <- minify_opts[int_opts] %>% paste0(names(.), "=", .)
    }
  }

  # Write the inlined HTML message out to a file
  # and remove the file after the function exits
  tempfile_ <- tempfile(fileext = ".html") %>% tidy_gsub("\\\\", "/")
  email$html_str %>% writeLines(con = tempfile_, useBytes = TRUE)
  on.exit(file.remove(tempfile_))

  # add input and file type
  run_args <- c("--type=html", run_args, tempfile_)

  # Minify via `processx::run()` and assign the result
  minify_result <- processx::run(command = binary_loc,
                                 args = run_args,
                                 echo_cmd = echo,
                                 timeout = 60L)

  if (isTRUE(minify_opts$verbose)) message(minify_result$stderr)

  email$html_str <- minify_result$stdout
  email
}
