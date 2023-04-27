#' Get a tibble of SMTP providers with settings
#'
#' @noRd
smtp_settings <- function() {
  # use_ssl means STARTTLS, not smtps://
  dplyr::tribble(
    ~short_name,   ~server,                    ~port, ~use_ssl, ~user,   ~long_name,
    "gmail",       "smtp.gmail.com",           465,   FALSE,    "email", "Gmail",
    "outlook",     "smtp-mail.outlook.com",    587,   TRUE,     "email", "Outlook.com",
    "office365",   "smtp.office365.com",       587,   TRUE,     "email", "Office365.com",
  )
}

#' Get a vector of SMTP provider short names
#'
#' @noRd
get_provider_list <- function() {
  smtp_settings()$short_name
}

#' Get a named vector of settings for an SMTP provider
#'
#' @noRd
get_smtp_provider_values <- function(provider) {

  smtp_settings() %>%
    dplyr::filter(short_name == provider) %>%
    as.list()
}

#' A slightly more sensible version of `gsub()`
#'
#' @param x The text to be transformed.
#' @param pattern The regex pattern.
#' @param replacement A replacement for the matched pattern.
#' @noRd
tidy_gsub <- function(x, pattern, replacement, fixed = FALSE) {

  gsub(pattern, replacement, x, fixed = fixed)
}

#' A slightly more sensible version of `grepl()`
#'
#' @param x The text to be transformed.
#' @param pattern The regex pattern.
#' @param fixed If `TRUE`, pattern is a string to be matched as is. Overrides
#'   all conflicting arguments.
#' @noRd
tidy_grepl <- function(x, pattern, fixed = FALSE) {

  grepl(pattern, x, fixed = fixed)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# nocov start

#' Upload an image to Imgur and return the response
#'
#' @noRd
imgur_upload <- function(file, client_id) {

  response <-
    httr::RETRY(
      verb = "POST",
      url = "https://api.imgur.com/3/image.xml",
      config = httr::add_headers(
        Authorization = paste("Client-ID", client_id)),
      body = list(image = httr::upload_file(file)),
      terminate_on = c(403, 404)
    )

  # Convert HTTP error to a `stop()` message if return
  # status isn't favorable
  httr::stop_for_status(response, "upload to Imgur.")

  # Get response content as raw bytes
  result <-
    (httr::content(response, as = "raw") %>%
       xml2::read_xml() %>%
       xml2::as_list()
    )[[1]]

  # If we get a `NULL` in the `link` field, then
  # the image didn't get uploaded to Imgur
  if (is.null(result$link[[1]])) {

    stop(paste0("The image (", file ,") has failed to upload."),
         call. = FALSE)
  }

  # Create a simplified list object from
  # the result
  list(
    id = result$id[[1]],
    link = result$link[[1]],
    type = result$type[[1]],
    anim = ifelse(result$animated[[1]] == "true", TRUE, FALSE),
    width = result$width[[1]] %>% as.numeric(),
    height = result$height[[1]] %>% as.numeric(),
    size = result$size[[1]] %>% as.numeric()
  )
}

# nocov end

#' Prepend a element to a list at a given position
#'
#' @param x The list object.
#' @param values The values to prepend to the list.
#' @param before The index position for the prepending operation.
#' @noRd
prepend_list <- function(x,
                         values,
                         before = 1) {

  n <- length(x)

  stopifnot(before > 0 && before <= n)

  if (before == 1) {

    c(values, x)

  } else {

    c(x[1:(before - 1)], values, x[before:n])
  }
}

#' Helper for creating paths for sidecar output files
#'
#' A sidecar file is a file that contains additional info related to some
#' (primary) file--in this case, the primary file is the output file of the
#' current rmarkdown::render. This function provides a path prefix that can be
#' used to create sidecar files that are in the same directory as the output,
#' with a sensible name.
#'
#' For example, if the render's output is going to myreport.html, then the
#' fig_path would default to something like "myreport_files/figure-html/". This
#' function would return "myreport_", which you can then paste0 with the rest of
#' your path, like "errors.log", to get "myreport_errors.log".
#'
#' This procedure is inherently brittle, because it relies on R Markdown's
#' current behavior of indirectly using the output file path to set fig.path.
#' https://github.com/rstudio/rmarkdown/blob/ff285a071d5457d37a227a7526610c297a898fd4/R/render.R#L603-L606
#'
#' Because of this brittleness, the `default` argument is required, so some
#' reasonable behavior can be expected even if a future version of R Markdown
#' causes this mechanism to fail.
#'
#' (The `condition` and `fig_path` arguments are exposed to make unit testing
#' easier.)
#'
#' @noRd
knitr_sidecar_prefix <- function(default,
  condition = !is.null(knitr::opts_knit$get("rmarkdown.version")),
  fig_path = knitr::opts_chunk$get("fig.path")) {

  if (missing(default)) {
    # Fail fast if default is missing.
    # But, don't eagerly evaluate otherwise, because it might be a stop().
    force(default)
  }

  if (condition && !is.null(fig_path)) {
    m <- regexec("^(.+)_files/figure-[\\w_]+/?$", fig_path, perl = TRUE)
    prefix <- regmatches(fig_path, m)[[1]][2]
    if (!is.na(prefix)) {
      return(prefix)
    }
  }

  return(default)
}
