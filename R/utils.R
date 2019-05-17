#' Get a tibble of SMTP providers with settings
#' @importFrom dplyr tribble
#' @noRd
smtp_settings <- function() {
  dplyr::tribble(
    ~short_name,   ~server,                    ~port, ~use_ssl, ~use_tls, ~authenticate, ~user,   ~long_name,
    "gmail",       "smtp.gmail.com",           465,   TRUE,     FALSE,    TRUE,          "email", "Gmail",
    "outlook",     "smtp-mail.outlook.com",    587,   TRUE,     TRUE,     TRUE,          "email", "Outlook.com",
    "office365",   "smtp.office365.com",       587,   FALSE,    TRUE,     TRUE,          "email", "Office365.com",
    "yahoo",       "smtp.mail.yahoo.com",      465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo",
    "yahoo_uk",    "smtp.mail.yahoo.co.uk",    465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo UK",
    "yahoo_au_nz", "smtp.mail.yahoo.au",       465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo AU/NZ",
    "aol",         "smtp.aol.com",             587,   TRUE,     TRUE,     TRUE,          "user",  "AOL",
    "mail_com",    "smtp.mail.com",            587,   FALSE,    TRUE,     TRUE,          "email", "Mail.com",
    "gmx",         "mail.gmx.com",             587,   TRUE,     FALSE,    TRUE,          "email", "GMX",
    "att",         "smtp.att.yahoo.com",       465,   TRUE,     FALSE,    TRUE,          "email", "AT&T",
    "verizon",     "outgoing.verizon.net",     587,   FALSE,    FALSE,    TRUE,          "email", "Verizon",
    "usa_net",     "smtp.postoffice.net",      465,   TRUE,     FALSE,    TRUE,          "email", "USA.NET",
    "ntl",         "smtp.ntlworld.com",        465,   TRUE,     FALSE,    TRUE,          "email", "NTL World",
    "bt",          "smtp.btconnect.com",       25,    FALSE,    FALSE,    TRUE,          "email", "BT Connect",
    "o2_de",       "mail.o2online.de",         25,    FALSE,    FALSE,    TRUE,          "email", "O2 Deutschland",
    "t_online_de", "securesmtp.t-online.de",   587,   FALSE,    TRUE,     TRUE,          "email", "T-Online Deutschland",
    "vodafone_de", "smtp.vodafone.de",         587,   TRUE,     TRUE,     TRUE,          "email", "Vodafone Deutschland",
    "1and1",       "smtp.1and1.com",           587,   FALSE,    TRUE,     TRUE,          "email", "1&1",
    "1and1_de",    "smtp.1und1.de",            587,   FALSE,    TRUE,     TRUE,          "email", "1&1 Deutschland",
    "shaw",        "mail.shaw.ca",             587,   TRUE,     TRUE,     TRUE,          "user",  "Shaw",
    "fastmail",    "smtp.fastmail.com",        587,   TRUE,     TRUE,     TRUE,          "email", "Fastmail",
  )
}

#' A slighly more sensible version of `gsub()`
#'
#' @param x The text to be transformed.
#' @param pattern The regex pattern.
#' @param replacement A replacement for the matched pattern.
#' @noRd
tidy_gsub <- function(x, pattern, replacement) {

  gsub(pattern, replacement, x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Make a single-length character vector with addresses
#'
#' @param addresses A vector of email addresses.
#' @noRd
make_address_list <- function(addresses) {
  if (!is.null(addresses)) {
    return(paste(addresses, collapse = ","))
  } else {
    return(no_arg())
  }
}

#' Get a text string identifying the system's OS
#'
#' @noRd
get_os_type <- function() {

  if (.Platform$OS.type == "windows") {
    return("windows")
  }

  if (unname(Sys.info()["sysname"] == "Linux")) {
    return("linux")
  }

  if (.Platform$OS.type == "unix" && unname(Sys.info()["sysname"] == "Darwin")) {
    return("mac_os")
  }

  if (.Platform$OS.type == "unix" && .Platform$OS.type != "Darwin") {
    return("unix")
  }

  "unknown"
}

#' Is this system using Windows?
#'
#' @noRd
is_windows_os <- function() {
  get_os_type() == "windows"
}

#' Is this system using macOS?
#'
#' @noRd
is_mac_os <- function() {
  get_os_type() == "mac_os"
}

#' Is this system using Linux?
#'
#' @noRd
is_linux_os <- function() {
  get_os_type() == "linux"
}

#' Is this system using Unix (neither macOS nor Linux)?
#'
#' @noRd
is_unix_os <- function() {
  get_os_type() == "unix"
}

#' Is this system using an unknown OS?
#'
#' @noRd
is_unknown_os <- function() {
  get_os_type() == "unknown"
}

#' Create a character object that signals `no_options`
#'
#' @noRd
no_options <- function() {
  no_opts <- "no_options"
  class(no_opts) <- "no_options"
  no_opts
}

#' Create a character object that signals `no_arg`
#'
#' @noRd
no_arg <- function() {
  no_arg_ <- "no_arg"
  class(no_arg_) <- "no_arg"
  no_arg_
}

#' Create vectors of args and vals for a list element
#'
#' @param x An element of the `run_args` list.
#' @noRd
get_arg_opts <- function(x) {

  if (!inherits(x[[1]], "no_options")) {
    arg_opts <- c(names(x), x[[1]])
    } else {
    arg_opts <- names(x)
    }

  arg_opts
}

#' Take a list of args/vals and prune unnecessary arguments
#'
#' @param run_args A list of arguments and associated options.
#' @noRd
prune_args <- function(run_args) {

  run_args[
    !vapply(
      run_args, function(x) inherits(x, "no_arg"),
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE)]
}

#' Create a vector of command arguments and any associated options
#'
#' @param run_args A list of arguments and associated options.
#' @noRd
create_args_opts_vec <- function(run_args) {

  run_args_vec <- c()

  for (i in seq(run_args)) {
    run_args_vec <-
      c(
        run_args_vec,
        run_args[i] %>% get_arg_opts()
      )
  }

  run_args_vec
}

#' Create a file attachment list
#'
#' @param file_path The path for the file to be attached.
#' @param disposition The attachment's disposition, which is either set as
#'   `attachment` (the default) or `inline`.
#' @noRd
create_attachment_list <- function(file_path,
                                   disposition) {

  list(
    file_path = file_path,
    disposition = disposition)
}

#' Add an attachment list to `email$attachments`
#'
#' @noRd
add_attachment_list <- function(email,
                                attachment_list) {

  email$attachments <-
    c(email$attachments, list(attachment_list))

  email
}

#' Create a vector of command arguments and any associated options for any file
#' attachments
#'
#' @param email The email message object, as created by the `compose_email()`
#'   function.
#' @noRd
create_attachment_args_vec <- function(email) {

  if (length(email$attachments) == 0) {
    return(character(0))
  }

  attachment_args_vec <- c()

  for (i in seq(email$attachments)) {

    # Collect arguments and options for for `processx::run()`
    # as a list
    attach_args <-
      list(
        `attach` = no_options(),
        `-file` = email$attachments[[i]]$file_path,
        `-inline` = no_options()
      )

    # Clean up arguments and options; create the
    # vector that's needed for `processx::run()`
    attachment_args_vec <-
      c(attachment_args_vec,
        attach_args %>%
          prune_args() %>%
          create_args_opts_vec()
      )
  }

  attachment_args_vec
}

#' Append the vector of arguments and options for file attachments to the
#' `args_opts_vec` vector of arguments and options
#'
#' @param args_opts_vec The vector created by the `create_args_opts_vec()`
#'   function.
#' @param attachment_args_vec The vector created by the
#'   `create_attachment_args_vec()` utility function.
#' @noRd
append_attachment_args_vec <- function(args_opts_vec,
                                       attachment_args_vec) {

  c(args_opts_vec, attachment_args_vec)
}

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

# nocov start

#' Find a binary on the system path or working directory
#'
#' @param bin_name The name of the binary to search for.
#' @importFrom processx run
#' @noRd
find_binary <- function(bin_name) {

  # Find binary on path with `Sys.which()`
  which_result <- Sys.which(bin_name) %>% unname()

  if (which_result != "") {
    return(which_result)
  }

  # Try to locate the binary in working directory
  which_result <-
    tryCatch(
      {
        processx::run(command = "ls", args = bin_name)
        file.path(getwd(), bin_name)
      },
      error = function(cond) ""
    )

  if (which_result != "") {
    return(which_result)
  }

  # If the binary isn't found in these locations,
  # return `NULL`
  NULL
}

#' Upload an image to Imgur and return the response
#'
#' @importFrom httr POST add_headers upload_file stop_for_status content
#' @importFrom xml2 as_list read_xml
#' @noRd
imgur_upload <- function(file, client_id) {

  response <-
    httr::POST(
      "https://api.imgur.com/3/image.xml",
      config = httr::add_headers(
        Authorization = paste("Client-ID", client_id)),
      body = list(image = httr::upload_file(file))
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
