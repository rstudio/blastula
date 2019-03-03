# SMTP providers with settings
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
    "gmx",         "mail.gmx.com",             587,   TRUE,     FALSE,     TRUE,          "email", "GMX",
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

#' Make a formatted address list string
#' @param addresses a vector of addresses
#' @noRd
make_address_list <- function(addresses) {
  if (!is.null(addresses)) {
    return(paste(addresses, collapse = ","))
  } else {
    return(NULL)
  }
}

#' Get a text string identifying the system's OS
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
#' @noRd
is_windows_os <- function() {
  get_os_type() == "windows"
}

#' Is this system using macOS?
#' @noRd
is_mac_os <- function() {
  get_os_type() == "mac_os"
}

#' Is this system using Linux?
#' @noRd
is_linux_os <- function() {
  get_os_type() == "linux"
}

#' Is this system using Unix (not macOS nor Linux)?
#' @noRd
is_unix_os <- function() {
  get_os_type() == "unix"
}

#' Is this system using an unknown OS?
#' @noRd
is_unknown_os <- function() {
  get_os_type() == "unknown"
}

#' Create a character object that signals `no_options`
#' @noRd
no_options <- function() {
  no_opts <- "no_options"
  class(no_opts) <- "no_options"
  no_opts
}

#' Create a character object that signals `no_arg`
#' @noRd
no_arg <- function() {
  no_arg_ <- "no_arg"
  class(no_arg_) <- "no_arg"
  no_arg_
}

#' @param bin_name The name of the binary to search for
#' @importFrom processx run
#' @noRd
find_binary <- function(bin_name) {

  # Find binary on path with `Sys.which()`
  which_result <- Sys.which(bin_name) %>% unname()

  if (which_result != "") {
    return(which_result)
  }

  # Find binary in working directory
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

  NULL
}
