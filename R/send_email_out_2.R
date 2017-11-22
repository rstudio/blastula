#' Send an email message // Experimental implementation
#' @description Send an email message to one
#' or more recipients.
#' @param message the email message object,
#' as created by the \code{compose_email()}
#' function. The object's class is
#' \code{email_message}
#' @param subject the subject of the
#' email.
#' @param from the email address of the
#' sender. This does not have to be
#' the same email that is associated with
#' the account actually sending the message.
#' @param to a vector of email
#' addresses.
#' @param cc a vector of email
#' addresses.
#' @param bcc a vector of email
#' addresses.
#' @param attachments a vector of paths
#' to files to be attached to the email.
#' @param creds_file an optional path to
#' an email credentials file. This file
#' must be created by the
#' \code{create_email_creds_file}
#' function.
#' @param sender the sender name.
#' @param host the email host.
#' @param port the port associated with
#' the email account.
#' @param user the username associated
#' with the email account.
#' @param password the password associated
#' with the email account.
#' @param use_ssl a logical value to
#' indicate whether to use SSL.
#' @param use_tls a logical value to
#' indicate whether to use TLS.
#' @param authenticate a logical value to
#' indicate whether to use authenication.
#' @param verbose a logical value indicating
#' whether verbose messages should be
#' printed to the console during sending
#' of email.
#' @param debug a logical value to indicate
#' whether the mail sending statement
#' should be printed to the console. No
#' emails are sent when debug is set to
#' \code{TRUE}.
#' @importFrom glue glue
#' @export send_email_out_2

send_email_out_2 <- function(message,
                             subject = NULL,
                             from = NULL,
                             to = NULL,
                             cc = NULL,
                             bcc = NULL,
                             attachments = NULL,
                             creds_file = NULL,
                             sender = NULL,
                             host = NULL,
                             port = NULL,
                             user = NULL,
                             password = NULL,
                             use_ssl = TRUE,
                             use_tls = FALSE,
                             authenticate = TRUE,
                             verbose = FALSE,
                             debug = FALSE) {

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(x = message, what = "email_message")) {
    stop("The object provided in `message` must be created by the `compose_email()` function.")
  }

  if (is.null(subject)) {
    subject_text <- "<no subject>"
  } else {
    subject_text <- glue::glue(subject)
  }

  # If a path to a credentials file is provided,
  # read in the values
  if (!is.null(creds_file)) {

    # Read in email credentials from `creds_file`
    credentials <- readRDS(creds_file)
    sender <- ifelse(!is.null(sender), sender, credentials[1])
    host <- ifelse(!is.null(host), host, credentials[2])
    port <- ifelse(!is.null(port), port, as.integer(credentials[3]))
    user <- ifelse(!is.null(user), user, credentials[4])
    password <- ifelse(!is.null(password), password, credentials[5])
    use_ssl <- as.logical(credentials[6])
    use_tls <- as.logical(credentials[7])
    authenticate <- as.logical(credentials[8])
  }

  if (use_ssl & use_tls) {
    use_ssl <- FALSE
    use_tls <- TRUE
  }

  if (is.null(to)) {
    to <- from
  }

  if (length(to) > 1) {
    to <- to %>% paste(collapse = ",")
  }

  if (!is.null(cc)) {
    if (length(cc) > 1) {
      cc <- cc %>% paste(collapse = ",")
    }
  }

  if (!is.null(bcc)) {
    if (length(bcc) > 1) {
      c <- bcc %>% paste(collapse = ",")
    }
  }

  # Get the system OS type
  os <-
    switch(
      Sys.info()[['sysname']],
      Windows = "win",
      Linux   = "linux",
      Darwin  = "mac_os")

  # Stop function if there is no binary
  # available for use
  if (system.file(
    package = "blastula", "exec",
    "mailsend") == "" &
    system.file(
      package = "blastula", os,
      "mailsend") == "") {
    stop("Please set up blastula by using the `blast_first()` function.
  Then, after doing that, try sending the message again.", call. = FALSE)
  }

  if (system.file(
    package = "blastula", "exec",
    "mailsend") != "") {
    binary_location <-
      system.file(
        package = "blastula", "exec",
        "mailsend")
  } else if (system.file(
    package = "blastula", os,
    "mailsend") != "") {
    binary_location <-
      system.file(
        package = "blastula", os,
        "mailsend")
  }

  # Copy binary to working directory
  file.copy(
    from = binary_location,
    to = paste0(getwd(), "/mailsend"))

  # Modify file permissions
  Sys.chmod(
    paste0(getwd(), "/mailsend"),
    mode = "0777", use_umask = TRUE)

  # Write the inlined HTML message
  # out to a file
  message$html_html %>%
    cat(file = paste0(getwd(), "/message_inlined.html"))

  # Send the message
  command <-
    glue::glue(
"{paste0(getwd(), \"/mailsend\")} \\
-from {from} \\
-name {sender} \\
-t \"{to}\" \\
{ifelse(!is.null(cc), paste0('-cc ', cc), '+cc')} \\
{ifelse(!is.null(bcc), paste0('-bc ', bcc), '+bc')} \\
-sub \"{subject}\" \\
{ifelse(use_ssl, '-ssl', '')} \\
{ifelse(use_tls, '-starttls', '')}\\
-auth \\
-smtp {host} \\
-port {port} \\
-user {user} \\
-pass {password} \\
{ifelse(verbose, '-v', '')} \\
-content-type \"multipart/related\" \\
-mime-type text/html \\
-msg-body \"message_inlined.html\"")

  if (debug == TRUE) {
    cat(command)
  } else {
    system(command = command, intern = TRUE)
  }

  # Remove the `message_inlined.html` file
  if (file.exists("message_inlined.html")) {
    file.remove("message_inlined.html")
  }

  # Remove the binary file
  if (file.exists("mailsend")) {
    file.remove("mailsend")
  }
}
