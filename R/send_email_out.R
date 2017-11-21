#' Send an email message
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
#' @param recipients a vector of email
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
#' @param debug a logical value to indicate
#' whether a detailed debug information
#' should be printed to the console
#' during sending of email.
#' @importFrom glue glue
#' @export send_email_out

send_email_out <- function(message,
                           subject = NULL,
                           from = NULL,
                           recipients = NULL,
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
    sender <- credentials[1]
    host <- credentials[2]
    port <- as.integer(credentials[3])
    user <- credentials[4]
    password <- credentials[5]
    use_ssl <- as.logical(credentials[6])
    use_tls <- as.logical(credentials[7])
    authenticate <- as.logical(credentials[8])
  }

  if (!is.null(from)) {
    sender <- from
  }

  if (is.null(recipients)) {
    recipients <- sender
  }

  # Send the message
  mailR::send.mail(
    from = sender,
    to = recipients,
    subject = subject,
    body = message[["html_str"]],
    smtp = list(
      host.name = host,
      port = port,
      user.name = user,
      passwd = password,
      ssl = use_ssl,
      tls = use_tls),
    authenticate = authenticate,
    html = TRUE,
    encoding = "utf-8",
    attach.files = attachments,
    debug = debug)
}
