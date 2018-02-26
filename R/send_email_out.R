#' Send an email message
#' @description Send an email message to one
#' or more recipients.
#' @param message the email message object,
#' as created by the \code{compose_email()}
#' function. The object's class is
#' \code{email_message}
#' @param from the email address of the
#' sender. This does not have to be
#' the same email that is associated with
#' the account actually sending the message.
#' @param to a vector of email addresses
#' serving as primary recipients for the
#' message. For secondary recipients, use
#' the \code{cc} and \code{bcc} arguments.
#' @param subject the subject of the
#' message, which is usually a brief summary
#' of the topic of the message.
#' @param cc a vector of email addresses
#' for sending the messsage as a carbon
#' copy. This list of for those who are to
#' receive a copy of a message addressed
#' primarily to another. The list of
#' recipients in the CC list is visible
#' to all other recipients of the message.
#' @param bcc a vector of email addresses
#' for sending the message as blind carbon
#' copies. Any email addresses provided
#' here will receive the message and these
#' email addresses will be concealed from
#' other recipients (including others on
#' the BCC list).
#' @param attachments a vector of paths
#' to files to be attached to the email.
#' @param attach_mime_types an optional
#' vector of mime types to use for
#' each of the attachments specified in
#' \code{attachments}. If not provided,
#' mime types will be assigned based on
#' file extensions.
#' @param attach_encodings an optional
#' vector of encoding types to use for
#' each of the attachments specified in
#' \code{attachments}. Options are
#' \code{base64}, \code{7bit},
#' \code{8bit}, or \code{none}.
#' @param attach_dispositions an optional
#' vector of disposition types for each
#' of the attachments specified in
#' \code{attachments}. Options are
#' \code{inline} and \code{attachment}.
#' @param creds_file an optional path to
#' an email credentials file. This file
#' must be created by the
#' \code{create_email_creds_file()}
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
#' @param ehlo a logical value to indicate
#' whether to force an EHLO command after
#' connection to the SMTP host.
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
#' @importFrom dplyr tibble bind_cols pull
#' @importFrom tidyr unite
#' @export send_email_out

send_email_out <- function(message,
                           from,
                           to,
                           subject = NULL,
                           cc = NULL,
                           bcc = NULL,
                           attachments = NULL,
                           attach_mime_types = NULL,
                           attach_encodings = NULL,
                           attach_dispositions = NULL,
                           creds_file = NULL,
                           sender = NULL,
                           host = NULL,
                           port = NULL,
                           user = NULL,
                           password = NULL,
                           use_ssl = TRUE,
                           use_tls = FALSE,
                           authenticate = TRUE,
                           ehlo = FALSE,
                           verbose = FALSE,
                           debug = FALSE) {

  # If mailsend binary is not available, then
  # call `blast_first()` to get it
  if (
    !file.exists(
      system.file("exec", "mailsend", package = "blastula"))) {

    blast_first()
  }

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(x = message, what = "email_message")) {
    stop("The object provided in `message` must be created by the `compose_email()` function.")
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

  if (is.null(subject)) {
    subject_text <- "<no subject>"
  } else {
    subject_text <- glue::glue(subject)
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
      bcc <- bcc %>% paste(collapse = ",")
    }
  }

  # Process any file attachments
  if (!is.null(attachments)) {

    attach_tbl <-
      dplyr::tibble(files = paste0("-attach ", attachments))

    if (!is.null(attach_dispositions)) {
      if (length(attach_dispositions) == length(attachments)) {
        attach_dispositions <- c("attachment", "attachment")
        dispositions_tbl <-
          dplyr::tibble(dispositions = paste0("-disposition \"", attach_dispositions, "\""))

        attach_tbl <-
          dplyr::bind_cols(dispositions_tbl, attach_tbl)
      }
    }

    if (!is.null(attach_encodings)) {
      if (length(attach_encodings) == length(attachments)) {
        attach_encodings <- c("base64", "base64")
        encodings_tbl <-
          dplyr::tibble(encodings = paste0("-enc-type \"", attach_encodings, "\""))

        attach_tbl <-
          dplyr::bind_cols(encodings_tbl, attach_tbl)
      }
    }

    if (!is.null(attach_mime_types)) {
      if (length(attach_mime_types) == length(attachments)) {
        attach_mime_types <- c("text/plain", "text/plain")
        mime_types_tbl <-
          dplyr::tibble(mime_types = paste0("-mime-type \"", attach_mime_types, "\""))

        attach_tbl <-
          dplyr::bind_cols(mime_types_tbl, attach_tbl)
      }
    }

    # Create the attachment expression
    attach_expr <-
      attach_tbl %>%
      tidyr::unite(expr, colnames(attach_tbl), sep = " ") %>%
      dplyr::pull(expr) %>%
      paste(collapse = " ")
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
-from \"{from}\" \\
-name \"{sender}\" \\
-t \"{to}\" \\
{ifelse(!is.null(cc), paste0('-cc ', cc), '+cc')} \\
{ifelse(!is.null(bcc), paste0('-bc ', bcc), '+bc')} \\
-sub \"{subject}\" \\
{ifelse(use_ssl, '-ssl', '')} \\
{ifelse(use_tls, '-starttls', '')} \\
{ifelse(ehlo, '-ehlo', '')} \\
-smtp {host} \\
-port {port} \\
-auth \\
-user {user} \\
-pass \"{password}\" \\
{ifelse(verbose, '-v', '')} \\
-mime-type \"text/html\" \\
-enc-type \"us-ascii\" \\
-disposition \"inline\" \\
-attach \"message_inlined.html\" \\
{ifelse(exists('attach_expr'), attach_expr, '')}")

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
