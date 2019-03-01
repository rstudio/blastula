#' Send an email message through SMTP
#'
#' Send an email message to one or more recipients via an SMTP server.
#'
#' @param email The email message object, as created by the
#'   \code{compose_email()} function. The object's class is \code{email_message}
#' @param from The email address of the sender. This does not have to be the
#'   same email that is associated with the account actually sending the
#'   message.
#' @param to A vector of email addresses serving as primary recipients for the
#'   message. For secondary recipients, use the \code{cc} and \code{bcc}
#'   arguments.
#' @param subject The subject of the message, which is usually a brief summary
#'   of the topic of the message.
#' @param cc A vector of email addresses for sending the message as a carbon
#'   copy. This list of for those who are to receive a copy of a message
#'   addressed primarily to another. The list of recipients in the CC list is
#'   visible to all other recipients of the message.
#' @param bcc A vector of email addresses for sending the message as blind
#'   carbon copies. Any email addresses provided here will receive the message
#'   and these email addresses will be concealed from other recipients
#'   (including others on the BCC list).
#' @param attachments A vector of paths to files to be attached to the email.
#' @param attach_mime_types An optional vector of mime types to use for each of
#'   the attachments specified in \code{attachments}. If not provided, mime
#'   types will be assigned based on file extensions.
#' @param attach_encodings An optional vector of encoding types to use for each
#'   of the attachments specified in \code{attachments}. Options are
#'   \code{base64}, \code{7bit}, \code{8bit}, or \code{none}.
#' @param attach_dispositions An optional vector of disposition types for each
#'   of the attachments specified in \code{attachments}. Options are
#'   \code{inline} and \code{attachment}.
#' @param creds_file An optional path to an email credentials file. This file
#'   must be created by the \code{create_email_creds_file()} function.
#' @param sender The sender name.
#' @param host The email host.
#' @param port The port associated with the email account.
#' @param user The username associated with the email account.
#' @param password The password associated with the email account.
#' @param use_ssl A logical value to indicate whether to use SSL.
#' @param authenticate A logical value to indicate whether to use
#'   authentication.
#' @param echo An option to print the standard output and error to the screen.
#' @param echo_cmd A logical value indicating whether the system command should
#'   be printed to the console during the sending of email.
#' @importFrom glue glue
#' @importFrom dplyr tibble bind_cols pull
#' @importFrom tidyr unite
#' @export
smtp_send <- function(email,
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
                      authenticate = TRUE,
                      binary_loc = NULL,
                      echo = FALSE,
                      echo_cmd = FALSE) {

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(email, "email_message")) {
    stop("The object provided in `email` must be an ",
         "`email_message` object.\n",
         " * This can be created with the `compose_email()` function.",
         call. = FALSE)
  }

  # Establish the location of the `mailsend-go` binary
  if (is.null(binary_loc)) {
    # binary_loc <- find_mailsend()
  }

  # Write the inlined HTML message out to a file
  email$html_html %>%
    writeLines(con = "message_inlined.html")

  # Handle a subject line that's not provided and use
  # `glue::glue()` for customizing a given `subject`
  if (is.null(subject)) {
    subject <- "<no subject>"
  } else {
    subject <- glue::glue(subject)
  }

  # Wrap the `subject` in single quotes
  subject <- paste0("'", subject, "'")

  # If a path to a credentials file is provided,
  # read in the values
  if (!is.null(creds_file)) {

    credentials <- readRDS(creds_file) %>% as.list()

  } else {

    credentials <-
      list(
        sender = sender,
        host = host,
        port = port,
        user = user,
        password = password,
        use_ssl = use_ssl,
        use_tls = use_tls,
        authenticate = authenticate
      )
  }

  # Send text email
  processx::run(
    command = binary_loc,
    args = c(
      "-sub", subject,
      "-smtp", credentials$host,
      "-port", credentials$port,
      ifelse(credentials$use_ssl, "-ssl", ""),
      "auth",
        "-user", credentials$user, "-pass", credentials$password,
      "-from", from, "-to", to,
      "body",
        "-file", "message_inlined.html"),
    echo = echo,
    echo_cmd = echo_cmd
  )

  # Remove the `message_inlined.html` file
  if (file.exists("message_inlined.html")) {
    file.remove("message_inlined.html")
  }
}


