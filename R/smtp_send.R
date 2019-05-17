#' Send an email message through SMTP
#'
#' Send an email message to one or more recipients via an SMTP server.
#'
#' @param email The email message object, as created by the `compose_email()`
#'   function. The object's class is `email_message`
#' @param from The email address of the sender. This does not have to be the
#'   same email that is associated with the account actually sending the
#'   message.
#' @param to A vector of email addresses serving as primary recipients for the
#'   message. For secondary recipients, use the `cc` and `bcc` arguments.
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
#' @param credentials One of three credential helper functions must be used
#'   here: (1) [creds_file()], (2) [creds_key()], or (3) [creds()]. The first,
#'   [creds_file()], relies on a credentials file stored on disk. Such a file is
#'   created using the [create_smtp_creds_file()] function. The [creds_key()] is
#'   used if credentials are stored in the system-wide key-value store, thorugh
#'   use of the [create_smtp_creds_key()] function. Using [creds()] allows for
#'   manual specification of SMTP configuration and credentials within that
#'   helper function.
#' @param binary_loc An option to supply the location of the `mailsend-go`
#'   binary file should it not be on the system path or in the working
#'   directory.
#' @param echo An option to print the standard output and error to the screen.
#' @param echo_cmd A logical value indicating whether the system command should
#'   be printed to the console during the sending of email.
#' @param ... The `...` is unused and only serves to force the naming of
#'   subsequent argument (`dry_run`) and avoid an unintended use.
#' @param dry_run Setting `dry_run` to `TRUE` will return information on the
#'   SMTP sending options. Furthermore, the function will stop short of actually
#'   sending the email message out. By default, however, this is set to `FALSE`.
#' @examples
#' \dontrun{
#' # Prepare a test message and send
#' # the email out with `smtp_send()`
#' prepare_test_message() %>%
#'   smtp_send(
#'     from = "sender@mail.com",
#'     to = "recipient@mail.com",
#'     subject = "Mail Subject",
#'     credentials = creds_file("mail_creds")
#'   )
#' }
#' @export
smtp_send <- function(email,
                      from,
                      to,
                      subject = NULL,
                      cc = NULL,
                      bcc = NULL,
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
                      echo_cmd = FALSE,
                      debug = FALSE) {

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
    binary_loc <- find_binary("mailsend-go")
    if (is.null(binary_loc)) {
      stop("The binary file `mailsend-go` is not in the system path or \n",
           "in the working directory:\n",
           " * install `mailsend-go` using the instructions at ",
           "https://github.com/muquit/mailsend-go#downloading-and-installing",
           call. = FALSE)
    }
  }

  # Write the inlined HTML message out to a file
  email$html_str %>%
    writeLines(con = "message_inlined.html")

  # Remove the file after the function exits
  on.exit(file.remove("message_inlined.html"))

  # Handle a subject line that's not provided and use
  # `glue::glue()` for customizing a given `subject`
  if (is.null(subject)) {
    subject <- "<no subject>"
  } else {
    subject <- glue::glue(subject) %>% as.character()
  }

  # Create comma-separated addresses for
  # `to`, `cc`, and `bcc`
  to <- make_address_list(to)
  cc <- make_address_list(cc)
  bcc <- make_address_list(bcc)

  # If a path to a credentials file is provided,
  # read in the values
  if (!is.null(creds_file)) {

    credentials <- readRDS(creds_file) %>% as.list()

    # Overwrite sender name if one is provided
    if (!is.null(sender)) credentials$sender <- sender

  } else {

    if (is.null(password)) {
      password <- getPass::getPass("Enter the SMTP server password: ")
    }

    credentials <-
      list(
        sender = sender,
        host = host,
        port = port,
        user = user,
        password = password,
        use_ssl = use_ssl,
        authenticate = authenticate
      )
  }

  # Set the `ssl` flag depending on the options provided
  if (credentials$use_ssl) {
    ssl_opt <- no_options()
  } else {
    ssl_opt <- no_arg()
  }

  # Collect arguments and options for for `processx::run()`
  # as a list
  run_args <-
    list(
      `-sub` = subject,
      `-smtp` = credentials$host,
      `-port` = credentials$port,
      `-ssl` = ssl_opt,
      `auth` = no_options(),
      `-user` = credentials$user,
      `-pass` = credentials$password,
      `-fname` = credentials$sender,
      `-from` = from,
      `-to` = to,
      `-cc` = cc,
      `-bcc` = bcc,
      `attach` = no_options(),
      `-file` = "message_inlined.html",
      `-mime-type` = "text/html",
      `-inline` = no_options()
    )

  # Create the vector of arguments related
  # to file attachments
  attachment_args_vec <- create_attachment_args_vec(email = email)

  # Clean up arguments and options; create the
  # vector that's needed for `processx::run()`
  run_args <-
    run_args %>%
    prune_args() %>%
    create_args_opts_vec() %>%
    append_attachment_args_vec(
      attachment_args_vec = attachment_args_vec
    )

  if (!debug) {

    # Send out email via `processx::run()` and
    # assign the result
    send_result <-
      processx::run(
        command = binary_loc,
        args = run_args,
        echo = echo,
        echo_cmd = echo_cmd
      )

    if (send_result$status == 0) {
      message("The email message was sent successfully.\n")
    } else {
      message("The email message was NOT successfully sent.\n")
    }

  }

}
