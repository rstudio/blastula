
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
                      debug = FALSE) {

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(email, "email_message")) {
    stop("The object provided in `email` must be an ",
         "`email_message` object.\n",
         " * This can be created with the `compose_email()` function.",
         call. = FALSE)
  }

  }

  # Write the inlined HTML message out to a file
  email$html_html %>%
    writeLines(con = "message_inlined.html")

  # Wrap the subject in single quotes
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
    command = "/usr/local/bin/mailsend-go",
    args = c(
      "-sub", subject,
      "-smtp", host,
      "-port", port, "-ssl",
      "auth",
      "-user", user, "-pass", password,
      "-from", from, "-to", to,
      "body",
        "-file", "message_inlined.html"),
    echo = echo,
    echo_cmd = debug
  )


  # Remove the `message_inlined.html` file
  if (file.exists("message_inlined.html")) {
    file.remove("message_inlined.html")
  }
}


