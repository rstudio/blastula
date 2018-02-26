#' Prepare a email test message object
#'
#' Create an email test message object,
#' which is helpful for sending a test
#' message with the \code{send_email_out()}
#' function.
#' @examples
#' \dontrun{
#' # Create a credentials file to
#' # send via Gmail (this will be named
#' # `.bls_smtp_gmail_com`)
#' create_email_creds_file(
#'   user = "username@gmail.com",
#'   password = "*************",
#'   provider = "gmail",
#'   sender = "Sender Name")
#'
#' # Send oneself a test message to
#' # test these new SMTP settings
#' send_email_out(
#'   message = prepare_test_message(),
#'   sender = "Sender Name",
#'   subject = "test 2",
#'   from = "username@gmail.com",
#'   to = "username@gmail.com",
#'   creds_file = ".bls_smtp_gmail_com")
#' }
#' @import ggplot2
#' @export
prepare_test_message <- function() {

  # Get a nicely formatted date/time string
  current_date_time <-
    paste0(
      format(Sys.time(), "%A, %B "),
      format(Sys.time(), "%d") %>% as.numeric(),
      ", ",
      format(Sys.time(), "%Y"),
      " at ",
      format(Sys.time(), "%l:%M") %>% trimws(),
      toupper(format(Sys.time(), " %p")),
      format(Sys.time(), " (%Z)"))

  # Create a ggplot plot object
  ggplot_object <-
    ggplot(
      data = mtcars,
      aes(
        x = disp, y = hp,
        color = wt, size = mpg)) +
    geom_point()

  # Compose the email test message
  message <-
    compose_email(
      body = "
  ## Hello! This a Test Message

  It was prepared using the *blastula* R package, where you \\
  can use Markdown formatting to **embolden** text or to add \\
  *emphasis*.

  There are helpers to add things like images:

  {add_image(system.file('img', 'pexels-photo-267151.jpeg', package = 'blastula'))}

  Or even things like a ggplot:

  {add_ggplot(ggplot_object, width = 5, height = 5)}

  Cheers",
      footer = "
  Brought to you by the *blastula* R package

  Sent on {current_date_time}")

  message
}
