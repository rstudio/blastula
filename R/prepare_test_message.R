#' Prepare a email test message object
#'
#' Create an email test message object, which is helpful for sending a test
#' message with the `smtp_send()` function.
#'
#' @param incl_ggplot An option to include a ggplot plot within the body of the
#'   test message. This requires that the \pkg{ggplot2} package is installed. By
#'   default, this is `FALSE`.
#' @param incl_image An option to include a test image within the body of the
#'   test message. By default, this is `FALSE`.
#' @return An `email_message` object.
#'
#' @examples
#' # Create a credentials file to send
#' # a test message via Gmail's SMTP
#' # (this file is named "gmail_secret")
#'
#' # create_smtp_creds_file(
#' #   file = "gmail_secret",
#' #   user = "sender@email.com",
#' #   provider = "gmail"
#' # )
#'
#' # Send oneself a test message to
#' # test these new SMTP settings and
#' # to ensure that the message appears
#' # correctly in the email client
#'
#' # prepare_test_message() %>%
#' #   smtp_send(
#' #     from = "sender@email.com",
#' #     to = "sender@email.com",
#' #     subject = "Test Message",
#' #     credentials = creds_file(
#' #       file = "gmail_secret"
#' #       )
#' #     )
#'
#' @export
prepare_test_message <- function(incl_ggplot = FALSE,
                                 incl_image = FALSE) {

  # nocov start

  if (incl_ggplot) {

    # If the `ggplot2` package is available, then
    # use the `ggplot2::ggsave()` function
    if (requireNamespace("ggplot2", quietly = TRUE)) {

      # Set a seed to make the plot reproducible
      set.seed(23)

      # Create a ggplot object with `qplot()`
      ggplot_object <-
        ggplot2::qplot(
          x = stats::rnorm(1000, 150, 6.6),
          geom = "histogram",
          breaks = seq(130, 170, 2),
          colour = I("black"),
          fill = I("white"),
          xlab = "x",
          ylab = "y"
        )

      ggplot_block <- paste0(
        "We can add a ggplot plot with `add_ggplot()`:\n\n",
        add_ggplot(ggplot_object), "\n", collapse = "")

    } else {

      stop("Please ensure that the `ggplot2` package is installed before ",
           "using `add_ggplot()`.",
           call. = FALSE)
    }
  }

  if (incl_image) {

    image_include <-
      add_image(
        system.file(
          'img', 'pexels-photo-267151.jpeg',
          package = 'blastula'))

    image_block <- paste0(
      "There are helpers to add things like images:\n\n",
      image_include, "\n", collapse = "")
  }

  # Compose the email test message
  message <-
    compose_email(
      body = "
  ## This a Test Message

  This message was prepared using the *blastula* R package, where \\
  you can use Markdown formatting to **embolden** text or to add \\
  *emphasis*.

  {ifelse(incl_image, image_block, '')}
  {ifelse(incl_ggplot, ggplot_block, '')}
  ",
      footer = "
  Brought to you by the *blastula* R package
      ")

  message

  # nocov end
}
