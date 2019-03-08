context("Message Composition")

test_that("composing a simple message is possible", {

  # Create a simple email message with no text
  email <- compose_email()

  # Expect an object of class `email_message`
  expect_is(
    object = email,
    class = "email_message")

  # Expect the object to be a list of length 3
  expect_equal(
    length(email),
    3)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html", "attachments")
  )

  # Expect that the first two list components
  # are of the class `character`
  expect_is(
    object = email$html_str,
    class = "character")

  expect_is(
    object = email$html_html,
    class = "character")

  # Expect that the third list component
  # is of the class `list`
  expect_is(
    object = email$attachments,
    class = "list")
})


test_that("composing a message with local inline images is possible", {

  library(ggplot2)

  # Create a ggplot plot
  plot <-
    ggplot(
      data = mtcars,
      aes(x = disp, y = hp,
          color = wt, size = mpg)) +
    geom_point()

  # Convert the plot to an HTML fragment
  plot_html <-
    add_ggplot(
      plot,
      height = 5, width = 7)

  # Create a simple email message with no text
  email <-
    compose_email(body = "
  Here is a plot:

  {plot_html}
  ")

  # Expect an object of class `email_message`
  expect_is(
    object = email,
    class = "email_message")

  # Expect the object to be a list of length 4
  expect_equal(
    length(email),
    4)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html", "attachments", "images")
  )

  # Expect that the first two list components
  # are of the class `character`
  expect_is(
    object = email$html_str,
    class = "character")

  expect_is(
    object = email$html_html,
    class = "character")

  # Expect that the third list component
  # is of the class `list`
  expect_is(
    object = email$attachments,
    class = "list")

  # Expect that the fourth list component
  # is of the class `list`
  expect_is(
    object = email$images,
    class = "list")

  # Expect that the inner component of
  # `email$images` is of the class `character`
  expect_is(
    object = email$images[[1]],
    class = "character")
})
