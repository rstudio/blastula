context("Message Composition")

test_that("composing a simple message is possible", {

  # Create a simple email message with no text
  email <- compose_email()

  # Expect an object of class `email_message`
  expect_is(
    object = email,
    class = "email_message")

  # Expect the object to be a list of length 2
  expect_equal(
    length(email),
    2)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html"))

  # Expect that each of the list components
  # is of class `character`
  expect_is(
    object = email[[1]],
    class = "character")

  expect_is(
    object = email[[2]],
    class = "character")
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

  # Expect the object to be a list of length 3
  expect_equal(
    length(email),
    3)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html", "images"))

  # Expect that the first two components
  # are of class `character`
  expect_is(
    object = email[[1]],
    class = "character")

  expect_is(
    object = email[[2]],
    class = "character")


  expect_is(
    object = email[[3]],
    class = "list")

  expect_is(
    object = email[[3]][[1]],
    class = "character")
})
