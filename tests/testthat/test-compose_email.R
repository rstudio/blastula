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
