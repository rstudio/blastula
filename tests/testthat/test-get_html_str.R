context("Extracting the HTML message string")

test_that("the `get_html_str()` function returns the expected output", {

  # Create a simple email object
  email <- compose_email(body = "test")

  # Expect that the extracted HTML message body
  # conforms to a regex pattern
  email %>%
    get_html_str() %>%
    expect_match("^<!doctype html>\n<html>\n.*test.*</html>$")
})
