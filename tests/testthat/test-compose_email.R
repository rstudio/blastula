test_that("composing a simple message is possible", {

  # Create a simple email message with no text
  email <- compose_email()

  # Expect an object of class `email_message` and type of 'list'
  expect_s3_class(email, "email_message")
  expect_type(email, "list")

  # Expect the object to be a list of length 3
  expect_equal(length(email), 4)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html", "attachments", "images")
  )

  # Expect that the first two list components
  # are of the class `character`
  expect_type(email$html_str, "character")
  expect_type(email$html_html, "character")

  # Expect that the third list component is a `list`
  expect_type(email$attachments, "list")
})

test_that("email components appear in the HTML message", {

  # Create a simple email message with text in
  # the body component
  email <- compose_email(body = "test_text_in_body")

  # Expect that the test string is available in the HTML
  expect_true(grepl("test_text_in_body", email$html_str))

  # Create a simple email message with text in
  # the header component
  email <- compose_email(header = "test_text_in_header")

  # Expect that the test string is available in the HTML
  expect_true(grepl("test_text_in_header", email$html_str))

  # Create a simple email message with text in
  # the footer component
  email <- compose_email(header = "test_text_in_footer")

  # Expect that the test string is available in the HTML
  expect_true(grepl("test_text_in_footer", email$html_str))

  # Create a simple email message with text in all
  # three components (header, body, and footer)
  email <-
    compose_email(
        body = "test_text_in_body",
      header = "test_text_in_header",
      footer = "test_text_in_footer"
    )

  # Expect that all three strings are available in the HTML
  expect_true(
    all(
      c(
        grepl("test_text_in_body", email$html_str),
        grepl("test_text_in_header", email$html_str),
        grepl("test_text_in_footer", email$html_str)
      )
    )
  )

  # Create a simple email message with text in
  # the body component and a title
  email <-
    compose_email(
      body = "test_text_in_body",
      title = "email_title"
    )

  # Expect that the title string appears within <title>
  # tags in the HTML
  expect_true(grepl("<title>email_title</title>", email$html_str))

  # Use blocks (`block_text()`) within the body, header,
  # and footer components
  email <-
    compose_email(
        body = blocks(block_text("test_text_in_body_block")),
      header = blocks(block_text("test_text_in_header_block")),
      footer = blocks(block_text("test_text_in_footer_block"))
    )

  # Expect that all three strings are available in the HTML
  expect_true(
    all(
      c(
        grepl("test_text_in_body_block", email$html_str),
        grepl("test_text_in_header_block", email$html_str),
        grepl("test_text_in_footer_block", email$html_str)
      )
    )
  )
})

test_that("composing a message with local inline images is possible", {

  library(ggplot2)

  # Create a ggplot plot
  plot <-
    ggplot(data = mtcars, aes(x = disp, y = hp, color = wt, size = mpg)) +
    geom_point()

  # Convert the plot to an HTML fragment
  plot_html <-
    add_ggplot(
      plot,
      height = 5, width = 7
    )

  body_input <-
    glue::glue(
"
Here is a plot:

{plot_html}
"
) %>% as.character()

  # Create a simple email message with no text
  email <- compose_email(body = md(body_input))

  # Expect an object of class `email_message` and type of 'list'
  expect_s3_class(email, "email_message")
  expect_type(email, "list")

  # Expect the object to be a list of length 4
  expect_equal(length(email), 4)

  # Expect specific names for each of
  # the list components
  expect_equal(
    names(email),
    c("html_str", "html_html", "attachments", "images")
  )

  # Expect that the first two list components
  # are of the class `character`
  expect_type(email$html_str, "character")
  expect_type(email$html_html, "character")

  # Expect that the third list component
  # is of the class `list`
  expect_type(email$attachments, "list")

  # Expect that the fourth list component
  # is of the class `list`
  expect_type(email$images, "list")

  # Expect that the inner component of
  # `email$images` is of the class `character`
  expect_type(email$images[[1]], "character")
})
