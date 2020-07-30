context("SMTP send")

test_that("Email is of expected format", {
  # Illegal inputs
  expect_error(smtp_send("hello"))
})

test_that("Creds File Deprecation", {

  email <- compose_email("email")

  cf <- creds_file(
    file = "test_creds_file"
  )

  expect_error(smtp_send(email=email, from = "sender@email.com",to = "recipient@email.com",
                         creds_file = cf))
})

test_that("Credentials is NULL", {
  email <- compose_email("email")
  expect_error(smtp_send(email, from = "sender@email.com",to = "recipient@email.com", credentials=NULL))
})


test_that("Credentials is Blastula class", {

  expect_error(smtp_send(from = "sender@email.com",to = "recipient@email.com",
                         "hello"
  ))
})
