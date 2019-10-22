context("Sending through SMTP")

test_that("the command line string is properly constructed", {

  # Create a simple email object
  email <- compose_email(body = "test")

  # Capture the `dry_run` message output from an
  # `smtp_send()` call that specifies a non-existent
  # binary location
  dry_run_output <-
    capture.output(
      email %>%
        smtp_send(
          to = "testthat@example.com",
          from = "testthat@example.com",
          subject = "testthat test",
          credentials = creds_anonymous(
            user = "testuser@example.com",
            host = "smtp.example.com",
            port = 465,
            use_ssl = TRUE
          ),
          binary_loc = "/test/location/mailsend-go",
          echo = TRUE,
          dry_run = TRUE
        ),
      type = "message"
    )

  # Expect that a specific pattern should match the
  # third element of the `dry_run_output` vector
  dry_run_output[3] %>%
    tidy_grepl(
      paste0(
        "'/test/location/mailsend-go' -sub 'testthat test' ",
        "-smtp 'smtp.example.com' -port '465' -ssl  auth  ",
        "-user 'testuser@example.com' -from 'testthat@example.com' ",
        "-to 'testthat@example.com' body  ",
        "-file '.*?.html' -mime-type 'text/html' "
      )
    )

  # Capture the `dry_run` message output from a slightly
  # diffent `smtp_send()` (where `use_ssl = FALSE`)
  dry_run_output_2 <-
    capture.output(
      email %>%
        smtp_send(
          to = "testthat@example.com",
          from = "testthat@example.com",
          subject = "testthat test",
          credentials = creds_anonymous(
            user = "testuser@example.com",
            host = "smtp.example.com",
            port = 465,
            use_ssl = FALSE
          ),
          binary_loc = "/test/location/mailsend-go",
          echo = TRUE,
          dry_run = TRUE
        ),
      type = "message"
    )

  # Expect that a specific pattern should match the
  # third element of the `dry_run_output_2` vector
  dry_run_output_2[3] %>%
    tidy_grepl(
      paste0(
        "'/test/location/mailsend-go' -sub 'testthat test' ",
        "-smtp 'smtp.example.com' -port '465' auth  ",
        "-user 'testuser@example.com' -from 'testthat@example.com' ",
        "-to 'testthat@example.com' body  ",
        "-file '.*?.html' -mime-type 'text/html' "
      )
    )

  # Expect an error occurs if the `smtp_send()` function
  # receives input that is not an `email_message` object
  expect_error(smtp_send(email = LETTERS))

  # Ensure that the `test_creds_file` is available in a
  # temporary directory
  creds_file_path <- file.path(tempdir(), "test_creds_file")
  file.copy(testthat::test_path("test_creds_file"), creds_file_path)

  # Expect an warning if anything is provided to the `creds_file`
  # argument (which is deprecated)
  expect_warning(
    suppressMessages(
      email %>%
        smtp_send(
          to = "testthat@example.com",
          from = "testthat@example.com",
          subject = "testthat test",
          creds_file = creds_file_path,
          binary_loc = "/test/location/mailsend-go",
          dry_run = TRUE
        )
    )
  )

  # Expect that a path to a credentials file provided to the
  # `credentials` argument is silently upgraded to
  # `creds_file(file = <creds_file_path>))`, producing a
  # valid command string
  dry_run_output_3 <-
    capture.output(
      email %>%
        smtp_send(
          to = "testthat@example.com",
          from = "testthat@example.com",
          subject = "testthat test",
          credentials = creds_file_path,
          binary_loc = "/test/location/mailsend-go",
          dry_run = TRUE,
          echo = TRUE
        ),
      type = "message"
    )

  # Expect that a specific pattern should match the
  # third element of the `dry_run_output_3` vector
  dry_run_output_3[3] %>%
    tidy_grepl(
      paste0(
        "'/test/location/mailsend-go' -sub 'testthat test' ",
        "-smtp 'smtp.example.com' -port '465' -ssl  auth  ",
        "-user 'testuser@example.com' -pass 'test' ",
        "-from 'testthat@example.com' ",
        "-to 'testthat@example.com' body  ",
        "-file '.*?.html' -mime-type 'text/html' "
      )
    )

  # Expect an error if the credentials supplied are
  # not of the class `blastula_creds` (and the input
  # is not a single-length character vector)
  expect_error(
    email %>%
      smtp_send(
        to = "testthat@example.com",
        from = "testthat@example.com",
        subject = "testthat test",
        credentials = LETTERS,
        binary_loc = "/test/location/mailsend-go",
        dry_run = TRUE
      )
  )

  # Expect an error if no credentials are supplied
  expect_error(
    email %>%
      smtp_send(
        to = "testthat@example.com",
        from = "testthat@example.com",
        subject = "testthat test",
        binary_loc = "/test/location/mailsend-go",
        dry_run = TRUE
      )
  )
})
