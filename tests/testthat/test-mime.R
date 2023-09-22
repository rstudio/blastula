test_that("`format_rfc2822_date()` produces valid dates", {

  # Expect properly formatted POSIXct date-times with varying time zones
  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "Europe/Paris")) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 +0200")

  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "America/Toronto")) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 -0400")

  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "GMT")) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 +0000")

  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "GMT0")) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 +0000")

  # Expect that day values don't have a leading `0`
  format_rfc2822_date(
    ISOdatetime(2015, 6, 2, 20, 18, 00, tz = "America/Los_Angeles")) %>%
    expect_equal("Tue, 2 Jun 2015 20:18:00 -0700")

  # Expect properly formatted POSIXlt date-times with varying time zones
  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "Europe/Paris") %>% as.POSIXlt()
  ) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 +0200")

  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "GMT0") %>% as.POSIXlt()
  ) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 +0000")

  format_rfc2822_date(
    ISOdatetime(2019, 6, 15, 20, 18, 00, tz = "America/Toronto") %>% as.POSIXlt()
  ) %>%
    expect_equal("Sat, 15 Jun 2019 20:18:00 -0400")
})

test_that("varying formats for recipient lists work as expected", {

  # Named vs. unnamed recipients (or some named and some unnamed)
  #   Recipients with Unicode characters and double-quotes in their display names
  # Single vs. multiple recipients

  # Create an empty email message
  email <- compose_email()

  email_date <- ISOdatetime(2015, 6, 2, 20, 18, 00, tz = "America/Los_Angeles")

  # Test resulting MIME messages with variations in email address lists
  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("receive_1@example.com")
  ) %>%
    expect_match("To: <receive_1@example.com>")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("Receiver" = "receive_1@example.com")
  ) %>%
    expect_match("To: \"Receiver\" <receive_1@example.com>")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("receive_1@example.com", "receive_2@example.com")
  ) %>%
    expect_match("To: <receive_1@example.com>, <receive_2@example.com>")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("receive_1@example.com", "Receiver" = "receive_2@example.com")
  ) %>%
    expect_match("To: <receive_1@example.com>, \"Receiver\" <receive_2@example.com>")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("Receiv\U00E8r" = "receive_1@example.com")
  ) %>%
    expect_match("To: =\\?utf-8\\?B\\?UmVjZWl2w6hy\\?= <receive_1@example.com>")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "test",
    from = "sender@example.com",
    to = c("The \"Real\" Receiver" = "receive_1@example.com")
  ) %>%
    expect_match("To: \"The \\\\\"Real\\\\\" Receiver\" <receive_1@example.com>")

  # Test the subject line with several variations
  # (Unicode characters, double-quotes)
  # generate_rfc2822(
  #   eml = email,
  #   date = email_date,
  #   subject = "Don't sw\U00E8at it",
  #   from = "sender@example.com",
  #   to = "receive_1@example.com"
  # ) %>%
  #   expect_match("Subject: =\\?utf-8\\?B\\?RG9uJ3Qgc3fguop0IGl0\\?=")

  generate_rfc2822(
    eml = email,
    date = email_date,
    subject = "Don't \"sweat it\", okay?",
    from = "sender@example.com",
    to = "receive_1@example.com"
  ) %>%
    expect_match("Subject: Don't \"sweat it\", okay?")

  # TODO:
  # Inline images and file attachments with weird filenames
  #   Filenames with spaces
  #   Filenames with unicode characters (on both Windows and POSIX)
  #   Filenames with angle brackets < >
  #   With various extensions

  # Create a test email with text lines > 80 characters and a
  # small binary attachment (an image); after calling
  # `generate_rfc2822()` on it, expect that `grepl("(?<!\\r)\n", str, perl = TRUE)`
  # returns FALSE
  blastula::prepare_test_message(
    incl_ggplot = FALSE, incl_image = FALSE
  ) %>%
    add_attachment(file = system.file("img", "pexels-photo-267151.jpeg", package = "blastula")) %>%
    generate_rfc2822() %>%
    grepl("(?<!\\r)\n", ., perl = TRUE) %>%
    expect_false()
})
