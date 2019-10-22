context("Getting a readable time")

test_that("the `add_readable_time()` function returns the expected output", {

  posix_time <-
    ISOdatetime(
      year = 2019,
      month = 6,
      day = 13,
      hour = 15,
      min = 36,
      sec = 5.323,
      tz = "America/Toronto"
    )

  # Expect that, with all default options, a formatted date and
  # time is provided as a string
  add_readable_time(time = posix_time) %>%
    expect_equal("Thursday, June 13, 2019 at 3:36 PM (EDT)")

  # Expect only the time and time zone if `use_date` is `FALSE`
  add_readable_time(time = posix_time, use_date = FALSE) %>%
    expect_equal("3:36 PM (EDT)")

  # Expect only the date and time zone if `use_time` is `FALSE`
  add_readable_time(time = posix_time, use_time = FALSE) %>%
    expect_equal("Thursday, June 13, 2019 (EDT)")

  # Expect no time zone if `use_tz` is `FALSE`
  add_readable_time(time = posix_time, use_tz = FALSE) %>%
    expect_equal("Thursday, June 13, 2019 at 3:36 PM")

  # Expect only the date if `use_time` and `use_tz` are `FALSE`
  add_readable_time(time = posix_time, use_time = FALSE, use_tz = FALSE) %>%
    expect_equal("Thursday, June 13, 2019")

  # Expect only the time if `use_date` and `use_tz` are `FALSE`
  add_readable_time(time = posix_time, use_date = FALSE, use_tz = FALSE) %>%
    expect_equal("3:36 PM")

  # Expect only the time zone if `use_date` and `use_time` are `FALSE`
  add_readable_time(time = posix_time, use_date = FALSE, use_time = FALSE) %>%
    expect_equal("EDT")

  # Expect an empty string if all options are `FALSE`
  add_readable_time(
    time = posix_time,
    use_date = FALSE,
    use_time = FALSE,
    use_tz = FALSE
  ) %>%
    expect_equal("")

  # Expect that no `time` provided results in a formatting
  # of the date-time from `Sys.time()`
  add_readable_time() %>%
    expect_match(".*?, .*?, \\d{4} at \\d*?:\\d*? (AM|PM) (.*?)")

  # Expect an error if the object provided to `time` isn't
  # of the class `"POSIXct"`
  expect_error(add_readable_time(time = 252922))
})
