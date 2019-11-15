context("Creating credentials")

test_that("utility functions for credentials work properly", {

  # Create a credentials list with no `provider` specified
  credentials_list_1 <-
    create_credentials_list(
      provider = NULL,
      user = "testuser@example.com",
      password = "testpass",
      host = "smtp.example.com",
      port = 465,
      use_ssl = TRUE
    )

  # Expect that a list is created
  credentials_list_1 %>% expect_type("list")

  # Expect certain names in the list object
  credentials_list_1 %>%
    names() %>%
    expect_equal(
      c("version", "host", "port", "use_ssl", "user", "password")
    )

  # Expect a specific value for `credentials_list_1$version`
  credentials_list_1$version %>% expect_equal(schema_version)


  # Create a credentials list with the `provider` of `gmail`
  credentials_list_2 <-
    create_credentials_list(
      provider = "gmail",
      user = "testuser@example.com",
      password = "testpass",
      host = NULL,
      port = NULL,
      use_ssl = NULL
    )

  # Expect certain names in the list object
  credentials_list_2 %>%
    names() %>%
    expect_equal(
      c("version", "host", "port", "use_ssl", "user", "password")
    )

  # Expect that the `host`, `port`, and `use_ssl` elements
  # have values
  credentials_list_2$host %>% expect_equal("smtp.gmail.com")
  credentials_list_2$port %>% expect_equal(465)
  credentials_list_2$use_ssl %>% expect_true()

  # Expect an error if `provider` is NULL and any of `host`,
  # `port`, or `use_ssl` are NULL
  expect_error(
    create_credentials_list(
      provider = NULL,
      user = "testuser@example.com",
      password = "testpass",
      host = NULL,
      port = NULL,
      use_ssl = NULL
    )
  )

  # Expect that the `validate_smtp_provider()` function will
  # not result in an error if email providers in the provider
  # list are used
  expect_silent(validate_smtp_provider(provider = "gmail"))
  expect_silent(validate_smtp_provider(provider = "outlook"))
  expect_silent(validate_smtp_provider(provider = "office365"))

  # Expect an error if a provider not in this list is used
  expect_error(validate_smtp_provider(provider = "hotmail"))

  # Expect that the `JSONify_credentials()` function creates
  # a JSON string with a credentials list
  credentials_list_1_json <-
    JSONify_credentials(credentials_list = credentials_list_1)

  # Expect that the JSON object is a character vector of length 1
  credentials_list_1_json %>% expect_type("character")
  credentials_list_1_json %>% length() %>% expect_equal(1)

  # Expect that unserializing with `jsonlite::unserializeJSON()`
  # produces the original credentials list
  credentials_list_1_json %>%
    jsonlite::unserializeJSON() %>%
    expect_equivalent(credentials_list_1)
})
