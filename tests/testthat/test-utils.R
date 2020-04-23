context("Utility functions work as expected")

test_that("knitr_sidecar_prefix preconditions hold", {

  if (rmarkdown::pandoc_available()) {

    # Ensure that rmarkdown assumptions still hold
    rmd_path <- file.path(tempdir(), "test-utils.Rmd")

    file.copy(testthat::test_path("test-utils.Rmd"), rmd_path)

    rmarkdown::render(rmd_path, quiet = TRUE)
  }
})

test_that("knitr_sidecar_prefix behavior", {

  if (rmarkdown::pandoc_available()) {

    # fig.path isn't set
    expect_null(knitr_sidecar_prefix(NULL))
    expect_null(knitr_sidecar_prefix(NULL, condition = FALSE))
    expect_null(knitr_sidecar_prefix(NULL, condition = TRUE))
    expect_identical("default", knitr_sidecar_prefix("default"))
    expect_identical("default", knitr_sidecar_prefix("default", condition = FALSE))
    expect_identical("default", knitr_sidecar_prefix("default", condition = TRUE))

    # Explicitly provide an unusable fig.path
    expect_null(knitr_sidecar_prefix(NULL, fig_path = "."))
    expect_null(knitr_sidecar_prefix(NULL, condition = FALSE, fig_path = "."))
    expect_null(knitr_sidecar_prefix(NULL, condition = TRUE, fig_path = "."))
    expect_identical("default", knitr_sidecar_prefix("default", fig_path = "."))
    expect_identical("default", knitr_sidecar_prefix("default", condition = FALSE, fig_path = "."))
    expect_identical("default", knitr_sidecar_prefix("default", condition = TRUE, fig_path = "."))

    # Usable fig.path provided
    expect_identical(
      knitr_sidecar_prefix(NULL, condition = TRUE,
                           fig_path = "a12 !_-x_files/b_files/figure-github_flavored_markdown/"),
      "a12 !_-x_files/b"
    )
    expect_null(
      knitr_sidecar_prefix(NULL, condition = FALSE,
                           fig_path = "a12 !_-x_files/b_files/figure-github_flavored_markdown/")
    )
    expect_null(
      knitr_sidecar_prefix(NULL,
                           fig_path = "a12 !_-x_files/b_files/figure-github_flavored_markdown/")
    )
  }
})

test_that("the `smtp_settings()` function returns the expected output", {

  # Expect that the table is a `tbl_df`
  smtp_settings() %>% expect_s3_class("tbl_df")

  # Expect 3 rows in the table
  smtp_settings() %>% nrow() %>% expect_equal(3)

  # Expect 8 columns in the table
  smtp_settings() %>% ncol() %>% expect_equal(6)
})

test_that("the `get_provider_list()` function returns the expected output", {

  # Expect that the provider list vector contains
  # three items
  get_provider_list() %>%
    expect_equal(c("gmail", "outlook", "office365"))
})


test_that("the `get_smtp_provider_values()` function returns the expected output", {

  # Expect that the `get_smtp_provider_values()` function
  # returns a list of values for a provider
  get_smtp_provider_values(provider = "gmail") %>%
    expect_type("list")

  # Expect the same set of names for each provider

  get_smtp_provider_values(provider = "gmail") %>%
    names() %>%
    expect_equal(
      c("short_name", "server", "port", "use_ssl", "user", "long_name")
    )

  get_smtp_provider_values(provider = "outlook") %>%
    names() %>%
    expect_equal(
      c("short_name", "server", "port", "use_ssl", "user", "long_name")
    )

  get_smtp_provider_values(provider = "office365") %>%
    names() %>%
    expect_equal(
      c("short_name", "server", "port", "use_ssl", "user", "long_name")
    )
})
