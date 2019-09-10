test_that("knitr_sidecar_prefix preconditions hold", {

  # Ensure that rmarkdown assumptions still hold
  rmd_path <- file.path(tempdir(), "test-utils.Rmd")

  file.copy(testthat::test_path("test-utils.Rmd"), rmd_path)

  rmarkdown::render(rmd_path, quiet = TRUE)
})

test_that("knitr_sidecar_prefix behavior", {

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

})
