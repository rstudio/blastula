context("Getting a CTA button")

test_that("the `add_cta_button()` function returns the expected output", {

  cta <-
    add_cta_button(
      url = "https://www.example.com",
      text = "An example website",
      align = "left"
    )

  snapshot(cta)
})
