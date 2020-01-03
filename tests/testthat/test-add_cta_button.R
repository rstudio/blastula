context("Getting a CTA button")

test_that("the `add_cta_button()` function returns the expected output", {

  cta <-
    add_cta_button(
      url = "https://www.example.com",
      text = "An example website",
      align = "left"
    )

  # Expect that the resulting HTML string fits a particular pattern
  cta %>%
    expect_match(
      paste0(
        "^\n<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" ",
        "class=\"btn btn-primary\" style=\"border-collapse: separate; ",
        "mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; ",
        "box-sizing: border-box;\">\n<tbody>\n<tr>\n<td align=\"left\" ",
        "style=\"font-family: sans-serif; font-size: 14px; ",
        "vertical-align: top; padding-bottom: 15px;\">.*?",
        "<a href=\"https://www.example.com\" target=\"_blank\".*?",
        "font-size: 14px; font-weight: bold; margin: 0; padding: 12px 25px;.*?",
        "An example website.*"
      )
    )
})
