test_that("The `create_rmd_preview_message()` function works properly", {

  # Create an HTML string for the .Rmd preview message
  rmd_preview_html <-
    create_rmd_preview_message(subject = "Connect test message")

  # Expect a certain pattern for the HTML string
  rmd_preview_html %>%
    expect_match(
      paste0(
        "^<div style=\"text-align: center; background:#fcfcfc\">",
        "<h2 style=\"margin-bottom: 0; padding-bottom: 0;\">",
        "This is an email preview for RStudio Connect</h2>",
        "<p style=\"text-align: center; background:#fcfcfc; padding-top: 0;.*?",
        "</span></strong>Connect test message<br></p><hr></div>$"
      )
    )
})
