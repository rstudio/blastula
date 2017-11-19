context("Helper Functions")

test_that("creating a CTA button HTML fragment is possible", {

  # Create a CTA button HTML fragment
  cta_button_html <-
    add_cta_button(
      url = "http://www.test.net",
      text = "button text",
      align = "center")

  # Expect an HTML fragment with the
  # supplied values within
  expect_equal(
    as.character(cta_button_html),
    "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" class=\"btn btn-primary\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%; box-sizing: border-box;\">\n<tbody>\n<tr>\n<td align=\"center\" style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; padding-bottom: 15px;\">\n<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: auto;\">\n<tbody>\n<tr>\n<td style=\"font-family: sans-serif; font-size: 14px; vertical-align: top; background-color: #3498db; border-radius: 5px; text-align: center;\"> <a href=\"http://www.test.net\" target=\"_blank\" style=\"display: inline-block; color: #ffffff; background-color: #3498db; border: solid 1px #3498db; border-radius: 5px; box-sizing: border-box; cursor: pointer; text-decoration: none; font-size: 14px; font-weight: bold; margin: 0; padding: 12px 25px; text-transform: capitalize; border-color: #3498db;\">button text</a> </td>\n</tr>\n</tbody>\n</table>\n</td>\n</tr>\n</tbody>\n</table>")

})

test_that("creating a base64-encoded image is possible", {

  # Create an HTML fragment that
  # contains an image
  img_file_path <-
    system.file(
      "graphics",
      "melon_cat.png",
      package = "blastula")

  img_file_html <-
    add_image(
      file = img_file_path)

  # Expect a base64 PNG within `img` tags
  expect_true(
    grepl("<img src=\"data:image/png;base64,.* />", img_file_html))
})

test_that("creating a base64-encoded ggplot is possible", {

  library(ggplot2)

  # Create a ggplot plot
  plot <-
    ggplot(
      data = mtcars,
      aes(x = disp, y = hp,
          color = wt, size = mpg)) +
    geom_point()

  # Create an HTML fragment that
  # contains an the ggplot as an
  # embedded plot
  plot_html <-
    add_ggplot(
      plot_object = plot,
      height = 5,
      width = 7)

  # Expect a base64 PNG within `img` tags
  expect_true(
    grepl("<img src=\"data:image/png;base64,.* />", plot_html))
})
