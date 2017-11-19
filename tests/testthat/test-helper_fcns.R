context("Helper Functions")

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
