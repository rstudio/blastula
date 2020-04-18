context("Helper Functions")

test_that("creating a base64-encoded image is possible", {

  # Create an HTML fragment that
  # contains an image
  img_file_path <-
    system.file(
      "example_files",
      "test_image.png",
      package = "blastula"
    )

  # Create an image as an <img> tag
  img_file_html <- add_image(file = img_file_path)

  snapshot(img_file_html)

  # Expect a base64 PNG within `img` tags
  expect_true(
    grepl("<img src=\"data:image/png;base64,.*\" .*?/>", img_file_html)
  )

  # Create an image as an <img> tag, with alt text
  img_file_html2 <- add_image(file = img_file_path, alt = "A test image")

  snapshot(img_file_html2)

  # Expect a base64 PNG within `img` tags
  # and the specified alt text
  expect_true(
    grepl("<img src=\"data:image/png;base64,.*\".*alt=\"A test image\".*/>", img_file_html2)
  )
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
    grepl("<img src=\"data:image/png;base64,.*\" .*?/>", plot_html))
})
