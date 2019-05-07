context("HTML manipulation utilities")

test_that("HTML unescaping is properly performed", {
  replace_attr("<img src='foo&#39;&xlarr;&BadEntity;bar.png'>", "img", "src", function(src) {
    expect_identical(src, "foo'\u27F5&BadEntity;bar.png")
    src
  })
})

test_that("HTML escaping is properly performed", {
  output <- replace_attr("<div title='foo.png'>", "div", "title", function(src) {
    "special chars '\"<>&"
  })

  expect_identical(output, "<div title='special chars &#39;&quot;&lt;&gt;&amp;'>")
})

test_that("Newlines between attributes are OK", {
  output <- replace_attr("<h1\n\tclass='headline'>", "h1", "class", function(class) {
    NULL
  })

  expect_identical(output, "<h1\n\tclass=''>")
})

test_that("URL encoding is correctly decoded", {
  output <- inline_images("url_encoding.html")
  expect_true(grepl("data:image/png", output))
})

# TODO: Test for proper handling of file:// URLs

test_that("WARNING: duplicate attribs are not supported correctly", {
  output <- replace_attr("<h1\n  class='headline' class='disabled'>", "h1", "class", function(class) {
    NULL
  })

  expect_identical(output, "<h1\n  class='' class='disabled'>")
})
