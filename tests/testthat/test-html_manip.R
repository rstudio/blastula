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
