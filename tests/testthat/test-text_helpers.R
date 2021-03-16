test_that("md() implementation is gt-compatible", {

  x <- md("hello")

  # It's important that the implementation of `md()` works exactly like this,
  # because it's possible md() can be masked by gt::md() so ideally we'd work
  # with both. It's a shame because we would really like to eagerly render the
  # markdown for blastula use cases, but this compatibility seems more
  # important.
  expect_identical(x, structure("hello", class = "from_markdown"))
})
