context("String manipulation utilities")

test_that("substr2 edge cases", {

  expect_identical(substr2("1234567890", 1, 1), "")
  expect_identical(substr2("1234567890", 0, 0), "")
  expect_identical(substr2("1234567890", 99, 100), "")

  # end position is off the end
  expect_identical(substr2("1234567890", 9, 100), "90")

  # passing both start and end positions in one argument
  expect_identical(substr2("1234567890", c(1,3)), "12")

  # vectorized start/end
  expect_identical(substr2("1234567890", c(1,3), c(2,4)), c("1", "3"))

  # end recycling
  expect_identical(substr2("1234567890", c(1,2), 3), c("12", "2"))

  # too-small start value is tolerated
  expect_identical(substr2("1234567890", -3, 2), "1")

  # multiple values for `x` are not tolerated
  expect_error(substr2(c("abc", "def"), 1, 3))
})

