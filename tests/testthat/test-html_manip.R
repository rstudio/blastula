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

  cid_output <- cid_images("url_encoding.html")
  expect_identical(
    attr(cid_output$images$img1, "content_type", exact = TRUE),
    "image/png"
  )
})

# TODO: Test for proper handling of file:// URLs

test_that("WARNING: duplicate attribs are not supported correctly", {
  output <- replace_attr("<h1\n  class='headline' class='disabled'>", "h1", "class", function(class) {
    NULL
  })

  expect_identical(output, "<h1\n  class='' class='disabled'>")
})

test_that("File URI parsing works correctly", {
  # Illegal inputs
  expect_error(file_uri_to_filepath("http://example.com"))
  expect_error(file_uri_to_filepath("logo.gif"))
  expect_error(file_uri_to_filepath("file:foobar"))
  expect_error(file_uri_to_filepath(""))

  expect_identical(file_uri_to_filepath("file:///foo/bar"), "/foo/bar")
  expect_identical(file_uri_to_filepath("FILE:///foo/bar"), "/foo/bar")
  expect_identical(file_uri_to_filepath("file:///foo/bar/"), "/foo/bar/")
  expect_error(file_uri_to_filepath("file://localhost/foo/bar"))
  expect_identical(file_uri_to_filepath("file://C:/foo/bar"), "C:/foo/bar")
  expect_identical(file_uri_to_filepath("file://C:/foo%20bar"), "C:/foo bar")
  # Escaping not allowed outside of the path
  expect_error(file_uri_to_filepath("file%3a//C:/foo%20bar"))
  # + is interpreted as " " in query strings, but not in URL paths
  expect_identical(file_uri_to_filepath("file://C:/foo+bar"), "C:/foo+bar")
})

test_that("src resolution works correctly", {
  expect_equal(src_to_filepath("foo%20bar", "/baz"), "/baz/foo bar")
  expect_equal(src_to_filepath("/foo%20bar", "/baz"), "/foo bar")
  expect_equal(src_to_filepath("/foo%20bar", "/baz"), "/foo bar")
  expect_equal(src_to_filepath("/foo%20bar", "."), "/foo bar")
  expect_equal(src_to_filepath("foo%20bar", "."), file.path(getwd(), "foo bar"))
  expect_equal(src_to_filepath("../a/b", "/c/d"), "/c/a/b")

  expect_equal(src_to_filepath("C:\\foo\\bar", "/baz"), "C:/foo/bar")
  # Newer versions of fs capitalize drive letters
  expect_true(src_to_filepath("foo/bar", "c:\\baz") %in% c("c:/baz/foo/bar", "C:/baz/foo/bar"))
  expect_true(src_to_filepath("", "c:\\baz") %in% c("c:/baz", "C:/baz"))
  expect_equal(src_to_filepath("foo", ""), file.path(getwd(), "foo"))
})
