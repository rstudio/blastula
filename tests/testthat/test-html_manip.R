context("HTML manipulation utilities")

test_that("HTML unescaping is properly performed", {
  replace_attr("<img src='foo&#39;&xlarr;&BadEntity;bar.png'>", "img", "src", function(src) {
    expect_identical(src, "foo'\u27F5&BadEntity;bar.png")
    src
  })

  # Tests that native encoding is parsed correctly as well
  times_called <- 0L
  replace_attr(enc2native("<img src='Á.png'/><img src='&Aacute;.png'/>"), "img", "src", function(src) {
    expect_identical(Encoding(src), "UTF-8")
    expect_identical(src, "Á.png")
    times_called <<- times_called + 1L
  })
  expect_identical(times_called, 2L)
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

  # Because of the potential for drive letters to be of
  # different cases, we transform the whole string to lower case
  expect_equal(tolower(src_to_filepath("foo%20bar", ".")), tolower(file.path(getwd(), "foo bar")))
  expect_equal(tolower(src_to_filepath("foo", "")), tolower(file.path(getwd(), "foo")))

  expect_equal(src_to_filepath("../a/b", "/c/d"), "/c/a/b")
  expect_equal(src_to_filepath("C:\\foo\\bar", "/baz"), "C:/foo/bar")
  # Newer versions of fs capitalize drive letters
  expect_true(src_to_filepath("foo/bar", "c:\\baz") %in% c("c:/baz/foo/bar", "C:/baz/foo/bar"))
  expect_true(src_to_filepath("", "c:\\baz") %in% c("c:/baz", "C:/baz"))
})

test_that("decode_hex works correctly including for Unicode chars", {
  expect_identical(html_unescape("&#x51;"), "Q")
  expect_identical(html_unescape("&#81;"), "Q")
  expect_identical(html_unescape("&#x2661;"), "\u2661")
  expect_identical(html_unescape("&#9825;"), "\u2661")
  expect_identical(html_unescape("&#x0010FFFF;"), "\U0010FFFF")
  expect_identical(html_unescape("&#1114111;"), "\U0010FFFF")

  # Error: Too many hex digits
  expect_error(html_unescape("&#x0010FFFFF;"))
  expect_error(html_unescape("&#x0010FFFFF;"))
})

test_that("gfsub doesn't butcher line endings", {
  expect_identical(
    gfsub("a\nb\r\nc", "[\\w]", toupper),
    toupper("a\nb\r\nc")
  )
})

test_that("duplicate images are not attached multiple times", {
  img <- add_image(system.file(package = "blastula", "img/pexels-photo-267151.jpeg"))
  email <- compose_email(body = list(img, img))
  expect_identical(length(email$images), 1L)
})
