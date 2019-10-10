context("Component Blocks")

test_that("text blocks have the correct internal contents", {

  # Create a text block
  text_block <- block_text("This is a block of text.")

  # Expect that the `text_block` object has the
  # class `block_text`
  expect_is(text_block, "block_text")

  # Expect that the input text is available in the
  # first element of the list
  expect_equal(
    text_block[[1]], "This is a block of text."
  )

  # Expect that the `text_block_template()` function
  # produces an HTML template as a table row fragment
  expect_true(
    grepl("^<tr>.*?</tr>$", text_block_template())
  )
})

test_that("title blocks have the correct internal contents", {

  # Create a title block
  title_block <- block_title("This is a title block.")

  # Expect that the `title_block` object has the
  # class `block_title`
  expect_is(title_block, "block_title")

  # Expect that the input text is available in the
  # first element of the list
  expect_equal(
    title_block[[1]], "This is a title block."
  )

  # Expect that the `title_block_template()` function
  # produces an HTML template as a table row fragment
  expect_true(
    grepl("^<tr>.*?</tr>$", title_block_template())
  )
})

test_that("spacer blocks have the correct internal contents", {

  # Create a spacer block
  spacer_block <- block_spacer()

  # Expect that the `spacer_block` object has the
  # class `block_text`
  expect_is(spacer_block, "block_spacer")

  # Expect that the static text string "&nbsp;" is
  # the first element of the list
  expect_equal(
    spacer_block[[1]], "&nbsp;"
  )

  # Expect that the `spacer_block_template()` function
  # produces an HTML template as a table row fragment
  expect_true(
    grepl("^<tr>.*?</tr>$", spacer_block_template())
  )
})

test_that("article items have the correct internal contents", {

  # Create an article item with all of the elements
  an_article <-
    article(
      image = "https://i.imgur.com/aYOm3Tk.jpg",
      title = "Japan",
      content = glue::glue(
"
Japan is an archipelago consisting \\
of 6,852 islands along East Asia's \\
Pacific Coast."
        ),
      link = "https://en.wikipedia.org/wiki/Japan"
    )

  # Expect that the `an_article` object has the
  # class `article`
  expect_is(an_article, "article")

  # Expect that the `an_article` list object has
  # four elements
  expect_equal(
    an_article %>% length(), 4
  )

  # Expect that the `an_article` list object has
  # components with particular names
  expect_equal(
    an_article %>% names(),
    c("image", "title", "content", "link")
  )

  # Expect that the `an_article` list object has
  # the text that was supplied in the `article()` call
  expect_equal(
    an_article %>% unlist() %>% unname(),
    c(
      "https://i.imgur.com/aYOm3Tk.jpg",
      "Japan",
      "Japan is an archipelago consisting of 6,852 islands along East Asia's Pacific Coast.",
      "https://en.wikipedia.org/wiki/Japan"
    )
  )

  # Expect an empty string for any argument of `article()`
  # isn't given a value
  expect_equal(
    article() %>% unlist() %>% unname(),
    rep("", 4)
  )
})

test_that("blocks have the correct internal contents", {

  # Create an article item with all of the elements
  an_article <-
    article(
      image = "https://i.imgur.com/aYOm3Tk.jpg",
      title = "Japan",
      content = glue::glue(
        "
Japan is an archipelago consisting \\
of 6,852 islands along East Asia's \\
Pacific Coast."
      ),
      link = "https://en.wikipedia.org/wiki/Japan"
    )

  # Add four different block components in a
  # `blocks()` container
  block_4 <-
    blocks(
      block_title("This is a title block."),
      block_spacer(),
      block_articles(an_article),
      block_text("This is a block of text.")
    )

  # Expect that items placed into `blocks()` will have
  # their order preserved
  expect_is(block_4[[1]], "block_title")
  expect_is(block_4[[2]], "block_spacer")
  expect_is(block_4[[3]], "block_articles")
  expect_is(block_4[[4]], "block_text")

  # Expect an error if a `blocks()` call contains nothing
  expect_error(blocks())

  # Expect that any `blocks()` object will automatically
  # insert empty text to serve as a small spacer
  expect_equal(blocks(block_articles(an_article))[[1]], " ")
  expect_is(blocks(block_articles(an_article))[[2]], "block_articles")
})
