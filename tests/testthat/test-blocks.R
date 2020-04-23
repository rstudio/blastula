context("Component Blocks")


test_that("spacer blocks have the correct internal contents", {

  # Create a spacer block
  spacer_block <- block_spacer()

  snapshot(spacer_block)
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

  snapshot(an_article)
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
  snapshot(block_4)
})
