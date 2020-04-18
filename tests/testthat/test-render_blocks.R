context("Rendering HTML blocks")

test_that("the `render_block_title()` function returns the expected output", {

  # Create a title block
  block_title <- blocks(block_title("Test Title"))

  snapshot(block_title)
})

test_that("the `render_block_text()` function returns the expected output", {

  # Create a text block
  block_text <- blocks(block_text("Test Text"))

  snapshot(block_text)
})

test_that("the `render_block_articles()` function returns the expected output", {

  # Create an articles block with one article
  block_articles_1 <-
    block_articles(
      article(
        title = "title_1",
        content = "content_1",
        link = "link_1"
      )
    )

  # Create an articles block with two articles
  block_articles_2 <-
    block_articles(
      article(
        title = "title_1",
        content = "content_1",
        link = "link_1"
      ),
      article(
        title = "title_2",
        content = "content_2",
        link = "link_2"
      )
    )

  # Create an articles block with three articles
  block_articles_3 <-
    block_articles(
      article(
        title = "title_1",
        content = "content_1",
        link = "link_1"
      ),
      article(
        title = "title_2",
        content = "content_2",
        link = "link_2"
      ),
      article(
        title = "title_3",
        content = "content_3",
        link = "link_3"
      )
    )

  snapshot(block_articles_1)
  snapshot(block_articles_2)
  snapshot(block_articles_3)

  # Expect an error if all objects provided to
  # `block_articles()` are not of the class `article`
  expect_error(
    block_articles(
      article(
        title = "title_1",
        content = "content_1",
        link = "link_1"
      ),
      article(
        title = "title_2",
        content = "content_2",
        link = "link_2"
      ),
      LETTERS
    )
  )
})

test_that("the social link functions work correctly", {

  social_link_1 <-
    social_link(service = "Dribbble", link = "https://dribbble.com")

  snapshot(social_link_1)

  # Expect that the `social_service_icons()` function
  # returns a tibble
  social_service_icons() %>%
    expect_s3_class("tbl_df")

  social_service_icons() %>%
    ncol() %>%
    expect_equal(2)

  social_service_icons() %>%
    nrow() %>%
    expect_equal(29)

  social_service_icon_variants %>%
    expect_equal(c("color", "bw", "dark_gray", "gray", "light_gray"))
})
