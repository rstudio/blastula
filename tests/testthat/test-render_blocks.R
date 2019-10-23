context("Rendering HTML blocks")

test_that("the `render_block_title()` function returns the expected output", {

  # Create a title block
  block_title <- blocks(block_title("Test Title"))

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `body`
  block_title[[1]] %>%
    render_block_title(context = "body") %>%
    expect_match(
      paste0(
      "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
      "<h1 class=\"align-center\" style=\"color: #222222; ",
      "font-family: Helvetica, sans-serif; font-weight: 300; ",
      "line-height: 1.4; margin: 0; font-size: 36px; ",
      "margin-bottom: 4px; text-transform: capitalize; ",
      "text-align: center;\">Test Title</h1>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `header`
  block_title[[1]] %>%
    render_block_title(context = "header") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<h1 class=\"align-center\" style=\"color: #999999; ",
        "font-family: Helvetica, sans-serif; font-weight: 300; ",
        "line-height: 1.4; margin: 0; font-size: 20px; ",
        "margin-bottom: 0px; text-transform: capitalize; ",
        "text-align: center;\">Test Title</h1>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `footer`
  block_title[[1]] %>%
    render_block_title(context = "footer") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<h1 class=\"align-center\" style=\"color: #999999; ",
        "font-family: Helvetica, sans-serif; font-weight: 300; ",
        "line-height: 1.4; margin: 0; font-size: 20px; ",
        "margin-bottom: 0px; text-transform: capitalize; ",
        "text-align: center;\">Test Title</h1>.*"
      )
    )
})

test_that("the `render_block_text()` function returns the expected output", {

  # Create a text block
  block_text <- blocks(block_text("Test Text"))

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `body`
  block_text[[1]] %>%
    render_block_text(context = "body") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<p class=\"align-center\" style=\"font-family: Helvetica, ",
        "sans-serif; color: #000000;font-size: 14px; font-weight: normal; ",
        "margin: 0; margin-bottom: 12px; text-align: center;\">Test Text</p>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `header`
  block_text[[1]] %>%
    render_block_text(context = "header") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<p class=\"align-center\" style=\"font-family: Helvetica, ",
        "sans-serif; color: #999999;font-size: 12px; font-weight: normal; ",
        "margin: 0; margin-bottom: 12px; text-align: center;\">Test Text</p>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `footer`
  block_text[[1]] %>%
    render_block_text(context = "footer") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<p class=\"align-center\" style=\"font-family: Helvetica, ",
        "sans-serif; color: #999999;font-size: 12px; font-weight: normal; ",
        "margin: 0; margin-bottom: 12px; text-align: center;\">Test Text</p>.*"
      )
    )
})

test_that("the `render_block_spacer()` function returns the expected output", {

  # Expect that the spacer block conforms to a
  # regex pattern when the context is `body`
  render_block_spacer(context = "body") %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"wrapper\" style=\"vertical-align: top; ",
        "box-sizing: border-box; padding: 4px;\" valign=\"top\">\n",
        "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" ",
        "style=\"border-collapse: separate; mso-table-lspace: 0pt; ",
        "mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">\n<tbody>",
        "\n<tr>\n<td style=\"vertical-align: top;\" valign=\"top\">",
        "\n&nbsp;\n</td>\n</tr>\n</tbody>\n</table>\n</td>\n</tr>"
      )
    )

  # Expect that the spacer block conforms to a
  # regex pattern when the context is `header`
  render_block_spacer(context = "header") %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"wrapper\" style=\"vertical-align: top; ",
        "box-sizing: border-box; padding: 0px;\" valign=\"top\">\n",
        "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" ",
        "style=\"border-collapse: separate; mso-table-lspace: 0pt; ",
        "mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">\n<tbody>",
        "\n<tr>\n<td style=\"vertical-align: top;\" valign=\"top\">",
        "\n&nbsp;\n</td>\n</tr>\n</tbody>\n</table>\n</td>\n</tr>"
      )
    )

  # Expect that the spacer block conforms to a
  # regex pattern when the context is `footer`
  render_block_spacer(context = "footer") %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"wrapper\" style=\"vertical-align: top; ",
        "box-sizing: border-box; padding: 0px;\" valign=\"top\">\n",
        "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" ",
        "style=\"border-collapse: separate; mso-table-lspace: 0pt; ",
        "mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">\n<tbody>",
        "\n<tr>\n<td style=\"vertical-align: top;\" valign=\"top\">",
        "\n&nbsp;\n</td>\n</tr>\n</tbody>\n</table>\n</td>\n</tr>"
      )
    )
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

  # Expect that the extracted HTML block
  # conforms to a regex pattern when there
  # is one article
  block_articles_1 %>%
    render_block_articles() %>%
    as.character() %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"wrapper\" style=\"font-family: Helvetica, ",
        "sans-serif; font-size: 14px; vertical-align: top; box-sizing: ",
        "border-box; padding: 24px;\" valign=\"top\">\n<table border=\"0\" ",
        "cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: ",
        "separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; ",
        "width: 100%;\" width=\"100%\">\n<tbody>\n<tr>\n<td ",
        "style=\"font-family: Helvetica, sans-serif; font-size: 14px; ",
        "vertical-align: top;\" valign=\"top\">\n\n<tr>\n<td ",
        "class=\"article-title\" style=\"font-family: Helvetica, sans-serif; ",
        "vertical-align: top; font-size: 14px; font-weight: 800; ",
        "line-height: 1.4em; padding-bottom: 8px;\" valign=\"top\">",
        "\n<a href=\"link_1\" target=\"_blank\" style=\"color: #222222; ",
        "text-decoration: none; font-size: 14px; font-weight: 800; ",
        "line-height: 1.4em;\">title_1</a>\n</td>\n</tr>\n\n<p ",
        "style=\"font-family: Helvetica, sans-serif; font-size: 14px; ",
        "font-weight: normal; margin: 0; margin-bottom: 16px;\">\ncontent_1\n",
        "</p>\n\n</td>\n</tr>\n</tbody>\n</table>\n</td>\n</tr>"
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

  # Expect that the extracted HTML block
  # conforms to a regex pattern when there
  # are two articles
  block_articles_2 %>%
    render_block_articles() %>%
    as.character() %>%
    expect_match("^<tr>.*?title_1.*?content_1.*?title_2.*?content_2.*</tr>$")

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

  # Expect that the extracted HTML block
  # conforms to a regex pattern when there
  # are two articles
  block_articles_3 %>%
    render_block_articles() %>%
    as.character() %>%
    expect_match(
      "^<tr>.*?title_1.*?content_1.*?title_2.*?content_2.*title_3.*?content_3.*</tr>$"
    )

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

  # Expect an error if more than three `article`
  # objects provided to `block_articles()`
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
      article(
        title = "title_3",
        content = "content_3",
        link = "link_3"
      ),
      article(
        title = "title_4",
        content = "content_4",
        link = "link_4"
      )
    )
  )

  # Expect that the article image template (1 article) has
  # a specific value
  article_image_template_1() %>%
    expect_match(
      paste0(
        "^<p style=\"font-family: Helvetica, sans-serif; font-size: 14px;.*?",
        "width=\"552\" class=\"img-responsive img-block\".*$"
      )
    )

  # Expect that the article image template (2 articles) has
  # a specific value
  article_image_template_2() %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"article-thumbnail\" style=\"font-family: ",
        "Helvetica, sans-serif; font-size: 14px; vertical-align: top; ",
        "padding-bottom: 8px;\".*?width=\"250\".*"
      )
    )

  # Expect that the article image template (3 articles) has
  # a specific value
  article_image_template_3() %>%
    expect_match(
      paste0(
        "<tr>\n<td class=\"article-thumbnail\" style=\"font-family: ",
        "Helvetica, sans-serif; font-size: 14px; vertical-align: top; ",
        "padding-bottom: 8px;\".*?width=\"149\".*"
      )
    )
})

test_that("the social link functions work correctly", {

  social_link_1 <-
    social_link(service = "Dribbble", link = "https://dribbble.com")

  # Expect that `social_link_1` is a list
  social_link_1 %>% expect_type("list")

  # Expect certain names in the list object
  social_link_1 %>%
    names() %>%
    expect_equal(
      c("service", "link", "icon", "variant", "alt")
    )

  # Expect that the `service` name is in lowercase
  social_link_1$service %>% expect_equal("dribbble")

  # Expect that the `link` has been written
  social_link_1$link %>% expect_equal("https://dribbble.com")

  # Expect that the link to the `icon` is available
  social_link_1$icon %>% expect_match("^https:.*?social_icons/dribbble-bw.png")

  # Expect that the variant is `NULL` (since it wasn't specified)
  social_link_1$variant %>% expect_null()

  # Expect that the `alt` text is the same as the lowercase
  # service name
  social_link_1$alt %>% expect_equal(social_link_1$service)

  # Expect that the `social_link_1` object has the
  # class `social_link`
  social_link_1 %>% expect_s3_class("social_link")

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

  social_service_icon_variants() %>%
    expect_equal(c("color", "bw", "dark_gray", "gray", "light_gray"))
})
