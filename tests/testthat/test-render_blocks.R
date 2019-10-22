context("Rendering HTML blocks")

test_that("the `render_block_title()` function returns the expected output", {

  # Create a title block
  block_title <- blocks(block_title("Test Title"))

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `body`
  block_title %>%
    render_block_title(context = "body") %>%
    expect_match(
      paste0(
      "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
      "<h1 class=\"align-center\" style=\"color: #222222; ",
      "font-family: Helvetica, sans-serif; font-weight: 300; ",
      "line-height: 1.4; margin: 0; font-size: 36px; ",
      "margin-bottom: 4px; text-transform: capitalize; ",
      "text-align: center;\">Test Title</p>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `header`
  block_title %>%
    render_block_title(context = "header") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<h1 class=\"align-center\" style=\"color: #999999; ",
        "font-family: Helvetica, sans-serif; font-weight: 300; ",
        "line-height: 1.4; margin: 0; font-size: 20px; ",
        "margin-bottom: 0px; text-transform: capitalize; ",
        "text-align: center;\">Test Title</p>.*"
      )
    )

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `footer`
  block_title %>%
    render_block_title(context = "footer") %>%
    expect_match(
      paste0(
        "^<tr>.*?<td .*?<table.*?<tbody>.*?<tr>.*?<td style.*?",
        "<h1 class=\"align-center\" style=\"color: #999999; ",
        "font-family: Helvetica, sans-serif; font-weight: 300; ",
        "line-height: 1.4; margin: 0; font-size: 20px; ",
        "margin-bottom: 0px; text-transform: capitalize; ",
        "text-align: center;\">Test Title</p>.*"
      )
    )
})

test_that("the `render_block_text()` function returns the expected output", {

  # Create a text block
  block_text <- blocks(block_text("Test Text"))

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `body`
  block_text %>%
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
  block_text %>%
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
  block_text %>%
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

  # Create a spacer block
  block_spacer <- blocks(block_spacer())

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `body`
  block_spacer %>%
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

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `header`
  block_spacer %>%
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

  # Expect that the extracted HTML block
  # conforms to a regex pattern when the
  # context is `footer`
  block_spacer %>%
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

  # Expect that the spacer line template has a specific value
  spacer_line_template() %>%
    expect_equal("<p class=\"align-center\" style=\"margin: 0; margin-bottom: 0; text-align: center;\">")
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
