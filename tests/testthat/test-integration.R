library(ggplot2)
library(htmltools)

test_that("Email that uses all compose features", {

  body <-
    blocks(
      block_title("This is a title block"),
      block_text("This is a text block, center aligned", align = "center"),
      block_spacer(),
      block_text("This is another text block, with a spacer above, and <strong>some html</strong> and *markdown*"),
      block_text(md("This is an md() block, with <strong>some html</strong> and *markdown*")),
      block_text(md(
        paste(
          add_cta_button(
            url = "http://www.website.net",
            text = "Press This Button"
          ),
          # Work around the fact that ggplot2 doesn't render exactly the same
          # on different platforms
          # 2020-05-08: We can't ever call add_ggplot directly because even on
          # Linux-only we were seeing differences between Joe's desktop and travis
          # add_ggplot(ggplot(cars, aes(speed, dist)) + geom_point() + ggtitle("A Plot"))
          add_image(test_path("ggplot.png"), alt = "A Plot", width = 500)
        )
      )),
      block_text(md(
        add_image(system.file(package="blastula", "img/pexels-photo-267151.jpeg"),
                  alt = "A nice sunset")
      )),
      block_articles(
        article(
          image = "https://i.imgur.com/XMU8yJa.jpg",
          title = "Taiwan",
          content =
            "It is a thriving mosaic of tradition,
              culture, and high-tech development,
              merging Eastern and Western influences."
        ),
        article(
          image = "https://i.imgur.com/aYOm3Tk.jpg",
          title = "Japan",
          content =
            "Japan is an archipelago consisting
              of 6,852 islands along East Asia's
              Pacific Coast."
        ),
        article(
          image = "https://i.imgur.com/ekjFVOL.jpg",
          title = "Singapore",
          content =
            "Singapore is an island city-state
               in Southeast Asia. It's lies at the
               southern tip of the Malay Peninsula."
        )
      )
    )

  footer <-
    blocks(
      block_text("Thanks for reading! Find us here:"),
      block_social_links(
        social_link(
          service = "pinterest",
          link = "https://www.pinterest.ca/TravelLeisure/",
          variant = "color"
        ),
        social_link(
          service = "tripadvisor",
          link = "https://www.tripadvisor.ca/TravelersChoice",
          variant = "color"
        )
      )
    )

  eml <-
    compose_email(
      body = body,
      header = "Header",
      footer = footer,
      title = "The Main Title",
      content_width = 600,
      font_family = "Roboto, 'Segoe UI', 'Lucida Grande', Verdana, 'Bitstream Vera Sans', sans-serif"
    )

  eml_raw <-
    generate_rfc2822(
      eml = eml, subject = "Subject",
      date = as.POSIXct("2020-04-18 16:38:44", tz = "GMT"),
      from = "sender@example.com", to = "recipient@example.com",
      uuid_source = create_uuid_source()
    )

  # Make sure no \n appears without \r. RFC2822 specifies \r\n separators;
  # rendering will break on Outlook/Windows if \n is used.
  expect_false(grepl("(?<!\r)\n", eml_raw, perl = TRUE))

  # Limitation in verify_output prevents it from working properly with strings
  # that contain \r\n
  eml_raw <- gsub("\r\n", "\n", eml_raw)

  # cat("\n")
  # cat(eml_raw)

  expect_snapshot(eml_raw)
})
