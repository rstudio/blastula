library(blastula)
library(ggplot2)
library(htmltools)

body <- blocks(
  block_title("This is a title block"),
  block_text("This is a text block"),
  block_spacer(),
  block_text("This is another text block, with a spacer above, and <strong>some html</strong> and *markdown*"),
  block_text(align = "left", md("This is an md() block, with <strong>some html</strong> and *markdown*")),
  block_text(md(
    paste(
      add_cta_button(
        url = "http://www.website.net",
        text = "Press This Button"
      ),
      add_ggplot(ggplot(cars, aes(speed, dist)) + geom_point())
    )
  )),
  block_text(md(
    add_image(system.file(package="blastula", "img/pexels-photo-267151.jpeg"))
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


eml <- compose_email(body, "Header", footer, title = "The Main Title", content_width = 600)
eml
blastula:::generate_rfc2822(eml, con = "test.eml", subject = "Subject",
  from = "sender@example.com", to = "recipient@example.com")
browseURL("test.eml")

if (FALSE) {
  smtp_send(eml,
    to = "rstudio.dfa1@litmusemail.com",
    from = "joe_vmkt@hotmail.com",
    subject = "Test message",
    credentials = creds_key("joe_vmkt1"))
}
