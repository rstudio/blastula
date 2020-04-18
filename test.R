library(blastula)
library(ggplot2)
library(htmltools)

panel <- function(..., width = "100%", outer_align = NULL, inner_align = NULL,
  padding = NULL, margin = NULL, outer_class = NULL, inner_class = NULL,
  background_color = NULL) {

  withTags(
    table(width = width, align = outer_align, class = outer_class,
      style = css(
        margin = margin,
        background_color = background_color
      ),
      tr(
        td(align = inner_align, class = inner_class,
          style = css(
            padding = padding
          ),
          ...
        )
      )
    )
  )
}

table_cell <- function(...) {
  withTags(
    table(
      tr(
        td(...)
      )
    )
  )
}

blocks <- function(...) {
  tagList(...)
}

block_title <- function(title) {
  tags$h1(class = "message-block block_title",
    style = css(
      color = "#222222",
      font_weight = "300",
      line_height = "1.4",
      margin = "0",
      font_size = "36px",
      margin_bottom = "4px",
      text_align = "center"
    ),
    title
  )
}

block_text <- function(text, align = c("center", "left", "right", "justify")) {
  if (length(align) > 1) {
    align <- align[[1]]
  }

  tags$div(class = "message-block block_text", style = css(text_align = align),
    text
  )
}

md <- function(text) {
  HTML(commonmark::markdown_html(
    paste(collapse = "\n",
      as.character(text)
    )
  ))
}

block_spacer <- function() {
  tags$table(class = "message-block block_spacer",
    tags$tr(
      tags$td(HTML("&nbsp;"))
    )
  )
}

social_link <- function(service,
  link,
  variant = NULL,
  alt = NULL) {

  # Lowercase the input given as `service`
  service <- tolower(service)

  # Generate a link to an image for the service
  icon <-
    blastula:::icon_for_social_service(
      service = service,
      variant = variant
    )

  # Set default alt text if not provided
  if (is.null(alt)) {
    alt <- service
  }

  tags$a(.noWS = c("after-begin", "before-end"),
    href = link,
    tags$img(src = icon, alt = alt, width = "44", height = "44", style = "border: none;")
  )
}

block_social_links <- function(...) {
  panel(class = "message-block block_social", inner_align = "center", ...)
}

article <- function(image = NULL, title = NULL, content = NULL, link = NULL) {
  maybe_link <- function(...) {
    if (is.null(link)) {
      tagList(...)
    } else {
      tags$a(.noWS = c("after-begin", "before-end"), href = link,
        style = css(text_decoration = "none"),
        ...
      )
    }
  }

  tagList(
    if (!is.null(image)) {
      tags$div(style = css(margin_bottom = "8px"),
        maybe_link(
          tags$img(
            src = image,
            width = "250",
            style = css(
              width = "100%",
              border = "none"
            )
          )
        )
      )
    },
    if (!is.null(title)) {
      tags$h3(style = css(margin = 0), maybe_link(title))
    },
    if (!is.null(content)) {
      tags$div(content)
    }
  )
}

block_articles <- function(...) {
  len <- length(list(...))
  pct <- round(100 / len)

  div(class = "message-block block_articles",
    tags$table(class = "articles",
      tags$tr(
        lapply(list(...), function(article) {
          tags$td(class = "article", valign = "top", width = paste0(pct, "%"),
            article
          )
        })
      )
    )
  )
}

tag_template <- function(title, html_header, html_body, html_footer) {
  as.character(tagList(
    HTML("<!doctype html>"),
    tags$html(
      HTML("<head>"),
      tags$meta(joe="cheng"),
      tags$title(title),
      tags$style(HTML("
.content {
  background-color: white;
}
.message-block {
  margin-bottom: 24px;
}
img {
  max-width: 100%;
}
@media only screen and (max-width: 768px) {
  .container {
    width: 100%;
  }
  .articles {
    display: block;
  }
  .article {
    display: block;
    width: 100%;
    margin-bottom: 24px;
  }
}
      ")),
      HTML("</head>"),
      tags$body(
        style = css(
          background_color = "#f6f6f6",
          font_family = "Helvetica, sans-serif",
          color = "#222",
          margin = "0",
          padding = "8px"
        ),
        panel(outer_class = "container", outer_align = "center", padding = "8px",
          width = "85%",

          if (!is.null(html_header)) {
            div(class = "header",
              style = css(
                font_family =  "Helvetica, sans-serif",
                color = "#999999",
                font_size = "12px",
                font_weight = "normal",
                margin = "0 0 24px 0",
                text_align = "center"
              ),
              html_header
            )
          },
          panel(outer_class = "content", padding = "8px", background_color = "white",
            html_body
          ),
          if (!is.null(html_footer)) {
            div(class = "footer", style = "margin-top: 24px",
              html_footer
            )
          }
        )
      )
    )
  ))
}

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


eml <- compose_email(body, tags$span("Header"), footer, title = "The Main Title", template = tag_template)
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
