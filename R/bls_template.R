#' Default template for `compose_email`
#'
#' A template function that is suitable for using as the `template` argument of
#' [compose_email()]. Template functions should generally not be called
#' directly. When implementing your own template function, you must include
#' parameters for `html_body`, `html_header`, `html_footer`, and `title`; you
#' may also optionally add your own parameters, which callers to
#' `compose_email()` can provide through the `...` argument.
#'
#' @param html_body,html_header,html_footer htmltools tag objects (e.g.
#'   [htmltools::tags()] or [htmltools::HTML()]), or `NULL` to omit.
#' @param title Plain text title to be used for the `<title>` element; may be
#'   displayed in mobile phone notifications.
#' @param content_width The width that should be used for the content area. This
#'   should NOT be specified CSS units like `"600px"`, but as either an integer
#'   (for pixels, e.g. `600`) or a percent string like `"85%"`.
#' @param font_family The CSS value to use for `font-family`.
#'
#' @return A string containing a complete HTML document.
#'
#' @export
blastula_template <- function(html_body, html_header, html_footer, title,
  content_width = "1000px", font_family = "Helvetica, sans-serif") {

  result <- htmltools::renderTags(
    tagList(
      tags$head(
        # derived from https://github.com/TedGoas/Cerberus
        htmltools::includeHTML(system.file(package = "blastula", "cerberus-meta.html")),
        tags$title(title),
        tags$style(HTML(paste0("
body {
  font-family: ", font_family, ";
  font-size: 14px;
}
.content {
  background-color: white;
}
.content .message-block {
  margin-bottom: 24px;
}
.header .message-block, .footer message-block {
  margin-bottom: 12px;
}
img {
  max-width: 100%;
}
@media only screen and (max-width: 767px) {
  .container {
    width: 100%;
  }
  .articles, .articles tr, .articles td {
    display: block;
    width: 100%;
  }
  .article {
    margin-bottom: 24px;
  }
}
      ")))
      ),
      tags$body(
        style = css(
          background_color = "#f6f6f6",
          font_family = font_family,
          color = "#222",
          margin = "0",
          padding = "0"
        ),
        panel(outer_class = "container", outer_align = "center", padding = "24px",
          width = "85%", max_width = htmltools::validateCssUnit(content_width),

          if (!is.null(html_header)) {
            div(class = "header",
              style = css(
                font_family = font_family,
                color = "#999999",
                font_size = "12px",
                font_weight = "normal",
                margin = "0 0 24px 0",
                text_align = "center"
              ),
              html_header
            )
          },
          panel(outer_class = "content", padding = "12px", background_color = "white",
            html_body
          ),
          if (!is.null(html_footer)) {
            div(class = "footer",
              style = css(
                font_family = font_family,
                color = "#999999",
                font_size = "12px",
                font_weight = "normal",
                margin = "24px 0 0 0",
                text_align = "center"
              ),
              html_footer
            )
          }
        )
      )
    )
  )

  HTML(sprintf("<!doctype html>
<html>
  <head>
%s
  </head>
%s
</html>", result$head, result$html))
}
