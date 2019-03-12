#' A block of text
#'
#' @param ... Paragraphs of text.
#' @importFrom commonmark markdown_html
#' @importFrom glue glue
#' @export
block_text <- function(...) {

  x <- list(...)

  text <-
    paste(x %>% unlist(), collapse = "\n") %>%
    commonmark::markdown_html() %>%
    tidy_gsub("<p>", "<p class=\"align-center\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; font-weight: normal; margin: 0; margin-bottom: 16px; text-align: center;\">")

  glue::glue(text_block_template()) %>% as.character()
}

#' A template for a text HTML fragment
#' @noRd
text_block_template <- function() {

"              <tr>
                <td class=\"wrapper\" style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top; box-sizing: border-box; padding: 24px;\" valign=\"top\">
                  <table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"border-collapse: separate; mso-table-lspace: 0pt; mso-table-rspace: 0pt; width: 100%;\" width=\"100%\">
                    <tbody>
                      <tr>
                        <td style=\"font-family: Helvetica, sans-serif; font-size: 14px; vertical-align: top;\" valign=\"top\">
                          {text}
                        </td>
                      </tr>
                    </tbody>
                  </table>
                </td>
              </tr>"
}
