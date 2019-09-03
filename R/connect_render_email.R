#' @export
connect_render_email <- function(input,
                                 connect_footer = TRUE,
                                 subject = NULL,
                                 attachments = NULL,
                                 attach_output = FALSE,
                                 preview = TRUE) {

  # Check that input is an .Rmd file

  connect_email(
    email = render_email(
      input = input,
      connect_footer = TRUE,
      envir = parent.frame(),
      quiet = TRUE),
    subject = NULL,
    attachments = NULL,
    attach_output = FALSE,
    text = NULL,
    preview = TRUE)

}
