#' @export
connect_render_email <- function(input,
                                 connect_footer = TRUE,
                                 subject = NULL,
                                 attachments = NULL,
                                 attach_output = FALSE,
                                 preview = TRUE,
                                 quiet = TRUE,
                                 envir = parent.frame()) {

  # Check that input is an .Rmd file

  connect_email(
    email = render_email(
      input = input,
      connect_footer = connect_footer,
      envir = envir,
      quiet = quiet
    ),
    subject = subject,
    attachments = attachments,
    attach_output = attach_output,
    text = NULL,
    preview = preview
  )
}
