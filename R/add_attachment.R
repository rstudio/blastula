#' Add a file attachment to an email message
#'
#' This gives us a simple interface for attaching a file to the email object.
#' When it comes time to send the email through `smtp_send()`, all attachments
#' (specified by individual calls to `add_attachment()`) will be faithfully
#' transmitted along with the message.
#'
#' @inheritParams smtp_send
#' @param file_path The path for the file to be attached.
#' @param error_on_missing An option to stop with an error if the file cannot be
#'   located on disk. By default this is set to `TRUE`.
#' @export
add_attachment <- function(email,
                           file_path,
                           error_on_missing = TRUE) {

  # Get the expanded path for the file
  expanded_path <- file_path %>% path.expand() %>% file.path()

  # Stop function if the file can't be
  # found on disk
  if (!file.exists(expanded_path) && error_on_missing) {
    stop("The file given in `file_path` cannot be found.",
         call. = FALSE)
  }

  # Create the attachment list
  attachment_list <-
    create_attachment_list(
      file_path = expanded_path,
      disposition = no_options()
    )

  # Add the attachment list to `email$attachments`
  email <- email %>% add_attachment_list(attachment_list = attachment_list)

  email
}
