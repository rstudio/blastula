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
                           content_type = mime::guess_type(file_path),
                           disposition = "attachment",
                           filename = basename(file_path)) {

  # Get the expanded path for the file
  expanded_path <- file_path %>% path.expand() %>%
    normalizePath(mustWork = TRUE)

  # Create the attachment list
  attachment_list <-
    list(
      file_path = expanded_path,
      content_type = content_type,
      disposition = disposition,
      filename = filename
    )

  # Add the attachment list to `email$attachments`
  email$attachments <- c(email$attachments, list(attachment_list))

  email
}
