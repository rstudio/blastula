#' Add a file attachment to an email message
#'
#' This gives us a simple interface for attaching a file to the email object.
#' When it comes time to send the email through [smtp_send()], all attachments
#' (specified by individual calls to `add_attachment()`) will be faithfully
#' transmitted along with the message.
#'
#' There are options available to specify the attachment's MIME type, its
#' disposition, and customize the attachment's recipient-facing filename.
#'
#' @inheritParams smtp_send
#' @param file The filename for the file to be attached.
#' @param content_type The MIME type for the attachment. By default, this is
#'   guessed by the `mime::guess_type()` function on the basis of the `file`'s
#'   extension. The available MIME types that can be guessed are available in
#'   the `mime::mimemap` named character vector.
#' @param filename the filename for the attachment. This can be different than
#'   the basename provided to `file` for the purpose of customization. By
#'   default, the basename of `file` is taken to be the attachment's filename.
#'
#' @export
add_attachment <- function(
    email,
    file,
    content_type = mime::guess_type(file),
    filename = basename(file)
) {

  # Get the expanded path for the file
  expanded_path <-
    file %>%
    path.expand() %>%
    normalizePath(mustWork = TRUE)

  # Create the attachment list
  attachment_list <-
    list(
      file_path = expanded_path,
      content_type = content_type,
      disposition = "attachment",
      filename = filename
    )

  # Add the attachment list to `email$attachments`
  email$attachments <- c(email$attachments, list(attachment_list))

  email
}
