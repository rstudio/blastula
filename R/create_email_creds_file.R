#' Create a file with email access credentials
#' @description Creates a file with access
#' credentials for the purpose of automatically
#' emailing notification messages.
#' @param provider an optional provider email provider
#' with which an STMP account is available. Options
#' include \code{gmail}, \code{outlook},
#' \code{office365}, and \code{icloud}.
#' @param host the \code{host} name.
#' @param user the username for the email account.
#' @param password the password associated with the
#' @param sender the sender name.
#' @param port the port number.
#' \code{user}'s email address.
#' @param use_ssl an option as to whether to use
#' SSL; supply a \code{TRUE} or \code{FALSE}
#' value (\code{TRUE} is the default value).
#' @param use_tls a logical value to
#' indicate whether to use TLS.
#' @param authenticate an option as to whether to
#' authenticate; supply a \code{TRUE} or \code{FALSE}
#' value (\code{TRUE} is the default value).
#' @examples
#' \dontrun{
#' # Create a credentials file to facilitate
#' # the sending of email messages
#' create_email_creds_file(
#'   file = "~/.email_file",
#'   sender = "correspondences@blastula.org",
#'   host = "smtp.blastula.org",
#'   port = 465,
#'   user = "have_a@blastula.org",
#'   password = "************")
#' }
#' @importFrom stringr str_replace_all
#' @export create_email_creds_file

create_email_creds_file <- function(provider = NULL,
                                    host = NULL,
                                    user = NULL,
                                    password = NULL,
                                    sender = NULL,
                                    port = NULL,
                                    use_ssl = TRUE,
                                    use_tls = FALSE,
                                    authenticate = TRUE) {

  # Ensure that `use_ssl` is either TRUE or FALSE
  if (!(use_ssl %in% c(TRUE, FALSE))) {
    stop("The value supplied to `use_ssl` must be TRUE or FALSE.")
  }

  # Ensure that `use_tls` is either TRUE or FALSE
  if (!(use_tls %in% c(TRUE, FALSE))) {
    stop("The value supplied to `use_tls` must be TRUE or FALSE.")
  }

  # Ensure that `authenticate` is either TRUE or FALSE
  if (!(authenticate %in% c(TRUE, FALSE))) {
    stop("The value supplied to `authenticate` must be TRUE or FALSE.")
  }

  # If a `provider` name is given, extract values for `host`,
  # `port`, `use_ssl`, `use_tls`, and `authenticate`
  if (!is.null(provider) &&
      provider %in% (smtp_settings() %>% dplyr::pull(short_name))) {

    settings_record <- smtp_settings() %>% dplyr::filter(short_name == provider)

    # Extract settings for the provider
    host <- settings_record %>% dplyr::pull(server)
    port <- settings_record %>% dplyr::pull(port)
    use_ssl <- settings_record %>% dplyr::pull(use_ssl)
    use_tls <- settings_record %>% dplyr::pull(use_tls)
    authenticate <- settings_record %>% dplyr::pull(authenticate)
  }

  # Collect all credential values into a
  # named vector
  credentials <- c(
    sender = as.character(sender),
    host = as.character(host),
    port = as.character(port),
    user = as.character(user),
    password = as.character(password),
    use_ssl = as.character(use_ssl),
    use_tls = as.character(use_tls),
    authenticate = as.character(authenticate))

  # Construct a file name
  file <-
    paste0(
      ".bls_",
      stringr::str_replace_all(
        string = host,
        pattern = "\\.",
        replacement = "_"))

  # Save the credential values as a file
  saveRDS(credentials, file = file)
}
