#' Store SMTP credentials in a file
#'
#' We can create a file with SMTP configuration and access credentials for the
#' purpose of more easily sending email messages through [smtp_send()]. With
#' this file produced, the credentials helper [creds_file()] can be used in the
#' `credentials` argument of [smtp_send()].
#'
#' @param file The output filename for the credentials file.
#' @param user The username for the email account. Typically, this is the email
#'   address associated with the account.
#' @param provider An optional email provider shortname for autocompleting STMP
#'   configuration details (the `host`, `port`, `use_ssl` options). Options
#'   currently include `gmail`, `outlook`, and `office365`. If nothing is
#'   provided then values for `host`, `port`, and `use_ssl` are expected.
#' @param host,port,use_ssl Configuration info for the SMTP server. The `host`
#'   and `port` parameters are the address and port for the SMTP server;
#'   `use_ssl` is an option as to whether to use SSL: supply a `TRUE` or `FALSE`
#'   value.
#' @param sender_name An option to specify a sender name. This isn't always
#'   visible to the recipient, however, as some SMTP servers will suppress this.
#' @examples
#' \dontrun{
#' # Create a credentials file to make it
#' # much easier to send email out through
#' # Gmail with `smtp_send()`; name the
#' # file "gmail_creds"
#' create_smtp_creds_file(
#'   file = "gmail_creds",
#'   user = "user_name@gmail.com",
#'   provider = "gmail"
#'   )
#' }
#' @export
create_smtp_creds_file <- function(file,
                                   user = NULL,
                                   provider = NULL,
                                   host = NULL,
                                   port = NULL,
                                   use_ssl = NULL,
                                   sender_name = NULL) {

  # Use an empty string for `user` if NULL
  if (is.null(user)) user <- ""

  # Create a credentials list from the function inputs
  credentials_list <-
    create_credentials_list(
      provider = provider,
      user = user,
      sender_name = sender_name,
      host = host,
      port = port,
      use_ssl = use_ssl
    )

  # Create a plaintext JSON string for the credentials
  serialized <- JSONify_credentials(credentials_list)

  # Write the credential values into a plaintext file
  # that contains JSON
  writeLines(serialized, file)
  Sys.chmod(file, mode = "0600")

  # Issue a message stating that the file has been created
  message("The SMTP credentials file (`", file, "`) has been generated")
}

#' Store SMTP credentials in the system's key-value store
#'
#' We can set SMTP access credentials in the system-wide key-value store for the
#' purpose of more easily sending email messages through [smtp_send()]. With
#' this key added, the credentials helper [creds_key()] can be used in the
#' `credentials` argument of [smtp_send()].
#'
#' Support for setting keys through `create_smtp_creds_key()` is provided
#' through the \pkg{keyring} package. This function cannot be used without that
#' package first being available on the system. We can use
#' `install.packages("keyring")` to install
#'
#' @param id An identifying label for the keyname. The full key name is
#'   constructed in the following way: `blastula-v1-<id>`.
#' @inheritParams create_smtp_creds_file
#' @examples
#' \dontrun{
#' # Store SMTP crendentials using the
#' # system's secure key-value store to
#' # make it much easier to send email
#' # out through Gmail with `smtp_send()`;
#' # provide the `id` of "gmail_creds"
#' create_smtp_creds_key(
#'   id = "gmail_creds",
#'   provider = "gmail",
#'   user = "user_name@gmail.com",
#'   )
#' }
#' @export
create_smtp_creds_key <- function(id,
                                  user = NULL,
                                  provider = NULL,
                                  host = NULL,
                                  port = NULL,
                                  use_ssl = NULL,
                                  sender_name = NULL) {

  # Use an empty string for `user` if NULL
  if (is.null(user)) user <- ""

  # Creating credential on the system-wide key-value
  # store requires the installation of the keyring package
  if (!requireNamespace("keyring", quietly = TRUE)) {

    stop("The `keyring` package is required for using the ",
         "`create_smtp_creds_key()` function",
         call. = FALSE)

  }

  # Determine whether the keyring package can be used
  validate_keyring_capable()

  # Create a credentials list from the function inputs
  credentials_list <-
    create_credentials_list(
      provider = provider,
      user = user,
      sender_name = sender_name,
      host = host,
      port = port,
      use_ssl = use_ssl
    )

  # Construct the final key name, using a prefix
  # that includes the schema version
  service_name <- paste0("blastula-v", schema_version, "-", id)

  # Create a plaintext JSON string for the credentials
  serialized <- JSONify_credentials(credentials_list)

  # Set the key in the system's default keyring
  keyring::key_set_with_value(
    service = service_name,
    user = user,
    password = serialized
  )

  # Issue a message stating that the file has been created
  message(
    "The system key store has been updated with the (`", service_name,
    "`) key.\n",
    " * You can use this key within `smtp_send()` with ",
    "`credentials = creds_key(\"", id, "\")`"
  )
}

#' Create a credentials list object
#'
#' @noRd
create_credentials_list <- function(provider,
                                    user,
                                    password,
                                    sender_name,
                                    host,
                                    port,
                                    use_ssl) {
  if (missing(password) || is.null(password)) {
    password <- getPass::getPass("Enter the SMTP server password: ")
  }

  creds_internal(
    user = user,
    password = password,
    provider = provider,
    sender_name = sender_name,
    host = host,
    port = port,
    use_ssl = use_ssl
  )
}

#' Create a credentials list object
#'
#' @noRd
creds_internal <- function(user = NULL,
                           password = NULL,
                           provider = NULL,
                           sender_name = NULL,
                           host = NULL,
                           port = NULL,
                           use_ssl = NULL) {

  # If a `provider` name is given, extract values for `host`,
  # `port`, `use_ssl`, `use_tls`, and `authenticate`
  if (!is.null(provider)) {

    validate_smtp_provider(provider = provider)

    # Extract the record for the SMTP provider
    smtp_settings <- get_smtp_provider_values(provider = provider)

    # Extract settings for the provider
    if (is.null(host)) host <- smtp_settings$server
    if (is.null(port)) port <- smtp_settings$port
    if (is.null(use_ssl)) use_ssl <- smtp_settings$use_ssl
  }

  if (any(is.null(host), is.null(port), is.null(use_ssl))) {
    stop("The `host`, `port`, and `use_ssl` SMTP options must be provided ",
         "with values.", call. = FALSE)
  }

  # Generate the credentials list
  list(
    version = schema_version,
    sender_name = sender_name,
    host = host,
    port = port,
    use_ssl = use_ssl,
    user = user,
    password = password
  )
}

#' Stops function is the system is not capable of using \pkg{keyring}
#'
#' @noRd
validate_keyring_capable <- function() {

  if (!keyring::has_keyring_support()) {
    stop("To store SMTP via *keyring*, the system needs to have",
         "*keyring* support", call. = FALSE)
  }
}

#' Stops function if a given `provider` is not supported
#'
#' @noRd
validate_smtp_provider <- function(provider) {

  if (!(provider %in% get_provider_list())) {

    stop("The supplied `provider` name is not one that is supported\n",
         " * Use either `\"gmail\"`, `\"outlook\"`, or `\"office365\"`.",
         call. = FALSE)
  }
}

#' Convert a `credentials_list` to a JSON string
#'
#' @noRd
JSONify_credentials <- function(credentials_list) {

  # Create a plaintext JSON string for the credentials
  credentials_list %>%
    jsonlite::serializeJSON() %>%
    as.character()
}

# Globally set the schema version for the storage
# of SMTP settings and authentication via keyring
schema_version <- 1L
