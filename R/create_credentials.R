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
#' @param provider An optional email provider shortname for autocompleting SMTP
#'   configuration details (the `host`, `port`, `use_ssl` options). Options
#'   currently include `gmail`, `outlook`, and `office365`. If nothing is
#'   provided then values for `host`, `port`, and `use_ssl` are expected.
#' @param host,port,use_ssl Configuration info for the SMTP server. The `host`
#'   and `port` parameters are the address and port for the SMTP server.
#'   `use_ssl` is an option as to whether to allow the use of STARTTLS, if
#'   available: it should be `TRUE` unless you have a specific reason to set it
#'   to `FALSE`.
#'
#' @examples
#' # Create a credentials file to make it
#' # much easier to send email out through
#' # Gmail with `smtp_send()`; name the
#' # file "gmail_creds"
#'
#' # create_smtp_creds_file(
#' #   file = "gmail_creds",
#' #   user = "user_name@gmail.com",
#' #   provider = "gmail"
#' #   )
#'
#' @export
create_smtp_creds_file <- function(file,
                                   user = NULL,
                                   provider = NULL,
                                   host = NULL,
                                   port = NULL,
                                   use_ssl = NULL) {

  # nocov start

  # Use an empty string for `user` if NULL
  if (is.null(user)) user <- ""

  # Create a credentials list from the function inputs
  credentials_list <-
    create_credentials_list(
      provider = provider,
      user = user,
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

  # nocov end
}

#' Store SMTP credentials in the system's key-value store
#'
#' We can set SMTP access credentials in the system-wide key-value store for the
#' purpose of more easily sending email messages through [smtp_send()]. With
#' this key added, the credentials helper [creds_key()] can be used in the
#' `credentials` argument of [smtp_send()] (the `id` value is used to
#' unambiguously refer to each key).
#'
#' Support for setting keys through `create_smtp_creds_key()` is provided
#' through the **keyring** package. This function cannot be used without that
#' package being available on the system. We can use
#' `install.packages("keyring")` to install **keyring**.
#'
#' @param id An identifying label for the keyname. The full key name is
#'   constructed in the following way: `blastula-v1-<id>`. This `id` value is
#'   what's needed later to either use the key with [creds_key()], or, delete
#'   the key with [delete_credential_key()]. A single, non-`NA` character,
#'   numeric, or integer value can be supplied here; the `id` will be coerced to
#'   a character value. If the `id` is supplied as a single character value, it
#'   cannot be an empty string and it cannot include hyphen characters.
#' @param overwrite An option that controls the overwriting of existing keys
#'   with the same `id` value. By default, this is `FALSE` (where overwriting is
#'   prohibited).
#' @inheritParams create_smtp_creds_file
#'
#' @examples
#' # Store SMTP credentials using the
#' # system's secure key-value store to
#' # make it much easier to send email
#' # out through Gmail with `smtp_send()`;
#' # provide the `id` of "gmail_creds"
#'
#' # create_smtp_creds_key(
#' #   id = "gmail_creds",
#' #   provider = "gmail",
#' #   user = "user_name@gmail.com",
#' #   )
#'
#' @export
create_smtp_creds_key <- function(id,
                                  user = NULL,
                                  provider = NULL,
                                  host = NULL,
                                  port = NULL,
                                  use_ssl = NULL,
                                  overwrite = FALSE) {

  # nocov start

  # Use an empty string for `user` if NULL
  if (is.null(user)) user <- ""

  # Creating credentials on the system-wide key-value
  # store requires the installation of the keyring package
  validate_keyring_available(fn_name = "create_smtp_creds_key")

  # Stop function if `id` provided is not of the right type
  if (!(inherits(id, "character") || inherits(id, "numeric") || inherits(id, "integer"))) {
    stop("The provided `id` value must be a single character or numeric value.",
         call. = TRUE)
  }

  # Stop function if the `id` value isn't of length 1
  if (length(id) != 1) {
    stop("Only a vector of length 1 should be used for the `id` value.",
         call. = FALSE)
  }

  # Stop function if the `id` is an empty string or NA
  if (is.na(id) || id == "") {
    stop("The value for `id` should not be `NA` or an empty string.",
         call. = FALSE)
  }

  # Stop function if a hyphen is used within `id`
  if (is.character(id) && grepl("-", id)) {
    stop("Hyphens are not allowed as characters for an `id` value",
         call. = FALSE)
  }

  # Determine whether the key already exists
  creds_tbl <- get_keyring_creds_table()
  existing_key <- id %in% creds_tbl$id

  # Stop the function if the `id` corresponds to an
  # existing key and `overwrite = FALSE` (the default)
  if (existing_key && !overwrite) {
    stop("The specified `id` corresponds to a credential key already in the key-value store:\n",
         "* Use a different `id` value with `create_smtp_creds_key()`, or\n",
         "* If intentional, overwrite the existing key using the `overwrite = TRUE` option",
         call. = FALSE)
  }

  # Delete the existing key if `overwrite = TRUE`; the
  # `keyring::key_set_with_value()` function doesn't support
  # overwriting keys with the same `service_name` and we'd get
  # a duplicate `key_name` otherwise
  if (existing_key && overwrite) {
    delete_credential_key(id = id)
  }

  # Create a credentials list from the function inputs
  credentials_list <-
    create_credentials_list(
      provider = provider,
      user = user,
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
    username = user,
    password = serialized
  )

  # Issue a message that provides identifiers and later usage
  message(
    "The system key store has been updated with the \"", service_name,
    "\" key with the `id` value \"", id ,"\".\n",
    "* Use the `view_credential_keys()` function to see all available keys\n",
    "* You can use this key within `smtp_send()` with ",
    "`credentials = creds_key(\"", id, "\")`"
  )

  # nocov end
}

#' Ask for a password
#'
#' @noRd
get_password <- function(msg = "Enter the SMTP server password: ") {

  # nocov start

  getPass::getPass(msg = msg)

  # nocov end
}

#' Create a credentials list object
#'
#' @noRd
create_credentials_list <- function(provider,
                                    user,
                                    password = get_password(),
                                    host,
                                    port,
                                    use_ssl) {

  creds_internal(
    user = user,
    password = password,
    provider = provider,
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
    host = host,
    port = port,
    use_ssl = use_ssl,
    user = user,
    password = password
  )
}

#' Stops function if the system is not capable of using **keyring**
#'
#' @noRd
validate_keyring_capable <- function() {

  # nocov start

  if (!keyring::has_keyring_support()) {
    stop("To store SMTP via *keyring*, the system needs to have",
         "*keyring* support", call. = FALSE)
  }

  # nocov end
}

#' Stops function if the **keyring** package isn't installed
#'
#' @noRd
validate_keyring_available <- function(fn_name) {

  # nocov start

  if (!requireNamespace("keyring", quietly = TRUE)) {
    stop("The `keyring` package is required for using the ",
         "`", fn_name, "()` function", call. = FALSE)
  }

  validate_keyring_capable()

  # nocov end
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
