# nocov start

#' View all available **blastula** credential keys
#'
#' To understand which keys have been set using the [create_smtp_creds_key()]
#' function (and how they are identified), we can use the
#' `view_credential_keys()` function. What's provided is a tibble with three
#' columns: `id`, `key_name`, and `username`.
#'
#' Support for using the `view_credential_keys()` function (and for doing any
#' credential key management) is provided through the **keyring** package. This
#' function cannot be used without that package being available on the system.
#' We can use `install.packages("keyring")` to install **keyring**.
#'
#' @examples
#' # View the available SMTP credentials
#' # that are in the system's secure
#' # key-value store; the `id` values
#' # in the returned tibble provide what's
#' # necessary to send email through
#' # `smtp_send()` and the `creds_key()`
#' # credential helper function
#'
#' # view_credential_keys()
#'
#' @export
view_credential_keys <- function() {

  # Viewing credentials that are on the system-wide key-value
  # store requires the installation of the keyring package
  if (!requireNamespace("keyring", quietly = TRUE)) {

    stop("The `keyring` package is required for using the ",
         "`view_credential_keys()` function",
         call. = FALSE)
  }

  get_keyring_creds_table()
}

#' Delete a single **blastula** credential key
#'
#' It may be important to delete a credential key and the
#' `delete_credential_key()` function makes this possible. To understand which
#' keys are available in the key-value store (and to get their `id` values), use
#' the [view_credential_keys()] function.
#'
#' Support for using the `delete_credential_key()` function (and for doing any
#' credential key management) is provided through the **keyring** package. This
#' function cannot be used without that package being available on the system.
#' We can use `install.packages("keyring")` to install **keyring**.
#'
#' @examples
#' # Delete the credential key with
#' # the `id` value of `outlook`
#'
#' # delete_credential_key("outlook")
#'
#' @export
delete_credential_key <- function(id) {

  # Deleting credentials that are on the system-wide key-value
  # store requires the installation of the keyring package
  if (!requireNamespace("keyring", quietly = TRUE)) {

    stop("The `keyring` package is required for using the ",
         "`delete_credential_key()` function",
         call. = FALSE)
  }

  creds_tbl <- get_keyring_creds_table()

  # Get a vector of key names, otherwise known as service names in `keyring`
  ids_available <- creds_tbl$id

  # Stop if there are no credential keys available
  if (length(ids_available) < 1) {
    stop("There are no credential keys available, so there is nothing to delete.",
         call. = FALSE)
  }

  # Stop if the provided `id` doesn't match any of those available
  if (!(id %in% ids_available)) {
    stop("The specified `id` doesn't correspond to a credential key:\n",
         "* Use the `view_credential_keys()` function to examine which `id` values are valid",
         call. = FALSE)
  }

  # Obtain the matching service name that corresponds to the `id` value
  key_to_delete <-
    creds_tbl %>%
    dplyr::filter(id == !!id) %>%
    dplyr::pull(key_name)

  # Delete with `keyring::key_delete()`
  keyring::key_delete(service = key_to_delete)

  message(paste0("The blastula credential key with the `id` of \"", id, "\" has been deleted."))
}

#' Delete all **blastula** credential keys
#'
#' The `delete_all_credential_keys()` function deletes all **blastula**
#' credential keys, giving you a clean slate. Should specific keys need to be
#' deleted, the [delete_credential_key()] could be used (one call per credential
#' key to delete). Before using `delete_all_credential_keys()`, it may be useful
#' to see which keys are available in the key-value store. For that, use the
#' [view_credential_keys()] function.
#'
#' Support for using the `delete_all_credential_keys()` function (and for doing
#' any credential key management) is provided through the **keyring** package.
#' This function cannot be used without that package being available on the
#' system. We can use `install.packages("keyring")` to install **keyring**.
#'
#' @examples
#' # Delete all blastula credential keys
#' # in the system's key-value store
#'
#' # delete_all_credential_keys()
#'
#' @export
delete_all_credential_keys <- function() {

  # Deleting credentials that are on the system-wide key-value
  # store requires the installation of the keyring package
  if (!requireNamespace("keyring", quietly = TRUE)) {

    stop("The `keyring` package is required for using the ",
         "`delete_all_credential_keys()` function",
         call. = FALSE)
  }

  creds_tbl <- get_keyring_creds_table()

  # Get a vector of key names, otherwise known as service names in `keyring`
  keys_to_delete <- creds_tbl$key_name

  # Stop if there are no keys to delete
  if (length(keys_to_delete) < 1) {
    stop("There are no blastula keys available for deletion.", call. = FALSE)
  }

  # For every key, delete with `keyring::key_delete()`
  for (key in keys_to_delete) {
    keyring::key_delete(service = key)
  }

  message("All blastula credential keys have been deleted.")
}

# nocov end

#' Retrieve metadata and authentication values from an on-disk credentials file
#'
#' @noRd
get_smtp_file_creds <- function(file_name = NULL) {

  # For the given `file_name`, read in the JSON
  # data and convert it into a list object
  readLines(file_name, encoding = "UTF-8") %>%
    jsonlite::unserializeJSON()
}

# nocov start

#' Retrieve metadata and authentication values from keyring data
#'
#' @noRd
get_smtp_keyring_creds <- function(id = NULL) {

  # We requires the installation of the keyring package
  if (!requireNamespace("keyring", quietly = TRUE)) {

    stop("The `keyring` package is required.",
         call. = FALSE)
  }

  id_name <- id

  # Get a filtered table of key and values that
  # are only those keys generated by the
  # `create_smtp_creds_key()` function
  blastula_keys_tbl <-
    get_keyring_creds_table() %>%
    dplyr::filter(id == id_name)

  # If the given `key_name` doesn't correspond to an
  # entry in `blastula_keys_tbl`, stop the function with
  # an explanatory message
  if (nrow(blastula_keys_tbl) == 0) {
    stop("There is no blastula key that corresponds to the `key_name` of \"",
         key_name, "\".",
         call. = FALSE)
  }

  # Get the `key_name`
  key_name <- (blastula_keys_tbl %>% dplyr::pull(key_name))[1]

  # Get the `username`
  username <- (blastula_keys_tbl %>% dplyr::pull(username))[1]

  # For the given `key_name` get the key's stored value and
  # transform the JSON data to a list object
  keyring::key_get(service = key_name, username = username) %>%
    jsonlite::unserializeJSON()
}

#' Utility function for obtaining keyring entries related to blastula creds
#'
#' @noRd
get_keyring_creds_table <- function() {

  creds_tbl <-
    keyring::key_list() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(grepl(paste0("blastula-v", schema_version), service))

  if (nrow(creds_tbl) == 0) {

    empty_creds_tbl <-
      dplyr::tibble(
        id = NA_character_,
        key_name = NA_character_,
        username = NA_character_
      )[-1, ]

    return(empty_creds_tbl)

  } else {

    creds_tbl <-
      creds_tbl %>%
      dplyr::mutate(id = sapply(strsplit(service,"-"), `[`, 3)) %>%
      dplyr::select(id, key_name = service, username)
  }

  creds_tbl
}

# nocov end
