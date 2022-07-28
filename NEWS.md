# blastula 0.3.3

* The `file` argument of `add_attachment()` now supports a vector of strings (#239, thanks @shrektan for the PR).

# blastula 0.3.2

* Email content width is now customizable in the `blastula_email()` and `compose_email()` functions. The default width is now increased to 1000px (#178).

* Added the `delete_credential_key()` and `delete_all_credential_keys()` functions for better management of credential keys added to the system key-value store by the `create_smtp_creds_key()` function (#173).

* Added the `creds_envvar()` credential helper function, which allows for SMTP password input via an environment variable (#159).

* Fixed incorrect rendering in Outlook, due to wrong line endings used for quoted-printable encoding (thanks @jdbarillas for identifying the source of the issue in #153).

* Fixed Unicode issues on Windows (#154).

* Fixed buggy internal logic for decoding numeric HTML entities that represent non-ASCII characters.

# blastula 0.3.1

This release contains fixes for R CMD check problems on CRAN test machines. 

# blastula 0.3.0

The **blastula** package has transitioned to using the **curl** package for SMTP mailing.

There are new functions for integrating **blastula** with R Markdown publishing on the RStudio Connect service. And sophisticated email bodies can be generated with *HTML Blocks*.

## Breaking Changes

* `send_email_out()` has been removed; the `smtp_send()` function is its replacement for sending email through an SMTP server

* The revised `smtp_send()` function internally uses `curl::send_mail()` to send RFC2822 email message that **blastula** now independently generates; now, there is no need to download third-party binaries to send via SMTP

* The `smtp_send()` no longer offers text interpolation in `{ }` (which was provided by `glue::glue()`) since this poses a security risk; email strings can be precomposed using `glue::glue()` independently, or, by using `c()`, `paste()`/`paste0`.

* If an email message string contains Markdown text or HTML fragments, the `md()` function must be used to perform rendering

* The `create_email_creds_file()` function has been removed in favor of a suite of functions for storing SMTP configuration info and credentials (`create_smtp_creds_file()`, `create_smtp_creds_key()`); the following credentials helper functions are available for retrieving or directly specifying configuration and credentials during sending: `creds_file()`, `creds_key()`, `creds_anonymous()`, and `creds()`

* The `preview_email()` function has been removed as email objects created by `compose_email()` now have a print method

* The `.preheader_text` argument in `compose_email()` has been removed because of display issues in certain email clients

## New Features

* Added functions useful for scheduled email sending in RStudio Connect during publication of R Markdown documents: `render_email()`, `render_connect_email()`, `attach_connect_email()`, `blastula_email()`, `suppress_scheduled_email()`, `prepare_rsc_example_files()`

* The `compose_email()` function now has a `header` argument, allowing for content to populate the area above the `body`

* Images embedded in email messages by using `add_image()` are now properly encoded as Base64 in the outgoing message; more MIME types are supported thanks to functions provided by the **mime** package

* The new `add_imgur_image()` function allows us to deploy a local image to Imgur and retrieve the image tag (all in one step); this is useful if you want to use external images in an email message

* We can now build sophisticated email messages with HTML blocks. With these responsive, higher-level components, we can create compose elements within the `header`, `body`, and `footer` components. The functions now available for this system are: `blocks()`, `block_title()`, `block_text()`, `block_spacer()`, `block_articles()`, and `block_social_links()`. Two useful subcomponent functions are `article()` and `social_link()`.

## Notes

* Functions from the `getPass` package are used internally to ask for a password whenever necessary during an interactive session

* Functions from the `keyring` package are used internally to aid in the storage and retrieval of SMTP config/credentials information in the system-wide key-value store (which is cross platform)

# blastula 0.2.1

* Moved local image URI data into list component, where references to image data are through CIDs

* Implemented internal functions for Base64-encoding of local images, removing dependency on knitr

# blastula 0.2.0

* Removed dependencies on Java; reworked `send_email_out()` uses a cross-platform binary to send email message via SMTP

* Added the `prepare_test_message()` function to create test email messages for testing purposes

* Added a `provider` argument to the `create_email_creds_file()` function to more easily configure SMTP settings in a credentials file

* Added the function `add_readable_time()` to allow for easy insertion of a readable version of the present time in a message

# blastula 0.1

* Added functions to compose (`compose_email()`), preview (`preview_email()`), and send (`send_email_out()` and `send_by_mailgun()`) HTML email messages 

* Added helper functions for inserting HTML fragments into the message body (`add_cta_button()`, `add_image()`, and `add_ggplot()`)

* Added a function to help generate a secret credentials file for sending email through SMTP
