# blastula 0.3.0 (Unreleased)

The blastula package has transitioned to using a new binary for SMTP mailing, and it's provided by the *mailsend-go* project (https://github.com/muquit/mailsend-go). Binaries are cross-platform, available on Windows, macOS (via Homebrew), and Linux (Debian and RPM packages are available).

There are new functions for integrating **blastula** with R Markdown publishing on the RStudio Connect service. And sophisticated email bodies can be generated with *HTML Blocks*.

## Breaking Changes

* `send_email_out()` has been removed; the `smtp_send()` function is its replacement for sending email through an SMTP server

* The `create_email_creds_file()` function has been removed in favor of a suite of functions for storing SMTP credentials (`create_smtp_creds_file()`, `create_smtp_creds_key()`); the following credentials helper functions are available for retrieving credentials during sending: `creds_file()`, `creds_key()`, `creds_anonymous()`, and `creds()`

* The `preview_email()` function has been removed as email objects created by `compose_email()` now have a print method

* The `.preheader_text` argument in `compose_email()` has been removed because of display issues in certain email clients

## New Features

* Added functions useful for scheduled email sending in RStudio Connect during publication of R Markdown documents: `render_email()`, `render_connect_email()`, `attach_connect_email()`, `blastula_email()`, `suppress_scheduled_email()`, `prepare_rsc_example_files()`

* The `compose_email()` function now has a `header` argument, allowing for content to populate the area above the `body`

* We can now build sophisticated email messages with HTML blocks. With these responsive, higher-level components, we can create compose elements within the `header`, `body`, and `footer` components. The functions now available for this system are: `blocks()`, `block_title()`, `block_text()`, `block_spacer()`, `block_articles()`, and `block_social_links()`. Two useful subcomponent functions are `article()` and `social_link()`.

## Notes

* Functions from the `getPass` package are used internally to ask for a password whenever necessary during an interactive session

* Functions from the `keyring` package are used internally to aid in the storage and retrieval of STMP config/credentials information in the system-wide key-value store (which is cross platform)

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
