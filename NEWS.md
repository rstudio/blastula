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
