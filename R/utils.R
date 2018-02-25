# SMTP providers with settings
smtp_settings <- function() {
  dplyr::tribble(
    ~short_name,  ~server,                   ~port, ~use_ssl, ~use_tls, ~authenticate, ~long_name,
    "gmail",      "smtp.gmail.com",          465,   TRUE,     FALSE,    TRUE,          "Gmail",
    "outlook",    "smtp-mail.outlook.com",   587,   FALSE,    TRUE,     TRUE,          "Outlook.com",
    "office365",  "smtp.office365.com",      587,   FALSE,    TRUE,     TRUE,          "Office365.com",
  )
}

