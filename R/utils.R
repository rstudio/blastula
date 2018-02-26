# SMTP providers with settings
smtp_settings <- function() {
  dplyr::tribble(
    ~short_name,   ~server,                    ~port, ~use_ssl, ~use_tls, ~authenticate, ~user,   ~long_name,
    "gmail",       "smtp.gmail.com",           465,   TRUE,     FALSE,    TRUE,          "email", "Gmail",
    "outlook",     "smtp-mail.outlook.com",    587,   TRUE,     TRUE,     TRUE,          "email", "Outlook.com",
    "office365",   "smtp.office365.com",       587,   FALSE,    TRUE,     TRUE,          "email", "Office365.com",
    "yahoo",       "smtp.mail.yahoo.com",      465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo",
    "yahoo_uk",    "smtp.mail.yahoo.co.uk",    465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo UK",
    "yahoo_au_nz", "smtp.mail.yahoo.au",       465,   TRUE,     FALSE,    TRUE,          "email", "Yahoo AU/NZ",
    "aol",         "smtp.aol.com",             587,   TRUE,     TRUE,     TRUE,          "user",  "AOL",
    "mail_com",    "smtp.mail.com",            587,   FALSE,    TRUE,     TRUE,          "email", "Mail.com",
    "gmx",         "smtp.gmx.com",             465,   TRUE,     FALSE,    TRUE,          "email", "GMX",
    "att",         "smtp.att.yahoo.com",       465,   TRUE,     FALSE,    TRUE,          "email", "AT&T",
    "verizon",     "outgoing.verizon.net",     587,   FALSE,    FALSE,    TRUE,          "email", "Verizon",
    "usa_net",     "smtp.postoffice.net",      465,   TRUE,     FALSE,    TRUE,          "email", "USA.NET",
    "ntl",         "smtp.ntlworld.com",        465,   TRUE,     FALSE,    TRUE,          "email", "NTL World",
    "bt",          "smtp.btconnect.com",       25,    FALSE,    FALSE,    TRUE,          "email", "BT Connect",
    "o2_de",       "mail.o2online.de",         25,    FALSE,    FALSE,    TRUE,          "email", "O2 Deutschland",
    "t_online_de", "securesmtp.t-online.de",   587,   FALSE,    TRUE,     TRUE,          "email", "T-Online Deutschland",
    "vodafone_de", "smtp.vodafone.de",         587,   TRUE,     TRUE,     TRUE,          "email", "Vodafone Deutschland",
    "1and1",       "smtp.1and1.com",           587,   FALSE,    TRUE,     TRUE,          "email", "1&1",
    "1and1_de",    "smtp.1und1.de",            587,   FALSE,    TRUE,     TRUE,          "email", "1&1 Deutschland",
    "shaw",        "mail.shaw.ca",             587,   TRUE,     TRUE,     TRUE,          "user",  "Shaw",
    "fastmail",    "smtp.fastmail.com",        587,   TRUE,     TRUE,     TRUE,          "email", "Fastmail",
  )
}

