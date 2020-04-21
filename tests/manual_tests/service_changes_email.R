library(blastula)
library(glue)

gt_logo_image <-
  add_image("tests/manual_tests/gt_logo.png") %>%
  gsub("width=\"520\"", "width=\"100%\"", .)

gt_logo_image_small <-
  add_image("tests/manual_tests/gt_logo.png") %>%
  gsub("width=\"520\"", "width=\"150\"", .)

order_number <- "0232358329"

email <-
  compose_email(
    body = md(glue::glue(
"
{gt_logo_image}

Order: **{order_number}**

Dear Customer,

Thank you for choosing Glänzend Telekom You will find a recap of your \\
order as well as your next bills' estimate in attachment.

The Self-Install or any changes to your service(s) will allow you to \\
enjoy a veritable *Welt der Aufregung*.

Should you have any questions about your Self-Install or your services, \\
please contact us at 1-800-267-8214.

Thank you for choosing Glänzend Telekom and enjoy your services.

Sincerely,<br>
Your Glänzend Telekom Team

<hr>

Sehr geehrter Kunde,

Vielen Dank, dass Sie sich für die Glänzend Telekom entschieden haben. \\
Sie finden eine Zusammenfassung Ihrer Bestellung sowie den Kostenvoranschlag \\
für Ihre nächsten Rechnungen im Anhang.

Durch die Selbstinstallation oder Änderungen an Ihren Diensten können Sie \\
genieße eine wahre Welt der Erfahrung.

Sollten Sie Fragen zu Ihrer Selbstinstallation oder Ihren Diensten haben, \\
Bitte kontaktieren Sie uns unter 1-800-267-8214.

Vielen Dank, dass Sie sich für die Glänzend Telekom entschieden haben \\
und Ihre Dienste genießen.

Mit freundlichen Grüßen <br>
Ihr Glänzend Telekom Team

")),
    footer = md(glue::glue(
"
{gt_logo_image_small}

[Consult our acceptable usage policy at gt.de](example.com)<br>
This email is for notification purposes only, and is unable to accept \\
replies to the originating address.

[Privacy Policy](example.com) | [Contact us](example.com)<br>
To read our Terms and Conditions, [click here](example.com).<br>

Glänzend Telekom<br>
Bayreuther Strasse 100<br>
Postfach 242<br>
D-15201 Frankfurt<br>
GERMANY<br>
"
)))

email
