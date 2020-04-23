library(blastula)
library(glue)

# Attribution Information available in `README-attribution.txt`

seb_image <-
  add_image(
    "tests/manual_tests/young_adult_male.jpg",
    alt = "Seb himself",
    align = "center"
  )

company_logo <-
  add_image(
    "tests/manual_tests/msms_logo.png",
    alt = "msms logo",
    width = "50%",
    align = "center"
  )

email <-
  compose_email(
    body = md(glue("

  ### Welcome Sebastian (Seb) Génin to Our Team! &#x263A; &#x263A; &#x263A;

  {seb_image}

  Sebastian Génin has just joined Our Team! We are thrilled like \\
  you would not believe at this outcome. Do you know Seb? If you don't, \\
  well, you'll want to because he is quite an interesting person and \\
  he can talk about virtually any topic. One hell of a conversationalist, \\
  you might say.

  Sebastian will be joining the `2+2=5` team (now relocating in between the \\
  *Accountaholics* and the *Product Pushers* on 9). He'll help in doing the \\
  (nearly) impossible, which is what this fine team is generally known for. \\
  If you're in the general vicinity, please stop over and say 'Yo, Seb!'

  &nbsp;&nbsp;&nbsp;&mdash;R.M.D. \\
")),
    footer = md(glue("

  {company_logo}

  Multi-Systems Merchant Services<br>
  2703 Hoffman Avenue, 8th Fl.<br>
  New York, New York 10013<br>
  917-882-8946<br>
")))

email
