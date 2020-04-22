library(blastula)
library(glue)

title_banner <- add_image("tests/manual_tests/new_r_package_releases.png") %>%
  gsub("width=\"520\"", "width=\"100%\"", .)

r_packages_gif <- add_image("tests/manual_tests/r_package_updates.gif") %>%
  gsub("width=\"520\"", "width=\"100%\"", .)

cta_github <-
  add_cta_button(
    url = "https://github.com/rich-iannone",
    text = "Check Out the New Releases",
    align = "center"
  )

email <-
  compose_email(
    body = blastula::md(glue::glue("

{title_banner}

Are you ready for this? There are four **R** packages that just received \\
some seriously awesome updates \u2192 **stationaRy**, **pointblank**, \\
**DiagrammeR**, and **blastula**.

{r_packages_gif}

There's so much reason for excitement. Where to begin?

With **stationaRy** you have access to a treasure trove of historical \\
weather data. It goes wide and deep too: lots of stations, decades of \\
measurements, hundreds of met fields. The update makes it easier than ever \\
to get this data in the form you need. \u2600 \u2601

You got data? Of course you do. Ever check the quality of it? You really \\
oughta. The **pointblank** package makes that whole process much easier. \\
It's like **testthat**, but, for data. Plus there are fancier reports \\
in this update! \u2714 \u2714 \u2714

Data is sometimes *graph-y* and **DiagrammeR** can help you work with \\
this type of data. This update stamps out a few nasty bugs adds a few new \\
functions for visualizing graphs. Check it out if you have a graph \\
problem. &xcirc;&#9476;&xcirc;

Finally, **blastula**... what an update! It now features better ways to \\
compose emails, with many more formatting options available than ever \\
before. Whether it is *R Markdown* reports or custom emails (*like this one!*) \\
you want to send, **blastula** is up to the task! &#9993;

<br>

{cta_github}

A lot of great contributors from all over the **R** community helped to \\
make these releases possible. Don't let their good work go to waste, \\
seriously check these packages!

")),
 footer = md("&copy;2020 Richard Iannone")
)

email
