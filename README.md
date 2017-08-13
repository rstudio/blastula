<img src="inst/graphics/blastula_diagram.png">

Sometimes we need to send an email message based on the result of some automated analysis process. For those such instances, let's endeavor to send out some pretty-nice-looking HTML email messages. I mean, we have the technology. We can now make these emails a bit easier on the eyes. The **blastula** package makes it easy to sent out HTML emails. We can use **markdown**, we can inject **R** code and objects into our email text. The best way to demonstrate this is to just show an example workflow...

### Sending an email message

Here's an example that shows a basic workflow for composing the message, previewing the content, creating optional on-disk credentials for email, and sending out the message.

So far, the functions in this package are:

- `create_email_creds_file()`: generates an on-disk, serialized file with email credentials
- `compose_email()`: generates the email message content inside the `email_message`-class object
- `preview_email()`: makes the email message created by `compose_email()` viewable in the RStudio Viewer
- `send_email_out()`: sends the `email_message` object to one or more recipients

When you compose an email, you can use character objects in the global workspace and splice those into the message content. Here, I'll create a nicely formatted date/time string (`current_date_time`), and, assign a link to an web image to an object (`img_link`).

```r
# Create some character objects in the global workspace

current_date_time <-
  paste0(
    format(Sys.time(), "%A, %B "),
    format(Sys.time(), "%d") %>% as.numeric(),
    ", ",
    format(Sys.time(), "%Y"),
    " at ",
    format(Sys.time(), "%l:%M") %>% trimws(),
    toupper(format(Sys.time(), " %p")),
    format(Sys.time(), " (%Z)"))

img_link <-
  "https://marketplace.canva.com/MAA_AbacFmo/2/0/thumbnail_large/canva-basic-good-vibes-email-header-MAA_AbacFmo.jpg"
```

Now, we can use the `compose_email()` to compose the email! There are two main arguments here, `body` and `footer`. You can supply **markdown** text to each of these. So things like `##`, links, tables, and all other valid **markdown** conventions should render to valid HTML.

Furthermore, string interpolation works by enclosing valid **R** code inside of curly braces. Below the very long link to an image is referenced to the `img_link` object from the global workspace. We can also supply variables in the `compose_email()` function directly. The `{sender}` part references not an object in the global workspace but the named argument `sender = "Mike"` in the function call. (The order of searching is from within the function first, then the global environment.) The `date_time` character object is nicely inserted in the footer of the email.

```r
email_object <-
  compose_email(
    body = "
  ## Hiya! This is an email message. Exciting Right?
  Enjoy it. And this here image:

  ![The alt text]({img_link} \"The title\")
      
  **Yeah!** I seriously hope that you enjoy this \\
  message and the good vibes it will bring to you \\
  and yours.
  
  Peace out,

  {sender}",
    footer = 
  "Brought to you by Smile AG on {current_date_time}",
    sender = "Mike")
```

Some more notes on style are useful here. The `\\` is a helpful line continuation marker. It'll help you break long lines up when composing but won't introduce line breaks or new paragraphs. I recommend formatting like above with few indents so as not to induce the `quote`-type formatting. Any literal quotation marks should be escaped using a single `\`. Blank lines between non-blank lines indicate new paragraphs. And, again, any valid **R** code can be enclosed inside `{...}` (e.g., `{Sys.Date()}`).

After creating the email message, you'll most certainly want to look at it to ensure that the formatting is what you want it to be. This is done with the `preview_email()` function. It's easy to use!

```r
# Preview the email
preview_email(email = email_object)
```

...and this is what I saw:

<img src="inst/graphics/rstudio_preview_email.png">

Looks good. Time to email this. I'd previously set up my email credentials in a file using the `create_email_creds_file()` function. Here's an example of how one might create a creds file as a hidden file in the home directory (`~`). 

```r
# Create a credentials file to facilitate
# the sending of email messages
create_email_creds_file(
  file = "~/.email_file",
  sender = "correspondences@blastula.org",
  host = "smtp.blastula.org",
  port = 465,
  user = "have_a@blastula.org",
  password = "************")
```

Having generated that file, you can use the `send_email_out()` function to send the email. I sent the email just to myself but do note that the `recipients` argument can accept a vector of email addresses for mass mailings.

```r
send_email_out(
  message = email_object,
  from = "mike@smile.de",
  recipients = "riannone@me.com",
  subject = "This is NOT junk mail.",
  creds_file = "email_creds")
```

Oddly enough, this message did appear in my Junk Mail folder. I fished it out, and this is how it appears:

<img src="inst/graphics/email_message.png">

Which is great. The underlying HTML/CSS is meant to display properly across a wide range of email clients and webmail services.

### Installation of the package

**blastula** is used in an **R** environment. If you don't have an **R** installation, it can be obtained from the [**Comprehensive R Archive Network (CRAN)**](https://cran.r-project.org/).

You can install the development version of **blastula** from **GitHub** using the **devtools** package.

```r
devtools::install_github("rich-iannone/blastula")
```

If you encounter a bug, have usage questions, or want to share ideas to make this package better, feel free to file an issue at [github](https://github.com/rich-iannone/blastula/issues).
