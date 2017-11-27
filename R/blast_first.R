#' Setup function to get SMTP mail working effectively
#' @description Run this function first to
#' obtain SMTP mailing functionality, which is
#' important for sending out email.
#' @importFrom downloader download
#' @export blast_first

blast_first <- function() {

  # Get the system OS type
  os <-
    switch(
      Sys.info()[['sysname']],
      Windows = "win",
      Linux   = "linux",
      Darwin  = "mac_os")

  # Create `exec` directory
  if (dir.exists(paste0(find.package("blastula"), "/exec")) == FALSE) {
    dir.create(paste0(find.package("blastula"), "/exec"))
  }

  # Download the correct binary
  if (os == "mac_os") {
    downloader::download(
      url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/mac_os/mailsend",
      destfile = paste0(find.package("blastula"), "/exec/mailsend"))
  } else if (os == "win") {
    downloader::download(
      url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/win/mailsend",
      destfile = paste0(find.package("blastula"), "/exec/mailsend"))
  } else if (os == "linux") {
    downloader::download(
      url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/linux/mailsend",
      destfile = paste0(find.package("blastula"), "/exec/mailsend"))
  }

  # Test for presence of the executable
  if (
    file.exists(
      system.file("exec", "mailsend", package = "blastula"))) {

    message("You are now set up to send email messages with the `send_mail_out()` function.")
  }
}
