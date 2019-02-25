#' Setup function to get SMTP mail working effectively
#'
#' Run this function first to obtain SMTP mailing functionality, which is
#' important for sending out email.
#' @importFrom downloader download
#' @export
blast_first <- function() {

  # Get the system OS type
  os <-
    switch(
      Sys.info()[['sysname']],
      Windows = "win",
      Linux   = "linux",
      Darwin  = "mac_os")

  # Create `exec` directory
  if (dir.exists(file.path(find.package("blastula"), "exec")) == FALSE) {
    dir.create(file.path(find.package("blastula"), "exec"))
  }

  # Download the correct binary
  if (os == "mac_os") {

    suppressMessages(
      downloader::download(
        url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/mac_os/mailsend",
        destfile = file.path(find.package("blastula"), "exec", "mailsend"),
        quiet = TRUE)
    )

  } else if (os == "win") {

    suppressMessages(
      downloader::download(
        url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/win/mailsend",
        destfile = file.path(find.package("blastula"), "exec", "mailsend"),
        quiet = TRUE)
    )

  } else if (os == "linux") {

    suppressMessages(
      downloader::download(
        url = "https://raw.githubusercontent.com/rich-iannone/blastula/master/inst/linux/mailsend",
        destfile = file.path(find.package("blastula"), "exec", "mailsend"),
        quiet = TRUE)
    )
  }
}
