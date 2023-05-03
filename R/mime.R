# Integration tests (compare vs reference values)
#
# (required)
# Named vs. unnamed recipients (or some named and some unnamed)
#   Recipients with Unicode characters and double-quotes in their display names
# Single vs. multi recipients
# Subject line with unicode characters, double-quotes
# Inline images and file attachments with weird filenames
#   Filenames with spaces
#   Filenames with unicode characters (on both Windows and POSIX)
#   Filenames with angle brackets < >
#   With various extensions

# Unit tests
#
# (required)
# format_rfc2822_date
#   With various timezones
#     +0000
#     Positive offset
#     Negative offset
#   Day values <10 (should NOT have leading 0)
#   Month values <10 (should NOT have leading 0)
#   Works with Date, POSIXct, and POSIXlt values
# (optional)
# header_quoted
#   Unicode characters (should encode if encode_unicode, else error)
#   Newlines, non-character should all throw errors
# header_unstructured
#   Unicode characters (should encode if encode_unicode, else error)
#   Newlines, >1 element, non-character should all throw errors
# encode_qp
#   Long lines should be wrapped at <=76 characters
#   Compare to a reference value



# Turns a blastula email object into an RFC2822 message
generate_rfc2822 <- function(
    eml,
    date = Sys.time(),
    subject = NULL,
    from = NULL,
    to = NULL,
    cc = NULL,
    con = NULL,
    uuid_source = NULL
) {

  if (is.null(uuid_source)) {
    # Do this instead of (the more obvious) `uuid_source = uuid::UUIDgenerate`
    # in the parameter declaration, to stop R CMD check complaining that uuid
    # isn't used
    uuid_source <- uuid::UUIDgenerate
  }

  stopifnot(inherits(eml, "blastula_message"))

  headers <-
    list(
      "MIME-Version" = "1.0",
      "Date" = format_rfc2822_date(date),
      "Message-ID" = paste0("<", uuid_source(), "@blastula.local>"),
      "Subject" = header_unstructured(subject, "Subject", encode_unicode = TRUE),
      "From" = format_rfc2822_addr(from, "From"),
      "To" = format_rfc2822_addr_list(to, "To"),
      "Cc" = format_rfc2822_addr_list(cc, "Cc")
    )

  images <-
    mapply(
      names(eml$images),
      eml$images,
      FUN = function(filename, data) {
        mime_part(
          attr(data, "content_type", exact = TRUE) %||% mime::guess_type(filename),
          headers = list(
            # TODO: escape
            "Content-Disposition" = "inline",
            "Content-ID" = paste0("<", filename, ">"),
            "X-Attachment-Id" = filename
          ),
          content = base64enc::base64decode(data)
        )
      },
      SIMPLIFY = FALSE
    )

  attachments <-
    lapply(
      eml$attachments,
      FUN = function(attachment) {

        raw_bytes <-
          readBin(
            attachment$file_path,
            what = "raw",
            n = file.info(attachment$file_path)$size
          )

        quoted_filename <-
          header_quoted(
            attachment$filename,
            str = "Filename",
            encode_unicode = TRUE
          )

        content_id <- tidy_gsub(uuid_source(), "-", "")

        mime_part(
          content_type = sprintf(
            "%s; name=%s",
            attachment$content_type,
            quoted_filename
          ),
          headers = list(
            "Content-Disposition" = sprintf(
              "%s; filename=%s",
              attachment$disposition,
              quoted_filename
            ),
            "Content-ID" = paste0("<", content_id, ">"),
            "X-Attachment-Id" = content_id
          ),
          content = raw_bytes
        )
  })


  msg <-
    mime_multipart(
      content_type = "multipart/related",
      headers = if (length(attachments) == 0) headers else list(),
      body_parts = rlang::list2(
        mime_part(
          content_type = "text/html; charset=utf-8",
          content = eml$html_str
        ),
        !!!images
      ),
      boundary = uuid_source()
    )

  # This is necessary for attachments to display correctly on
  # iOS Mail in particular (other clients are more relaxed
  # about showing attachments whenever available)
  if (length(attachments) > 0) {

    msg <-
      mime_multipart(
        content_type = "multipart/mixed",
        headers = headers,
        body_parts = rlang::list2(
          msg,
          !!!attachments
        ),
        boundary = uuid_source()
      )
  }

  if (is.null(con)) {

    f <- file(open = "w+b")
    on.exit(close(f), add = TRUE)

    write_mime(create_output_sink(f), msg)
    str <- readChar(f, seek(f), useBytes = TRUE)
    Encoding(str) <- "UTF-8"
    str

  } else {

    if (is.character(con)) {
      con <- file(con, open = "w+b")
      on.exit(close(con), add = TRUE)
    }

    write_mime(create_output_sink(con), msg)
    invisible()
  }
}

# Constant representing canonical CRLF
crlf <- "\r\n"

# Constructor for a mime message. Can be nested in mime_multipart.
mime_part <- function(
    content_type,
    headers = list(),
    content
) {

  if (is.raw(content)) {
    encoding <- "base64"
  } else if (is.character(content)) {
    encoding <- "quoted-printable"
  } else {
    content <- as.character(content)
    encoding <- "quoted-printable"
  }

  structure(
    class = "mime_part",
    list(
      headers = rlang::list2(
        "Content-Type" = content_type,
        "Content-Transfer-Encoding" = encoding,
        !!!headers
      ),
      encoding = encoding,
      content = content
    )
  )
}

# Constructor for a multipart mime message. Can be nested in mime_multipart.
mime_multipart <- function(
    content_type = c("multipart/alternative", "multipart/related", "multipart/mixed"),
    headers = list(),
    body_parts,
    boundary,
    preamble = NULL,
    epilogue = NULL
) {

  structure(
    class = "mime_multipart",
    list(
      headers = rlang::list2(
        !!!headers,
        "Content-Type" = paste0(content_type, "; boundary=\"", boundary, "\"")
      ),
      body_parts = body_parts,
      boundary = boundary,
      premable = preamble,
      epilogue = epilogue
    )
  )
}

# In the functions below, the `out` parameter is expected to be a function that
# takes any number of arguments, and outputs them to a binary-mode connection,
# with no separating spaces (unlike the default for `cat`). Use
# `create_output_sink` to create such a function for a binary-mode connection.
create_output_sink <- function(con) {
  function(...) {
    cat(..., file = con, sep = "", append = TRUE)
  }
}

# @param out Output function
# @param headers Named list of headers
write_headers <- function(out, headers) {
  mapply(names(headers), headers, FUN = function(header_name, header_value) {
    if (!is.null(header_value)) {
      header_unstructured(header_value, header_name)
      out(header_name, ": ", header_value, crlf)
    }
  })
  out(crlf)
}

# @param out Output function
# @param boundary A bare boundary string (not incl leading "--")
# @param end Whether this is a terminating boundary
write_boundary <- function(out, boundary, end = FALSE) {
  out("--", boundary)
  if (end) {
    out("--")
  }
  out(crlf)
}

write_mime <- function(out, x) {
  UseMethod("write_mime", x)
}

write_mime.mime_part <- function(out, x) {
  write_headers(out, x$headers)

  encoder <- switch(x$encoding,
    "quoted-printable" = encode_qp,
    "base64" = function(x) {
      # Wrap at 76 chars
      base64enc::base64encode(x, linewidth = 76, newline = "\r\n")
    }
  )

  out(encoder(x$content))
  out(crlf)
}

write_mime.mime_multipart <- function(out, x) {

  # headers, body_parts, boundary, preamble, epilogue
  write_headers(out, x$headers)
  out(x$preamble)

  for (body_part in x$body_parts) {

    write_boundary(out, x$boundary)
    write_mime(out, body_part)
  }

  write_boundary(out, x$boundary, end = TRUE)
  out(x$epilogue)
}

write_mime.default <- function(out, x) {
  stop("Not implemented for ", class(x))
}

encode_qp <- function(str) {

  str <- enc2utf8(str)

  # Normalize line endings, as required by RFC2045 Section 6.7.4
  str <- tidy_gsub(str, "\\r?\\n", "\r\n")

  # Convert to bytes, easier to deal with
  bytes <- charToRaw(str)

  # Section 6.7.1-6.7.4
  # Also encoding 0x2E ('.') to guard against dot stuffing issues
  needs_encode <-
    !(bytes %in% as.raw(c(0x09, 0x0A, 0x0D, 0x20, 0x21:0x2D, 0x2F:0x3C, 0x3E:0x7E)))

  enc_chars <- character(length(bytes))
  enc_chars[needs_encode] <- sprintf("=%02X", as.integer(bytes[needs_encode]))
  enc_chars[!needs_encode] <- strsplit(rawToChar(bytes[!needs_encode]), "")[[1]]

  f <- file(open = "w+b")
  on.exit(close(f), add = TRUE)
  out <- create_output_sink(f)

  cur_line_len <- 0L

  for (chunk in enc_chars) {

    # See if soft line break is required (Section 6.7.5)
    if (identical(chunk, "\n")) {

      cur_line_len <- -1L

    } else if (cur_line_len + nchar(chunk) >= 76) {

      out("=\r\n")
      cur_line_len <- 0L
    }

    out(chunk)
    cur_line_len <- cur_line_len + nchar(chunk)
  }

  # Ensure that trailing spaces are not ignored
  out("=\r\n\r\n")

  paste(collapse = "\r\n", readLines(f, warn = FALSE, encoding = "UTF-8"))
}

format_rfc2822_date <- function(date) {

  # Thu, 24 Oct 2019 08:17:43 -0700

  dc <- as.POSIXlt(date)

  dow <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[[dc$wday + 1L]]

  day <- format(dc$mday)

  month <-
    c(
      "Jan", "Feb", "Mar",
      "Apr", "May", "Jun",
      "Jul", "Aug", "Sep",
      "Oct", "Nov", "Dec"
    )[[dc$mon + 1L]]

  year <- sprintf("%04d", 1900L + dc$year)

  time <- sprintf("%02d:%02d:%02d", dc$hour, dc$min, floor(dc$sec))

  tz <-
    if (is.null(dc$gmtoff) || dc$gmtoff == 0) {
      "+0000"
    } else if (dc$gmtoff < 0) {
      sprintf("%05d", dc$gmtoff / 3600 * 100)
    } else {
      sprintf("+%04d", dc$gmtoff / 3600 * 100)
    }

  paste0(dow, ", ", paste(day, month, year, time, tz))
}

format_rfc2822_addr <- function(addr, fieldname) {

  # To trigger errors if called without fieldname, even if length(addr) == 1
  force(fieldname)

  if (is.null(addr)) {
    return(NULL)
  }

  if (length(addr) != 1) {
    stop("The '", fieldname, "' field must contain exactly one email address")
  }

  format_rfc2822_addr_list(addr, fieldname)
}

format_rfc2822_addr_list <- function(addrs, fieldname) {

  if (length(addrs) == 0) {
    return(NULL)
  }

  if (is.null(names(addrs))) {
    names(addrs) <- rep.int("", length(addrs))
  }

  paste0(
    ifelse(
      nzchar(names(addrs)),
      paste0(header_quoted(names(addrs), fieldname, encode_unicode = TRUE), " "),
      ""
    ),
    "<", addrs, ">",
    collapse = ", "
  )
}

# NOTE: str can be length > 1
header_quoted <- function(
    str,
    fieldname,
    encode_unicode = FALSE
) {

  if (length(str) == 0) {
    return(str)
  }

  force(fieldname)

  if (!is.character(str)) {
    stop("The '", fieldname, "' field must be a character vector")
  }

  if (any(grepl("[\r\n]", str))) {
    stop("The '", fieldname, "' field must not contain newlines")
  }

  str <- enc2utf8(str)

  needs_encoding <- grepl("[^\x01-\x7F]", str)

  if (any(needs_encoding)) {

    if (encode_unicode) {

      str[needs_encoding] <-
        vapply(
          str[needs_encoding],
          FUN.VALUE = character(1),
          FUN = function(x) {
            base64enc::base64encode(charToRaw(x), 0)
          }) %>%
        sprintf("=?utf-8?B?%s?=", .)

      return(str)

    } else {

      warning(
        "The '", fieldname, "' field contains impermissible characters, ",
        "please use 7-bit ASCII only"
      )
    }

  } else {

    return(paste0("\"", gsub("([\"\\])", "\\\\\\1", str), "\""))
  }
}

header_unstructured <- function(
    str,
    fieldname,
    encode_unicode = FALSE
) {

  force(fieldname)

  if (is.null(str)) {
    return(NULL)
  }

  if (!is.character(str)) {
    stop("The '", fieldname, "' field must be a character vector")
  }

  if (length(str) != 1) {
    stop("The '", fieldname, "' field must have a single element")
  }

  if (any(grepl("[\r\n]", str))) {
    stop("The '", fieldname, "' field must not contain newlines")
  }

  if (grepl("[^\x01-\x7F]", str)) {

    if (encode_unicode) {
      str <- enc2utf8(str)
      str <- sprintf("=?utf-8?B?%s?=", base64enc::base64encode(charToRaw(str)), 0)

    } else {

      warning(
        "The '", fieldname, "' field contains impermissible characters, ",
        "please use 7-bit ASCII only"
      )
    }
  }

  str
}
