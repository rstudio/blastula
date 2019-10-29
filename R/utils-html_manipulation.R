# Like stringr::str_match_all, but instead of returning the matched and captured
# substrings, it returns the positions of those matches and captures.
#
# The returned value is either NULL for no match, or a list, with one element for
# each match. Each element is a matrix with a `start` and `end` row. The first
# column gives the location for the entire match, and the remaining columns are
# for capturing groups.
#
# Important Note: The `end` positions refer to the position AFTER the end of the
# match/capture. Don't use this value with substr, instead use it with substr2.
#
# > find_all("1ab2cd", "([a-z])[a-z]")
# [[1]]
#
# start 2 2
# end   4 3
#
# [[2]]
#
# start 5 5
# end   7 6
#
find_all <- function(string,
                     pattern,
                     ignore_case = FALSE) {

  match <- gregexpr(pattern, string, perl = TRUE, ignore.case = ignore_case)[[1]]
  match_start <- as.integer(match)
  match_start <- ifelse(match_start <= 0, NA, match_start)

  attrs <- attributes(match)
  match_end <- match_start + attrs[["match.length"]]

  capture_start <- attrs[["capture.start"]]
  capture_length <- attrs[["capture.length"]]

  # If there are no capturing groups, these attributes don't exist
  if (is.null(capture_start)) {
    capture_start <- matrix(integer(0), nrow = length(match_start), ncol = 0)
    capture_length <- matrix(integer(0), nrow = length(match_start), ncol = 0)
  }
  capture_start <- ifelse(capture_start <= 0, NA, capture_start)
  capture_end <- capture_start + capture_length

  if (length(match_start) == 1 && is.na(match_start)) {
    return(NULL)
  }

  lapply(seq_along(match_start), function(i) {
    mtx <- rbind(
      c(match_start[[i]], capture_start[i,]),
      c(match_end[[i]], capture_end[i,])
    )
    rownames(mtx) <- c("start", "end")
    mtx
  })
}

# Similar to stringr::str_replace_all, except the replacement is
# given dynamically by a function. `func` should take a single
# string as an argument, and either return a single string or
# NULL.
gfsub <- function(string,
                  pattern,
                  func,
                  ignore_case = FALSE) {

  matches <- find_all(string, pattern, ignore_case = ignore_case)

  f <- file(open = "w+b", encoding = "UTF-8")
  on.exit(close(f), add = TRUE)

  out <- function(str) {
    bytes <- charToRaw(enc2utf8(str))
    writeBin(bytes, f)
  }
  pos <- 1

  for (match in matches) {
    out(substr2(string, pos, match["start",1]))

    args <- as.list(substr2(string, match["start",], match["end",]))
    replacement <- do.call(func, args)
    if (!is.null(replacement)) {
      out(replacement)
    }

    pos <- match["end",1]
  }

  out(substr2(string, pos, nchar(string) + 1L))
  paste(collapse = "\n", readLines(f, warn = FALSE, encoding = "UTF-8"))
}

# Like substr, but the end position is exclusive (i.e. it points
# to the position just beyond the end of the substring).
substr2 <- function(x,
                    start,
                    end) {

  if (missing(end) && length(start) == 2) {
    end <- start[[2]]
    start <- start[[1]]
  }

  if (length(x) != 1) {
    stop("substr2 can only substring a single value")
  }
  substring(x, start, end - 1)
}

parse_attr <- function(attr = "src='data'  alt    =  \"whatever\"  id = foo") {

  match_list <-
    find_all(attr, "(\\w+)\\s*=(?>\\s*)(?:\"([^\"]*)\"|'([^']*)'|([^\\s]*))")

  transformed_matches <-
    lapply(match_list, function(match) {
      name <- match[,2]
      value <- ifelse(
        !is.na(match[,3]), match[,3],
        ifelse(
          !is.na(match[,4]), match[,4],
          ifelse(
            !is.na(match[,5]), match[,5],
            NA
          )
        )
      )

      # Return a named list with a single element
      value_list <- list(value)
      names(value_list) <- tolower(substr2(attr, name[[1]], name[[2]]))
      value_list
    })

  # Turn a list of named lists, into a single named list
  do.call("c", transformed_matches)
}

#' @param tag The HTML of a single begin tag (or self-closing tag)
#' @return `NULL` if parsing fails, or a list that includes an `attributes`
#'   element. This element is a named list where the names are HTML attrib
#'   names (like `href` or `src`) and the values are 2-element vectors
#'   representing the start (inclusive) and end (exclusive) positions of
#'   that attribute's body.
#' @noRd
parse_tag <- function(tag) {

  match <- find_all(tag, "^<(?>(\\w+))([^>]*?)(/?)>$")
  if (is.null(match)) {
    return(NULL)
  }

  match <- match[[1]]

  tag_attr <-
    lapply(parse_attr(substr2(tag, match[,3])), function(attr_loc) {

    # Adjust attr offset to be relative to `tag`, not substr2(tag, match[,3])
    attr_loc + (match[1,3] - 1)
  })

  list(attributes = tag_attr)
}

#' @param html An HTML string, possibly containing some tags.
#' @param tag_name The case-insensitive name of a tag whose attrib we want to
#'   replace.
#' @param attr_name The case-insensitive name of an attribute whose values we
#' want to replace. Note that if an attribute appears multiple times on the same
#' tag, we'll only replace the first instance.
#' @param func A function that takes an (HTML escaped) attribute string and
#'   returns an (HTML escaped) attribute string. Please be especially careful
#'   not to returned unescaped single or double quotes.
#' @noRd
replace_attr <- function(html,
                         tag_name,
                         attr_name,
                         func) {

  stopifnot(grepl("^[a-zA-Z]\\w*$", tag_name))

  pattern <- paste0("<", tag_name, "(?!\\w)[^>]*>")

  gfsub(html, pattern, ignore_case = TRUE, function(tag_html) {

    tag <- parse_tag(tag_html)
    attr_loc <- tag$attributes[[attr_name]]

    if (is.null(attr_loc)) {
      # No change
      return(tag_html)
    }

    pre <- substr2(tag_html, 1, attr_loc[["start"]])
    attr_val <- substr2(tag_html, attr_loc)
    post <- substr2(tag_html, attr_loc[["end"]], nchar(tag_html) + 1L)

    paste0(
      pre,
      htmltools::htmlEscape(func(html_unescape(attr_val)), attribute = TRUE),
      post
    )
  })
}

# replace_attr(
#   html = "<img src='whatever'> <div hi=bye> <IMG SRC='baz' alt=\"hi\"/>  <!-- <img src='no'> --> ]",
#   tag_name = "img",
#   attr_name = "src",
#   func = toupper
# )

#' Convert HTML decoded, but not URI escaped, file URI to a filepath
#' @noRd
file_uri_to_filepath <- function(src) {

  m <- stringr::str_match(src, "^[Ff][Ii][Ll][Ee]://(([A-Za-z]:)?/.*)$")

  if (is.na(m[1,1])) {
    stop("Invalid file URI")
  }

  path <- m[1,2]
  utils::URLdecode(path)
}

#' Convert HTML decoded, but not URI escaped, file URI to an absolute path,
#' possibly by resolving the path relative to basedir.
#' @noRd
src_to_filepath <- function(src,
                            basedir) {

  src <- utils::URLdecode(src)
  fs::path_abs(src, basedir)
}

src_to_datauri <- function(src,
                           basedir) {

  if (grepl("^https?:", src, ignore.case = TRUE, perl = TRUE)) {
    return(src)
  } else if (grepl("^data:", src, ignore.case = TRUE, perl = TRUE)) {
    return(src)
  } else if (grepl("^file:", src, ignore.case = TRUE, perl = TRUE)) {
    full_path <- file_uri_to_filepath(src)
  } else {
    full_path <- src_to_filepath(src, basedir)
  }

  if (file.exists(full_path)) {

    type <- mime::guess_type(full_path, unknown = NA, empty = NA)
    if (is.na(type)) {
      return(src)
    }

    f <- file(full_path, open = "rb")
    on.exit(close(f), add = TRUE)
    b64 <- base64enc::base64encode(f, 0)
    paste0("data:", type, ";base64,", b64)

  } else {
    src
  }
}

inline_images <- function(html_file) {

  basedir <- dirname(html_file)
  html <- paste(collapse = "\n", readLines(html_file, warn = FALSE))

  replace_attr(html, tag_name = "img", attr_name = "src", function(src) {
    src <- src_to_datauri(src, basedir)
    src
  })
}

cid_counter <- function(prefix,
                        initial_value = 1L) {

  idx <- initial_value - 1L

  function() {
    idx <<- idx + 1L
    paste0(prefix, idx)
  }
}

# Reads in the specified HTML file, and replaces any images found
# (either data URI or relative file references) with cid references.
cid_images <- function(html_file,
                       next_cid = cid_counter("img")) {

  idx <- 0L

  next_cid <- function(content_type) {
    idx <<- idx + 1L
    paste0("img", idx, ".", content_type)
  }

  basedir <- dirname(html_file)
  html <- paste(collapse = "\n", readLines(html_file, warn = FALSE))

  html_data_uri <-
    replace_attr(html, tag_name = "img", attr_name = "src", function(src) {
      src_to_datauri(src, basedir)
    })

  images <- new.env(parent = emptyenv())

  html_cid <-
    replace_attr(html_data_uri, tag_name = "img", attr_name = "src", function(src) {
    m <- stringr::str_match(src, "^data:image/(\\w+);(base64,)(.+)")
    data <- m[1,4]
    content_type <- m[1,2]
    if (is.na(data)) {
      src
    } else {
      cid <- next_cid(content_type = content_type)
      images[[cid]] <- structure(
        data,
        "content_type" = paste0("image/", content_type)
      )
      paste0("cid:", cid)
    }
  })

  structure(
    class = c("blastula_message", "email_message"),
    list(
      html_str = html_cid,
      html_html = htmltools::HTML(html_data_uri),
      images = as.list(images)
    )
  )
}

decode_hex <- function(hex) {

  if (nchar(hex) %% 2 == 1) {
    hex <- paste0("0", hex)
  }
  chars <- strsplit(hex, "")[[1]]
  left <- chars[c(TRUE, FALSE)]
  right <- chars[c(FALSE, TRUE)]
  values <- strtoi(paste0(left, right), 16)
  str <- rawToChar(as.raw(values))
  Encoding(str) <- "UTF-8"
  str
}

html_unescape <- function(html) {

  gfsub(html, "&#x([0-9a-f]+);|&#([0-9]+);|&([a-z0-9]+);", ignore_case = TRUE,
    function(entity, hex, dec, named) {
      if (!is.na(hex)) {
        decode_hex(hex)
      } else if (!is.na(dec)) {
        decode_hex(sprintf("%x", strtoi(dec, 10)))
      } else if (!is.na(named)) {
        switch(named,
          amp = "&",
          lt = "<",
          gt = ">",
          quot = "\"",
          {
            str <- html_entities[[entity]]
            if (!is.null(str)) {
              str
            } else {
              entity # not found
            }
          }
        )
      }
    }
  )
}

process_text <- function(text) {

  # If text has been passed in with `md()`, collapse
  # that vector with "\n" and convert to HTML with
  # `commonmark::markdown_html()`
  if (inherits(text, "from_markdown")) {

    text <-
      text %>%
      as.character() %>%
      paste(collapse = "\n") %>%
      commonmark::markdown_html()

    return(text)
  }

  # If text isn't `from_markdown`, it should inherit
  # from `character`; if not, stop the function
  if (!inherits(text, "character")) {

    stop("The input text must be of class `\"character\"`.",
         call. = FALSE)
  }

  # Fashion plain text into HTML
  text %>%
    paste(collapse = "\n") %>%
    htmltools::htmlEscape() %>%
    tidy_gsub("\n\n", "<br />\n")
}
