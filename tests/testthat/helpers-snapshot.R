pathify <- function(str) {
  tolower(gsub(".", "_", make.names(str), fixed = TRUE))
}

snapshot_path <- function(lbl) {
  test_path("snapshots",
    pathify(get_reporter()$.context),
    paste0(pathify(lbl), ".txt")
  )
}

snapshot <- local({
  seen <- c()

  function(x, print = base::print) {
    lbl <- deparse(substitute(x))
    path <- snapshot_path(lbl)
    if (path %in% seen) {
      stop("The snapshot path '", path, "' cannot be used twice in one run")
    }
    seen <<- c(seen, path)
    if (!dir.exists(dirname(path))) {
      dir.create(dirname(path), recursive = TRUE)
    }
    verify_output(path, {
      print(x)
    })
  }
})

# A uuid_source for generate_rfc2822 that returns deterministic results, so we
# can use snapshot testing
create_uuid_source <- function() {
  i <- -1
  prefix <- "90845e5a-81ce-11ea-aeb2-b32eaa15"
  function() {
    i <<- i + 1
    paste0(prefix, sprintf("%04x", i))
  }
}
