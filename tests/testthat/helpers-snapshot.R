pathify <- function(str) {
  gsub(".", "_", make.names(str), fixed = TRUE)
}

snapshot_path <- function(lbl) {
  test_path("snapshots",
    pathify(get_reporter()$.context),
    paste0(pathify(lbl), ".txt")
  )
}
snapshot <- local({
  seen <- c()

  function(x) {
    lbl <- deparse(substitute(x))
    path <- snapshot_path(lbl)
    if (path %in% seen) {
      stop("The snapshot path '", path, "' cannot be used twice in one run")
    }
    seen <<- c(seen, path)
    if (!dir.exists(dirname(path))) {
      dir.create(dirname(path), recursive = TRUE)
    }
    verify_output(path, print(x))
  }
})
