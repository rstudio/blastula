#' Create an HTML table object that can be styled
#'
#' Create an HTML table object such that we can
#' perform styling transformations before transforming
#' the entire object to an HTML fragment (for
#' inclusion into the email message body). This is
#' the first step in the \code{build_html_table()} ->
#' \code{add_column_style()} -> \code{emit_html()}
#' pattern.
#' @param tbl a \code{data.frame} object or a
#' tibble.
#' @return an HTML table object.
#' @importFrom purrr map_chr map_df
#' @importFrom dplyr as_tibble rename mutate bind_rows
#' @export
build_html_table <- function(tbl) {

  tbl_classes <-
    seq(ncol(tbl)) %>%
    purrr::map_chr(
      .f = function(x) tbl[[x]] %>% class())

  table_heading <-
    tbl %>%
    names() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(content = value) %>%
    dplyr::mutate(type = "character") %>%
    dplyr::mutate(row = 0) %>%
    dplyr::mutate(column = 1:nrow(.))

  table_body <-
    seq(nrow(tbl)) %>%
    purrr::map_df(
      .f = function(x) {
        tbl[x, ] %>% t() %>%
          dplyr::as_tibble() %>%
          dplyr::rename(content = V1) %>%
          dplyr::mutate(type = tbl_classes) %>%
          dplyr::mutate(row = x) %>%
          dplyr::mutate(column = 1:nrow(.))})

  dplyr::bind_rows(
    table_heading,
    table_body)
}

#' Add inline CSS styles to columns in an HTML table object
#'
#' Modify column styles for a table within an HTML
#' table object. This is part of an intermediate set
#' of step in the \code{build_html_table()} ->
#' \code{add_column_style()} -> \code{emit_html()}
#' pattern.
#' @param html_tbl an HTML table object that is
#' created using the \code{build_html_table()}
#' function.
#' @param columns an optional vector of column index
#' numbers that are to be targeted for transformation.
#' If nothing is provided here then the style will
#' be applied to all columns.
#' @param property the CSS style property that is
#' to the added.
#' @param values values for the CSS style property.
#' This should be provided as a single-length
#' character vector, where multiple values for the
#' same property are separated by a space character.
#' @return an HTML table object.
#' @importFrom dplyr pull bind_rows filter mutate arrange
#' @importFrom rlang UQ
#' @export
add_column_style <- function(html_tbl,
                             columns = NULL,
                             property,
                             values) {

  if (is.null(columns)) {
    columns <-
      html_tbl %>%
      dplyr::pull(column) %>%
      unique()
  }

  if (!(property %in% colnames(html_tbl))) {

    html_tbl_style <-
      dplyr::bind_rows(
        html_tbl %>%
          dplyr::filter(column %in% columns) %>%
          dplyr::mutate(rlang::UQ(property) := values),
        html_tbl %>%
          dplyr::filter(!(column %in% columns)) %>%
          dplyr::mutate(rlang::UQ(property) := NA_character_)) %>%
      dplyr::arrange(row, column)
  }

  if (property %in% colnames(html_tbl)) {

    html_tbl_style <-
      dplyr::bind_rows(
        html_tbl %>%
          dplyr::filter(column %in% columns) %>%
          dplyr::mutate(rlang::UQ(property) := values),
        html_tbl %>%
          dplyr::filter(!(column %in% columns))) %>%
      dplyr::arrange(row, column)
  }

  html_tbl_style
}


#' Transform a HTML table object to an HTML fragment
#'
#' Take a suitably styled HTML table object and
#' transform it to an HTML fragment. This is the
#' final step in the \code{build_html_table()} ->
#' \code{add_column_style()} -> \code{emit_html()}
#' pattern.
#' @param html_tbl an HTML table object that is
#' created using the \code{build_html_table()}
#' function.
#' @return an HTML table object.
#' @importFrom dplyr pull mutate filter
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @importFrom purrr map
#' @importFrom rlang squash_chr
#' @export
emit_html <- function(html_tbl) {

  for (i in 5:ncol(html_tbl)) {

    if (i == 5) style_names <- colnames(html_tbl)[5:ncol(html_tbl)]

    for (j in 1:nrow(html_tbl)) {

      html_tbl[j, i] <-
        paste0(colnames(html_tbl)[i], ":", html_tbl[j, i] %>% dplyr::pull(), ";")
    }
  }

  table_content_styles <-
    html_tbl %>%
    tidyr::unite(col = style_attrs, 5:ncol(html_tbl), sep = "") %>%
    dplyr::mutate(style_attrs = case_when(
      row == 0 ~ glue::glue("<th style=\"{style_attrs}\">{content}</th>") %>% as.character(),
      row != 0 ~ glue::glue("<td style=\"{style_attrs}\">{content}</td>") %>% as.character()))


  table_opening_component <-
    "<table style=\"width:100%;\">\n <thead>\n   <tr>\n"

  table_heading_component <-
    table_content_styles %>%
    dplyr::filter(row == 0) %>%
    dplyr::pull(style_attrs) %>%
    paste("   ", ., "\n", collapse = "")

  table_heading_component_2 <-
    " </thead>\n<tbody>\n"

  table_body_component <-
    table_content_styles %>%
    dplyr::filter(row != 0) %>%
    dplyr::pull(row) %>%
    unique() %>%
    purrr::map(.f = function(x) {

      table_content_styles %>%
        dplyr::filter(row == x) %>%
        dplyr::pull(style_attrs) %>%
        paste("   ", ., collapse = "\n") %>%
        paste0("  <tr>\n", ., "\n  </tr>\n")
    })

  table_body_component <-
    table_body_component %>%
    rlang::squash_chr() %>%
    paste(collapse = "")

  table_closing_component <- "</tbody>\n</table>\n"

  paste(
    table_opening_component,
    table_heading_component,
    table_heading_component_2,
    table_body_component,
    table_closing_component,
    collapse = "")
}


#' Helper function for adding a data table
#'
#' Add a data table inside the body of the
#' email with this helper function. Simply
#' provide a \code{data.frame} or tibble
#' object and the function will sensibly
#' style the table during insertion.
#'
#' If more control over the table styling is
#' required, then an alternative is to use the
#' \code{build_html_table()} ->
#' \code{add_column_style()} -> \code{emit_html()}
#' pattern for building and inserting a table.
#' @param tbl a \code{data.frame} object or a
#' tibble.
#' @return a character object with an HTML
#' fragment that can be placed inside the
#' message body wherever the table should
#' appear.
#' @importFrom dplyr filter pull
#' @export
add_table <- function(tbl) {

  html_table <-
    build_html_table(tbl = tbl)

  numeric_columns <-
    html_table %>%
    dplyr::filter(row != 0) %>%
    dplyr::filter(type %in% c("integer", "numeric")) %>%
    dplyr::pull(column) %>%
    unique()

  character_columns <-
    html_table %>%
    dplyr::filter(row != 0) %>%
    dplyr::filter(type == "character") %>%
    dplyr::pull(column) %>%
    unique()

  if (length(numeric_columns) > 0) {

    html_table <-
      html_table %>%
      add_column_style(
        columns = numeric_columns,
        property = "text-align",
        values = "right")
  }

  if (length(character_columns) > 0) {

    html_table <-
      html_table %>%
      add_column_style(
        columns = character_columns,
        property = "text-align",
        values = "left")
  }

  html_table <-
    html_table %>%
    add_column_style(
      property = "padding",
      values = "5px") %>%
    add_column_style(
      property = "border-bottom",
      values = "1px solid #ddd")

  emit_html_table(html_tbl = html_table)
}
