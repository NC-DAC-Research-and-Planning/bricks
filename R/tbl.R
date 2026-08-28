#' Reference a Databricks table lazily
#'
#' Returns a lazy dplyr/dbplyr (or sparklyr) table. Pipe further dplyr verbs
#' onto it and they push down to Databricks compute; nothing is pulled locally
#' until [bricks_collect()].
#'
#' @param con A `bricks_con` from [bricks_connect()].
#' @param table A table identifier: `"catalog.schema.table"` (also accepts
#'   `"schema.table"` or `"table"`). Ignored on the `"csv"` backend, which
#'   returns the extract.
#' @return A lazy `tbl`, or a local [tibble][tibble::tibble] for the `"csv"`
#'   backend.
#' @seealso [bricks_sql()], [bricks_collect()]
#' @export
#' @examples
#' \dontrun{
#' con <- bricks_connect()
#' bricks_tbl(con, "main.default.diamonds") |>
#'   dplyr::filter(cut == "Ideal") |>
#'   bricks_collect()
#' }
bricks_tbl <- function(con, table) {
  stopifnot(inherits(con, "bricks_con"))
  switch(con$backend,
    odbc               = ,
    databricks         = ,
    databricks_connect = dplyr::tbl(con$con, .bricks_table_ident(table)),
    csv                = .bricks_csv(con)
  )
}

#' Run a SQL query lazily
#'
#' @inheritParams bricks_tbl
#' @param sql A SQL string. Ignored on the `"csv"` backend, which returns the
#'   extract.
#' @return A lazy `tbl`, or a local [tibble][tibble::tibble] for the `"csv"`
#'   backend.
#' @seealso [bricks_tbl()], [bricks_collect()]
#' @export
#' @examples
#' \dontrun{
#' con <- bricks_connect()
#' bricks_sql(con, "select count(*) as n from main.default.diamonds") |>
#'   bricks_collect()
#' }
bricks_sql <- function(con, sql) {
  stopifnot(inherits(con, "bricks_con"))
  switch(con$backend,
    odbc               = dplyr::tbl(con$con, dbplyr::sql(sql)),
    databricks         = ,
    databricks_connect = sparklyr::sdf_sql(con$con, sql),
    csv                = .bricks_csv(con)
  )
}

# Turn "catalog.schema.table" (or fewer parts) into a dbplyr identifier.
.bricks_table_ident <- function(table) {
  parts <- strsplit(table, ".", fixed = TRUE)[[1]]
  switch(as.character(length(parts)),
    "3" = dbplyr::in_catalog(parts[1], parts[2], parts[3]),
    "2" = dbplyr::in_schema(parts[1], parts[2]),
    "1" = table,
    rlang::abort(sprintf("Cannot parse table identifier: '%s'.", table))
  )
}

# Read the CSV extract behind a csv-backed bricks_con.
.bricks_csv <- function(con) {
  rlang::check_installed("readr", "for the csv backend.")
  readr::read_csv(con$con, show_col_types = FALSE)
}
