#' Open a backend-agnostic Databricks connection
#'
#' Opens a connection using the resolved [bricks_backend()] (or an explicit
#' `backend`) and wraps it in a lightweight `bricks_con` object that the other
#' `bricks_*` verbs dispatch on. Pair every connection with
#' [bricks_disconnect()], typically via [on.exit()].
#'
#' @param backend One of `"odbc"`, `"databricks"`, `"databricks_connect"`, or
#'   `"csv"`. Defaults to [bricks_backend()].
#' @param path Path to a CSV extract. Required when `backend = "csv"`, ignored
#'   otherwise.
#' @param ... Passed to the underlying connector, e.g. `cluster_id` or
#'   `serverless` for the Spark backends (see [databricks_spark_connect()]).
#' @return A `bricks_con` object.
#' @seealso [bricks_tbl()], [bricks_sql()], [bricks_collect()],
#'   [bricks_disconnect()]
#' @export
#' @examples
#' # A CSV-backed connection needs no live workspace:
#' con <- bricks_connect("csv", path = "extract.csv")
#' con
#'
#' \dontrun{
#' con <- bricks_connect()
#' on.exit(bricks_disconnect(con), add = TRUE)
#' bricks_sql(con, "select 1 as x") |> bricks_collect()
#' }
bricks_connect <- function(backend = bricks_backend(), path = NULL, ...) {
  backend <- match.arg(backend, .bricks_backends)
  con <- switch(backend,
    odbc               = databricks_connect(...),
    databricks         = databricks_spark_connect(method = "databricks", ...),
    databricks_connect = databricks_spark_connect(method = "databricks_connect", ...),
    csv                = {
      if (is.null(path) || !nzchar(path)) {
        rlang::abort(
          "`path` is required when `backend = \"csv\"`.",
          class = "bricks_error_missing_path"
        )
      }
      path
    }
  )
  structure(list(con = con, backend = backend), class = "bricks_con")
}

#' @export
print.bricks_con <- function(x, ...) {
  cat(sprintf("<bricks_con: %s>\n", x$backend))
  invisible(x)
}

#' Close a bricks connection
#'
#' Dispatches to the right teardown for the connection's backend:
#' [DBI::dbDisconnect()] for `"odbc"` and [sparklyr::spark_disconnect()] for the
#' Spark backends. A no-op for the `"csv"` backend and for any object that is
#' not a `bricks_con`, so it is always safe to call from [on.exit()].
#'
#' @param con A `bricks_con` from [bricks_connect()].
#' @return `NULL`, invisibly.
#' @seealso [bricks_connect()]
#' @export
bricks_disconnect <- function(con) {
  if (!inherits(con, "bricks_con")) {
    return(invisible(NULL))
  }
  switch(con$backend,
    odbc               = DBI::dbDisconnect(con$con),
    databricks         = ,
    databricks_connect = sparklyr::spark_disconnect(con$con),
    csv                = NULL
  )
  invisible(NULL)
}


# ---- low-level connection builders -----------------------------------------
# bricks_connect() dispatches to these. Call them directly only when you need a
# backend-specific handle, e.g. a raw DBI connection or a Spark session opened
# with custom connect arguments.

#' Connect to a Databricks SQL warehouse (DBI/odbc)
#'
#' Opens a [DBI][DBI::DBI-package] connection to a Databricks SQL warehouse
#' through the odbc driver. Computation happens locally, after rows are pulled.
#'
#' @details
#' Configuration is read from the environment (for example `~/.Renviron`):
#'
#' * `DATABRICKS_HOST` --- the workspace URL, e.g. `https://adb-...`.
#' * `DATABRICKS_WAREHOUSE_ID` --- the SQL warehouse id.
#'
#' Authentication is resolved by [odbc::databricks()]: a `DATABRICKS_TOKEN`
#' personal access token when set, otherwise the OAuth token from the matching
#' Databricks CLI profile.
#'
#' @param warehouse_id SQL warehouse id. Defaults to the
#'   `DATABRICKS_WAREHOUSE_ID` environment variable.
#' @return A [DBI][DBI::DBI-package] connection object.
#' @seealso [bricks_connect()], [databricks_spark_connect()]
#' @export
#' @examples
#' \dontrun{
#' con <- databricks_connect()
#' DBI::dbGetQuery(con, "select 1")
#' DBI::dbDisconnect(con)
#' }
databricks_connect <- function(warehouse_id = Sys.getenv("DATABRICKS_WAREHOUSE_ID")) {
  rlang::check_installed(c("DBI", "odbc"), "for the odbc backend.")
  if (!nzchar(warehouse_id)) {
    rlang::abort("`DATABRICKS_WAREHOUSE_ID` is not set (check your .Renviron).")
  }
  # No DATABRICKS_TOKEN check: auth may come from the CLI profile's OAuth token.
  # If neither that nor a PAT is available, odbc raises a clear "failed to detect
  # ambient Databricks credentials" of its own.
  if (!nzchar(Sys.getenv("DATABRICKS_HOST"))) {
    rlang::abort("`DATABRICKS_HOST` is not set (check your .Renviron).")
  }
  DBI::dbConnect(
    odbc::databricks(),
    driver   = "Databricks ODBC Driver",
    httpPath = paste0("/sql/1.0/warehouses/", warehouse_id)
  )
}

#' Connect to Databricks with sparklyr
#'
#' Opens a `sparklyr` Spark connection, either native (inside a Databricks
#' notebook) or via Databricks Connect / Spark Connect from an IDE. Computation
#' stays in Spark until you [bricks_collect()].
#'
#' @details
#' The Databricks Connect path (`method = "databricks_connect"`) needs a
#' one-time backend install ([pysparklyr::install_databricks()]) and a token
#' with cluster/compute scope; a SQL-warehouse-scoped token works for
#' [databricks_connect()] but not here. Credentials are read from the
#' environment (`DATABRICKS_HOST`, `DATABRICKS_TOKEN`).
#'
#' @param method `"databricks_connect"` (remote compute from an IDE) or
#'   `"databricks"` (native, inside a Databricks notebook).
#' @param cluster_id Databricks cluster id. Defaults to the
#'   `DATABRICKS_CLUSTER_ID` environment variable.
#' @param serverless Use serverless compute. Defaults to `TRUE` when no
#'   `cluster_id` is available.
#' @param version Optional Databricks Runtime version for serverless. Defaults
#'   to the `DATABRICKS_DBR_VERSION` environment variable when set.
#' @return A `sparklyr` Spark connection.
#' @seealso [bricks_connect()], [databricks_connect()]
#' @export
#' @examples
#' \dontrun{
#' sc <- databricks_spark_connect(method = "databricks_connect", serverless = TRUE)
#' sparklyr::spark_disconnect(sc)
#' }
databricks_spark_connect <- function(method = c("databricks_connect", "databricks"),
                                     cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
                                     serverless = !nzchar(cluster_id),
                                     version = Sys.getenv("DATABRICKS_DBR_VERSION")) {
  method <- match.arg(method)
  need <- if (method == "databricks_connect") {
    c("sparklyr", "pysparklyr", "dbplyr")
  } else {
    c("sparklyr", "dbplyr")
  }
  rlang::check_installed(need, sprintf("for the %s backend.", method))

  # Native, in-notebook: attach to the ambient Databricks cluster; creds implicit.
  if (method == "databricks") {
    return(sparklyr::spark_connect(method = "databricks"))
  }

  # Databricks Connect from an IDE: needs host + a cluster/compute-scoped token.
  if (!nzchar(Sys.getenv("DATABRICKS_HOST")) || !nzchar(Sys.getenv("DATABRICKS_TOKEN"))) {
    rlang::abort("`DATABRICKS_HOST` and `DATABRICKS_TOKEN` must be set (check your .Renviron).")
  }

  args <- list(method = "databricks_connect")
  if (serverless) {
    args$serverless <- TRUE
    if (nzchar(version)) args$version <- version
  } else {
    if (!nzchar(cluster_id)) {
      rlang::abort(
        "Set `DATABRICKS_CLUSTER_ID` (or pass `cluster_id`), or use `serverless = TRUE`."
      )
    }
    args$cluster_id <- cluster_id
  }
  do.call(sparklyr::spark_connect, args)
}
