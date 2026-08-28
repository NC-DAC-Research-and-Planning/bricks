# The set of recognized backends. Order is not significant.
.bricks_backends <- c("odbc", "databricks", "databricks_connect", "csv")

#' Resolve which Databricks backend to use
#'
#' Determines the active backend from configuration, in precedence order: the
#' `bricks.backend` [option][base::options()], then the `BRICKS_BACKEND`
#' environment variable, then auto-detection. Auto-detection returns
#' `"databricks"` when running inside a Databricks runtime (detected via the
#' `DATABRICKS_RUNTIME_VERSION` environment variable) and `"odbc"` otherwise.
#'
#' @return A single string: one of `"odbc"`, `"databricks"`,
#'   `"databricks_connect"`, or `"csv"`.
#' @seealso [bricks_connect()]
#' @export
#' @examples
#' # Force a backend for the duration of a call:
#' old <- options(bricks.backend = "csv")
#' bricks_backend()
#' options(old)
bricks_backend <- function() {
  opt <- getOption("bricks.backend")
  if (!is.null(opt) && nzchar(opt)) {
    return(match.arg(opt, .bricks_backends))
  }
  env <- Sys.getenv("BRICKS_BACKEND")
  if (nzchar(env)) {
    return(match.arg(env, .bricks_backends))
  }
  if (nzchar(Sys.getenv("DATABRICKS_RUNTIME_VERSION"))) "databricks" else "odbc"
}
