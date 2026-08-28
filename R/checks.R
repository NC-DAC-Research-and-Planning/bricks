# Preflight checks shared across the connection builders.
#
# These centralize the environment-variable and platform/install checks that
# bricks_connect() would otherwise hit only when a delegated connector fails
# with a cryptic message. Each raises a classed condition (prefixed
# `bricks_error_`) so callers and tests can dispatch on the specific failure.

# ---- internal predicates ---------------------------------------------------

# Is an environment variable set to a non-empty value?
.have_env <- function(var) nzchar(Sys.getenv(var))

# Are we on Windows, where local Spark needs the MSVC++ 2010 redistributable?
.is_windows <- function() identical(.Platform$OS.type, "windows")

# How many Spark versions has sparklyr installed locally? Returns 0 when
# sparklyr is absent or reports none, so callers can treat 0 as "no local Spark".
.n_spark_versions <- function() {
  if (!rlang::is_installed("sparklyr")) {
    return(0L)
  }
  tryCatch(
    nrow(sparklyr::spark_installed_versions()),
    error = function(e) 0L
  )
}

# ---- env-var preflight (shared by the connection builders) -----------------

# Abort unless every `vars` is set. `backend` only shapes the message.
.check_env <- function(vars, backend) {
  missing <- vars[!vapply(vars, .have_env, logical(1))]
  if (length(missing) == 0) {
    return(invisible())
  }
  rlang::abort(
    c(
      sprintf("Missing configuration for the %s backend.", backend),
      x = sprintf("`%s` is not set.", missing),
      i = "Set it in your `.Renviron` (see `vignette(\"windows-setup\", \"bricks\")`)."
    ),
    class = "bricks_error_missing_config"
  )
}

# Abort if local Spark cannot run here (Windows without the MSVC++ runtime, or
# no installed Spark version). Raised before sparklyr is invoked so the user
# sees the fix instead of a downstream "external pointer is not valid".
.check_local_spark <- function() {
  if (.n_spark_versions() > 0) {
    return(invisible())
  }
  msg <- c(
    "No local Spark installation was found for the `databricks` backend.",
    i = "Install a Spark version with `sparklyr::spark_install()`."
  )
  if (.is_windows()) {
    msg <- c(
      msg,
      i = paste(
        "On Windows, local Spark also requires the Microsoft Visual C++ 2010",
        "SP1 x64 Redistributable (vcredist_x64.exe), then restart R:"
      ),
      " " = "https://www.microsoft.com/en-us/download/details.aspx?id=26999"
    )
  }
  rlang::abort(msg, class = "bricks_error_spark_unavailable")
}

# Re-raise a sparklyr/Spark Connect serverless-incompatibility error as a
# classed bricks condition with actionable guidance, passing anything else
# through unchanged. Wrap the spark_connect() call in this.
#
# FRAGILE: sparklyr/pysparklyr surfaces this as a generic error, so we detect it
# by matching the message text below. If sparklyr rewords the message, the match
# silently stops firing -- the original error still propagates unchanged (we only
# add a hint, never swallow), so the failure mode is a missing hint, not a broken
# connection. Revisit these patterns if the serverless hint stops appearing.
.with_serverless_hint <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      compat <- grepl(
        "Serverless mode is not yet supported|version compatibility",
        msg,
        ignore.case = TRUE
      )
      if (!compat) {
        return(invisible())
      }
      rlang::abort(
        c(
          "Serverless Databricks Connect is not supported by this setup.",
          x = "The installed Databricks Connect version does not support serverless.",
          i = paste(
            "Target a cluster with `DATABRICKS_CLUSTER_ID` (or `cluster_id`),",
            "or upgrade Databricks Connect. For plain warehouse queries, use the",
            "`odbc` backend instead."
          ),
          i = "https://docs.databricks.com/aws/en/dev-tools/databricks-connect/requirements"
        ),
        class = "bricks_error_dbconnect_serverless",
        parent = cnd
      )
    }
  )
}

# ---- user-facing diagnostic ------------------------------------------------

#' Diagnose a bricks backend's configuration
#'
#' Reports, without opening a connection, whether the pieces a backend needs
#' are in place: the relevant environment variables, the odbc driver, and (for
#' the Spark backends) a local Spark install and the Windows runtime caveat.
#' Use it to debug a failing [bricks_connect()] before chasing a connector's
#' own error message.
#'
#' @param backend Backend to check. One of `"odbc"`, `"databricks"`, or
#'   `"databricks_connect"`. Defaults to [bricks_backend()].
#' @return A tibble with one row per check: the `check` name, a logical `ok`,
#'   and a human-readable `detail`. Returned invisibly; also printed as a
#'   readable report when called interactively.
#' @seealso [bricks_connect()], [bricks_backend()]
#' @export
#' @examples
#' \dontrun{
#' bricks_check("odbc")
#' }
bricks_check <- function(backend = bricks_backend()) {
  backend <- match.arg(backend, setdiff(.bricks_backends, "csv"))

  env_check <- function(var) {
    tibble::tibble(
      check = var,
      ok = .have_env(var),
      detail = if (.have_env(var)) "set" else "not set"
    )
  }

  rows <- switch(backend,
    odbc = dplyr::bind_rows(
      env_check("DATABRICKS_HOST"),
      env_check("DATABRICKS_WAREHOUSE_ID"),
      tibble::tibble(
        check = "DATABRICKS_TOKEN or CLI profile",
        ok = .have_env("DATABRICKS_TOKEN") || .have_env("DATABRICKS_CONFIG_PROFILE"),
        detail = if (.have_env("DATABRICKS_TOKEN")) {
          "PAT set"
        } else if (.have_env("DATABRICKS_CONFIG_PROFILE")) {
          "CLI profile set"
        } else {
          "no PAT or CLI profile found"
        }
      ),
      tibble::tibble(
        check = "odbc driver",
        ok = rlang::is_installed("odbc"),
        detail = if (rlang::is_installed("odbc")) "odbc installed" else "install the odbc package"
      )
    ),
    databricks = dplyr::bind_rows(
      tibble::tibble(
        check = "local Spark",
        ok = .n_spark_versions() > 0,
        detail = sprintf("%d Spark version(s) installed", .n_spark_versions())
      ),
      tibble::tibble(
        check = "Windows MSVC++ runtime",
        ok = !.is_windows() || .n_spark_versions() > 0,
        detail = if (.is_windows()) {
          "Windows: needs MSVC++ 2010 SP1 x64 for local Spark"
        } else {
          "not applicable on this platform"
        }
      )
    ),
    databricks_connect = dplyr::bind_rows(
      env_check("DATABRICKS_HOST"),
      env_check("DATABRICKS_TOKEN"),
      tibble::tibble(
        check = "pysparklyr",
        ok = rlang::is_installed("pysparklyr"),
        detail = if (rlang::is_installed("pysparklyr")) {
          "installed"
        } else {
          "install pysparklyr and run pysparklyr::install_databricks()"
        }
      )
    )
  )

  rows <- tibble::add_column(rows, backend = backend, .before = 1L)

  if (rlang::is_interactive()) {
    ok <- all(rows$ok)
    cli_line <- sprintf(
      "bricks_check(%s): %s\n",
      encodeString(backend, quote = '"'),
      if (ok) "all checks passed" else "some checks failed"
    )
    cat(cli_line)
    for (i in seq_len(nrow(rows))) {
      cat(sprintf(
        "  %s %s: %s\n",
        if (rows$ok[i]) "[ok]" else "[--]",
        rows$check[i],
        rows$detail[i]
      ))
    }
  }

  invisible(rows)
}
