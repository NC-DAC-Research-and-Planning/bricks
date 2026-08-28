#' Collect a lazy result into a local tibble
#'
#' Pulls a lazy table (or a local frame) into a [tibble][tibble::tibble] and
#' casts Spark 64-bit integers ([bit64::integer64]) to `double`. Spark `BIGINT`
#' columns arrive as `integer64`, which base R modelling functions (and
#' `tidymodels`) do not accept; casting centrally means no downstream code has
#' to remember to.
#'
#' @param x A lazy `tbl` (or local data frame) from [bricks_tbl()] or
#'   [bricks_sql()].
#' @param ... Passed on to [dplyr::collect()].
#' @return A [tibble][tibble::tibble] with any `integer64` columns cast to
#'   `double`.
#' @seealso [bricks_tbl()], [bricks_sql()]
#' @export
#' @examples
#' bricks_collect(data.frame(x = 1:3))
bricks_collect <- function(x, ...) {
  dplyr::collect(x, ...) |>
    dplyr::mutate(dplyr::across(dplyr::where(~ inherits(.x, "integer64")), as.double)) |>
    tibble::as_tibble()
}
