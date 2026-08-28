test_that(".check_env passes when all vars are set", {
  withr::local_envvar(DATABRICKS_HOST = "https://x", DATABRICKS_WAREHOUSE_ID = "w1")
  expect_invisible(.check_env(c("DATABRICKS_HOST", "DATABRICKS_WAREHOUSE_ID"), "odbc"))
})

test_that(".check_env aborts with a classed condition when a var is missing", {
  withr::local_envvar(DATABRICKS_HOST = "https://x", DATABRICKS_WAREHOUSE_ID = "")
  expect_error(
    .check_env(c("DATABRICKS_HOST", "DATABRICKS_WAREHOUSE_ID"), "odbc"),
    class = "bricks_error_missing_config"
  )
})

test_that(".check_local_spark passes when a Spark version is installed", {
  local_mocked_bindings(.n_spark_versions = function() 1L)
  expect_invisible(.check_local_spark())
})

test_that(".check_local_spark aborts when no Spark version is installed", {
  local_mocked_bindings(.n_spark_versions = function() 0L)
  expect_error(.check_local_spark(), class = "bricks_error_spark_unavailable")
})

test_that(".with_serverless_hint re-raises the compat error as a classed condition", {
  boom <- function() {
    rlang::abort("Serverless mode is not yet supported in this version of Databricks Connect.")
  }
  expect_error(
    .with_serverless_hint(boom()),
    class = "bricks_error_dbconnect_serverless"
  )
})

test_that(".with_serverless_hint passes other errors through unchanged", {
  expect_error(
    .with_serverless_hint(rlang::abort("some other failure", class = "other_error")),
    class = "other_error"
  )
})

test_that("bricks_check() reports odbc config as a tibble", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://x",
    DATABRICKS_WAREHOUSE_ID = "w1",
    DATABRICKS_TOKEN = "dapiX",
    DATABRICKS_CONFIG_PROFILE = ""
  )
  res <- bricks_check("odbc")
  expect_s3_class(res, "tbl_df")
  expect_identical(unique(res$backend), "odbc")
  expect_true(all(res$ok[res$check %in% c("DATABRICKS_HOST", "DATABRICKS_WAREHOUSE_ID")]))
})

test_that("bricks_check() flags missing odbc config", {
  withr::local_envvar(
    DATABRICKS_HOST = "",
    DATABRICKS_WAREHOUSE_ID = "",
    DATABRICKS_TOKEN = "",
    DATABRICKS_CONFIG_PROFILE = ""
  )
  res <- bricks_check("odbc")
  expect_false(all(res$ok))
})

test_that("bricks_check() rejects the csv backend", {
  expect_error(bricks_check("csv"))
})
