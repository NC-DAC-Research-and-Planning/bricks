test_that("the option takes precedence over the env var", {
  withr::local_options(bricks.backend = "csv")
  withr::local_envvar(BRICKS_BACKEND = "odbc")
  expect_identical(bricks_backend(), "csv")
})

test_that("the env var is used when no option is set", {
  withr::local_options(bricks.backend = NULL)
  withr::local_envvar(BRICKS_BACKEND = "databricks_connect")
  expect_identical(bricks_backend(), "databricks_connect")
})

test_that("a Databricks runtime auto-detects as 'databricks'", {
  withr::local_options(bricks.backend = NULL)
  withr::local_envvar(BRICKS_BACKEND = "", DATABRICKS_RUNTIME_VERSION = "15.4")
  expect_identical(bricks_backend(), "databricks")
})

test_that("off-cluster with no config defaults to 'odbc'", {
  withr::local_options(bricks.backend = NULL)
  withr::local_envvar(BRICKS_BACKEND = "", DATABRICKS_RUNTIME_VERSION = "")
  expect_identical(bricks_backend(), "odbc")
})

test_that("an unknown backend errors", {
  withr::local_options(bricks.backend = "postgres")
  expect_error(bricks_backend())
})
