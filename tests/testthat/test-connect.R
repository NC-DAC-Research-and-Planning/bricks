test_that("the csv backend builds a bricks_con and stores the path", {
  con <- bricks_connect("csv", path = "extract.csv")
  expect_s3_class(con, "bricks_con")
  expect_identical(con$backend, "csv")
  expect_identical(con$con, "extract.csv")
})

test_that("the csv backend requires a path", {
  expect_error(bricks_connect("csv"), class = "bricks_error_missing_path")
  expect_error(bricks_connect("csv", path = ""), class = "bricks_error_missing_path")
})

test_that("an unknown backend errors", {
  expect_error(bricks_connect("mysql"))
})

test_that("bricks_disconnect() is a safe no-op on non-connections", {
  expect_null(bricks_disconnect(NULL))
  expect_null(bricks_disconnect(42))
  expect_null(bricks_disconnect(bricks_connect("csv", path = "x.csv")))
})

test_that("print.bricks_con reports the backend", {
  con <- bricks_connect("csv", path = "x.csv")
  expect_output(print(con), "bricks_con: csv")
  expect_identical(print(con), con)
})
