test_that("bricks_collect() returns a tibble and preserves values", {
  out <- bricks_collect(data.frame(x = 1:3, y = c("a", "b", "c")))
  expect_s3_class(out, "tbl_df")
  expect_identical(out$x, 1:3)
  expect_identical(out$y, c("a", "b", "c"))
})

test_that("bricks_collect() casts integer64 columns to double", {
  skip_if_not_installed("bit64")
  df <- tibble::tibble(id = bit64::as.integer64(c(1, 2, 3)), keep = 1:3)
  out <- bricks_collect(df)
  expect_type(out$id, "double")
  expect_identical(out$id, c(1, 2, 3))
  expect_type(out$keep, "integer")
})
