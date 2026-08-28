test_that(".bricks_table_ident() returns a bare name unchanged", {
  expect_identical(.bricks_table_ident("diamonds"), "diamonds")
})

test_that(".bricks_table_ident() parses two- and three-part names", {
  skip_if_not_installed("dbplyr")
  expect_no_error(.bricks_table_ident("default.diamonds"))
  expect_no_error(.bricks_table_ident("main.default.diamonds"))
})

test_that(".bricks_table_ident() rejects an over-qualified name", {
  expect_error(.bricks_table_ident("a.b.c.d"))
})
