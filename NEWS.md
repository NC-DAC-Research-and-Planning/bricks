# bricks 0.0.0.9000

* `bricks_check()` diagnoses a backend's configuration (environment variables,
  odbc driver, local Spark install, and the Windows MSVC++ runtime caveat)
  without opening a connection.

* Connection builders now preflight their requirements and raise classed
  conditions with actionable guidance: `bricks_error_missing_config`,
  `bricks_error_spark_unavailable` (local Spark not installed, with the Windows
  redistributable hint), and `bricks_error_dbconnect_serverless` (serverless
  incompatibility).

* Initial version. A backend-agnostic Databricks data-access API:
  `bricks_connect()`, `bricks_disconnect()`, `bricks_tbl()`, `bricks_sql()`,
  `bricks_collect()`, and `bricks_backend()`, plus the lower-level
  `databricks_connect()` and `databricks_spark_connect()` builders.
