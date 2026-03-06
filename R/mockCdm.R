#' Locate the bundled mock ARTEMIS CDM DuckDB database
#'
#' @param absolute Optional absolute path to a DuckDB file. If provided, the
#' function validates and returns that path.
#'
#' @return An absolute path to the DuckDB database file.
#' @export
loadMockArtemisCdm <- function(absolute = NULL) {
  if (!is.null(absolute)) {
    if (!file.exists(absolute)) {
      stop("Error: The specified file was not found: ", absolute)
    }
    return(normalizePath(absolute, winslash = "/", mustWork = TRUE))
  }

  path <- system.file("extdata", "mockArtemisCdm.duckdb", package = "ARTEMIS")
  if (!nzchar(path) || !file.exists(path)) {
    local_path <- file.path("inst", "extdata", "mockArtemisCdm.duckdb")
    if (file.exists(local_path)) {
      return(normalizePath(local_path, winslash = "/", mustWork = TRUE))
    }
    stop("Error: Could not find bundled mockArtemisCdm.duckdb in ARTEMIS.")
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Create DatabaseConnector details for the mock ARTEMIS CDM
#'
#' @param absolute Optional absolute path to a DuckDB file. If provided, the
#' function validates and uses that path.
#'
#' @return A `DatabaseConnector` connection details object configured for DuckDB.
#' @export
createMockArtemisConnectionDetails <- function(absolute = NULL) {
  dbPath <- loadMockArtemisCdm(absolute = absolute)
  DatabaseConnector::createConnectionDetails(dbms = "duckdb", server = dbPath)
}
