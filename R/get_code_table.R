#' Import 'code' table from DuckDB database
#'
#' Connects to the specified DuckDB database and retrieves the hierarchical classification table `code` as an R `data.frame`.
#' Supports both the R `duckdb` package and the `duckdb` CLI executable automatically.
#'
#' @param db_path Character. Path to the DuckDB database file (`.db`).
#' @param full_table Logical. If `TRUE`, imports the complete table (including `signature` and `_state` columns).
#'   If `FALSE` (default), only imports `sample` and the hierarchical index (`_int`) and full path (`_full`) columns.
#' @return A `data.frame` of class `c("btx_code", "data.frame")` containing the imported codes.
#' @export
get_code_table <- function(db_path, full_table = FALSE) {
  if (missing(db_path) || is.null(db_path) || nchar(trimws(db_path)) == 0) {
    stop("Must specify the path to the DuckDB database ('db_path').")
  }

  if (!file.exists(db_path)) {
    stop("The specified database does not exist: ", db_path)
  }

  db_path <- normalizePath(db_path)

  # 1. Try using the R duckdb package
  if (requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)) {
    con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

    all_cols <- DBI::dbListFields(con, "code")

    if (full_table) {
      cols_to_select <- all_cols
    } else {
      cols_to_select <- all_cols[all_cols == "sample" | grepl("_(int|full)$", all_cols)]
    }

    query <- sprintf("SELECT %s FROM code", paste(sprintf('"%s"', cols_to_select), collapse = ", "))
    res <- DBI::dbGetQuery(con, query)
    
    # Assign custom S3 class btx_code alongside data.frame
    class(res) <- c("btx_code", "data.frame")
    
    return(res)
  }
  # 2. If not available, try using the DuckDB CLI executable
  else if (Sys.which("duckdb") != "") {
    col_query <- "PRAGMA table_info('code')"
    col_info <- system2("duckdb", args = c(shQuote(db_path), "-csv", "-c", shQuote(col_query)), stdout = TRUE, stderr = FALSE)

    if (length(col_info) > 0) {
      df_cols <- read.csv(text = paste(col_info, collapse = "\n"), stringsAsFactors = FALSE)
      all_cols <- df_cols$name

      if (full_table) {
        cols_to_select <- all_cols
      } else {
        cols_to_select <- all_cols[all_cols == "sample" | grepl("_(int|full)$", all_cols)]
      }

      select_query <- sprintf("SELECT %s FROM code", paste(sprintf('"%s"', cols_to_select), collapse = ", "))
      csv_data <- system2("duckdb", args = c(shQuote(db_path), "-csv", "-c", shQuote(select_query)), stdout = TRUE, stderr = FALSE)

      res <- read.csv(text = paste(csv_data, collapse = "\n"), stringsAsFactors = FALSE, check.names = FALSE)
      
      # Assign custom S3 class btx_code alongside data.frame
      class(res) <- c("btx_code", "data.frame")
      
      return(res)
    } else {
      stop("Failed to retrieve columns of 'code' table using DuckDB CLI.")
    }
  }
  # 3. Throw an error if no options are available
  else {
    stop("To import the classification table, please install the 'duckdb' package in R (install.packages('duckdb')) or ensure that the 'duckdb' executable is in your system PATH.")
  }
}
