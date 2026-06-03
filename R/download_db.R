#' Download reference database from Zenodo
#'
#' Downloads and decompresses the DuckDB reference database for a specific bacterial genus from the Zenodo record (17791772).
#'
#' @param genus Character. The bacterial genus of interest (e.g., "Escherichia", "Salmonella"). Case-insensitive.
#' @param dest_dir Path to the directory where the uncompressed database will be saved. Defaults to the user data directory (`tools::R_user_dir("bactaxidR", which = "data")`).
#' @param force Logical. If `TRUE`, downloads the database even if it already exists locally.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @return Absolute path to the decompressed DuckDB database (`.db`).
#' @export
download_db <- function(genus, dest_dir = tools::R_user_dir("bactaxidR", which = "data"), force = FALSE, verbose = TRUE) {
  if (missing(genus) || is.null(genus) || nchar(trimws(genus)) == 0) {
    stop("Must specify a bacterial genus.")
  }

  genus_clean <- trimws(genus)
  available <- list_available_dbs(use_api = TRUE)

  # Case-insensitive search
  match_idx <- which(tolower(available) == tolower(genus_clean))
  if (length(match_idx) == 0) {
    stop(sprintf("The genus '%s' is not available in the Zenodo databases. Available genera:\n%s",
                 genus_clean, paste(available, collapse = ", ")))
  }

  genus_std <- available[match_idx]

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_db <- file.path(dest_dir, paste0(genus_std, ".db"))

  if (file.exists(dest_db) && !force) {
    if (verbose) message("The database for ", genus_std, " already exists at: ", dest_db)
    return(normalizePath(dest_db))
  }

  url <- sprintf("https://zenodo.org/api/records/17791772/files/%s.db.gz/content", genus_std)
  if (verbose) message("Downloading database for ", genus_std, " from Zenodo...")

  temp_gz <- tempfile(fileext = ".db.gz")
  on.exit({
    if (file.exists(temp_gz)) {
      unlink(temp_gz)
    }
  })

  status <- utils::download.file(url, destfile = temp_gz, mode = "wb", quiet = !verbose)
  if (status != 0) {
    stop("Error downloading the database file from Zenodo.")
  }

  if (verbose) message("Decompressing database...")

  # Native R base decompression using binary connections
  con_in <- gzfile(temp_gz, "rb")
  con_out <- file(dest_db, "wb")

  on.exit({
    try(close(con_in), silent = TRUE)
    try(close(con_out), silent = TRUE)
    if (file.exists(temp_gz)) unlink(temp_gz)
  }, add = TRUE)

  while (length(buf <- readBin(con_in, raw(), n = 10 * 1024 * 1024)) > 0) {
    writeBin(buf, con_out)
  }

  close(con_in)
  close(con_out)

  if (verbose) message("Database for ", genus_std, " downloaded and decompressed at: ", dest_db)
  return(normalizePath(dest_db))
}
