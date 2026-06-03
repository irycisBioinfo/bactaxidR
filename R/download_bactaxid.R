#' Download BacTaxID executable
#'
#' Downloads the precompiled BacTaxID binary (version 1.0 for Linux) from the official GitHub repository.
#'
#' @param dest_dir Path to the directory where the executable will be saved. Defaults to the user data directory (`tools::R_user_dir("bactaxidR", which = "data")`).
#' @param force Logical. If `TRUE`, downloads the executable even if it already exists locally.
#' @return Absolute path to the downloaded executable.
#' @export
download_bactaxid <- function(dest_dir = tools::R_user_dir("bactaxidR", which = "data"), force = FALSE) {
  if (.Platform$OS.type != "unix") {
    warning("The precompiled BacTaxID binary is only validated for Linux systems. On Windows/macOS, you may need to compile it manually from GitHub source.")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_file <- file.path(dest_dir, "bactaxid")

  if (file.exists(dest_file) && !force) {
    return(normalizePath(dest_file))
  }

  url <- "https://github.com/irycisBioinfo/BacTaxID/releases/download/v1.0/bactaxid"
  message("Downloading BacTaxID executable from: ", url, "...")

  status <- utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE)
  if (status != 0) {
    stop("Error downloading BacTaxID executable.")
  }

  # Set execution permissions
  Sys.chmod(dest_file, "0755")
  message("Executable successfully downloaded and configured.")

  return(normalizePath(dest_file))
}
