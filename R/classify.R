#' Classify genomes using BacTaxID
#'
#' Performs taxonomic classification on a set of input genome files (in FASTA format)
#' against a genus-specific DuckDB reference database.
#'
#' @param fasta_files Character vector of paths to input FASTA files to classify.
#' @param genus Character (optional). Bacterial genus of the database to use. If specified,
#'   the database `<genus>.db` will be searched under `db_dir` or downloaded if missing.
#' @param db_path Character (optional). Direct path to a DuckDB database file (`.db`). If specified,
#'   takes precedence over `genus`.
#' @param db_dir Character. Directory to search for or download reference databases.
#' @param bin_path Character (optional). Direct path to the BacTaxID executable. If NULL, searches under `bin_dir`.
#' @param bin_dir Character. Directory to search for or download the BacTaxID executable.
#' @param verbose Logical. If `TRUE`, prints progress and stdout/stderr of the BacTaxID executable.
#' @return A `data.frame` of class `c("btx_cls", "data.frame")` containing classification results with columns
#'   such as `query_id`, `query_signature`, `best_hit_id`, `best_hit_signature`, `similarity_score`,
#'   `levels_reached`, `final_code`, and `best_hit_code`.
#' @export
classify <- function(fasta_files,
                     genus = NULL,
                     db_path = NULL,
                     db_dir = tools::R_user_dir("bactaxidR", which = "data"),
                     bin_path = NULL,
                     bin_dir = tools::R_user_dir("bactaxidR", which = "data"),
                     verbose = TRUE) {

  if (missing(fasta_files) || length(fasta_files) == 0) {
    stop("Must specify at least one input FASTA file.")
  }

  # Validate and resolve absolute paths for FASTA files
  fasta_paths <- sapply(fasta_files, function(f) {
    if (!file.exists(f)) {
      stop("FASTA file does not exist: ", f)
    }
    normalizePath(f)
  })

  # Prompt for genus interactively if both genus and db_path are NULL
  if (is.null(genus) && is.null(db_path)) {
    if (interactive()) {
      genus_input <- readline("Please enter the bacterial genus for BacTaxID (e.g., Escherichia): ")
      genus_input <- trimws(genus_input)
      if (nchar(genus_input) == 0) {
        stop("No genus was provided.")
      }
      genus <- genus_input
    } else {
      stop("Must specify 'genus' or 'db_path' when running in non-interactive mode.")
    }
  }

  # Resolve executable
  if (is.null(bin_path)) {
    # 1. Try using the bundled executable in the package
    bin_builtin <- system.file("bin/bactaxid", package = "bactaxidR")
    if (bin_builtin != "" && file.exists(bin_builtin)) {
      bin_path <- normalizePath(bin_builtin)
    } else {
      # 2. Fall back to bin_dir or downloading
      bin_file_expected <- file.path(bin_dir, "bactaxid")
      if (!file.exists(bin_file_expected)) {
        if (verbose) message("The BacTaxID executable was not found locally or bundled. Downloading...")
        bin_path <- download_bactaxid(dest_dir = bin_dir)
      } else {
        bin_path <- normalizePath(bin_file_expected)
      }
    }
  } else {
    if (!file.exists(bin_path)) {
      stop("The specified executable in 'bin_path' does not exist: ", bin_path)
    }
    bin_path <- normalizePath(bin_path)
  }

  # Resolve database
  if (is.null(db_path)) {
    # Search or download based on genus
    available <- list_available_dbs(use_api = TRUE)
    match_idx <- which(tolower(available) == tolower(trimws(genus)))
    if (length(match_idx) == 0) {
      stop(sprintf("The genus '%s' is not available in Zenodo.", genus))
    }
    genus_std <- available[match_idx]

    db_expected <- file.path(db_dir, paste0(genus_std, ".db"))
    if (!file.exists(db_expected)) {
      if (verbose) message("The database for ", genus_std, " was not found locally. Downloading...")
      db_path <- download_db(genus = genus_std, dest_dir = db_dir, verbose = verbose)
    } else {
      db_path <- normalizePath(db_expected)
    }
  } else {
    if (!file.exists(db_path)) {
      stop("The specified database in 'db_path' does not exist: ", db_path)
    }
    db_path <- normalizePath(db_path)
  }

  # Setup temporary files for query lists and outputs
  queries_file <- tempfile(fileext = "_queries.txt")
  output_file <- tempfile(fileext = "_output.tsv")

  on.exit({
    if (file.exists(queries_file)) unlink(queries_file)
    if (file.exists(output_file)) unlink(output_file)
  })

  # Write query absolute paths to the queries file
  writeLines(fasta_paths, con = queries_file)

  # Build command arguments
  args <- c(
    "classify",
    "--db", shQuote(db_path),
    "--queries", shQuote(queries_file),
    "--output", shQuote(output_file)
  )

  if (verbose) {
    args <- c(args, "--verbose")
  }

  if (verbose) {
    message("Running classification with BacTaxID...")
  }

  # Redirect stdout/stderr if not verbose
  stdout_opt <- if (verbose) "" else FALSE
  stderr_opt <- if (verbose) "" else FALSE

  exit_code <- system2(bin_path, args = args, stdout = stdout_opt, stderr = stderr_opt)

  if (exit_code != 0) {
    stop("BacTaxID execution failed with exit code ", exit_code)
  }

  if (!file.exists(output_file)) {
    stop("Execution completed but output file was not generated.")
  }

  # Read TSV results into a data.frame
  results <- utils::read.delim(
    output_file,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Assign custom S3 class btx_cls alongside data.frame
  class(results) <- c("btx_cls", "data.frame")

  return(results)
}
