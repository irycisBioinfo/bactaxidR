#' Plot hierarchical Krona chart of taxonomic classifications
#'
#' Generates an interactive Krona chart or a static PNG snapshot representing the hierarchical
#' structure and frequency of each taxonomic level from a BacTaxID code table (`btx_code`) or classification results (`btx_cls`).
#'
#' @param df A data.frame of class `btx_code` (from `get_code_table()`) or `btx_cls` (from `classify()`).
#' @param root_label Character. Label for the center/root node of the Krona chart. Defaults to `"Total"`.
#' @param interactive Logical. If `TRUE` (default), returns an interactive htmltools tag object (iframe).
#'   If `FALSE`, generates a static PNG snapshot and returns its file path.
#' @param file Character. Optional file path to save the static PNG snapshot when `interactive = FALSE`. If `NULL`, a temporary file path is generated.
#' @param ... Additional arguments passed to `KronaR::kronar_plot` (if `interactive = TRUE`) or `KronaR::kronar_snapshot` (if `interactive = FALSE`).
#' @return If `interactive = TRUE`, an htmltools tag object (iframe) displaying the interactive Krona chart.
#'   If `interactive = FALSE`, the character file path where the static PNG snapshot was saved.
#' @export
#' @importFrom KronaR kronar_plot kronar_snapshot
plot_krona <- function(df, root_label = "Total", interactive = TRUE, file = NULL, ...) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Must specify a valid data.frame ('df').")
  }

  # Ensure KronaR is available
  if (!requireNamespace("KronaR", quietly = TRUE)) {
    stop("The 'KronaR' package is required for plotting. Please install it using remotes::install_github('irycisBioinfo/KronaR').")
  }

  # If input is classification results, convert it to a level-like structure
  if (inherits(df, "btx_cls")) {
    if (!"final_code" %in% names(df)) {
      stop("The classification table must contain a 'final_code' column.")
    }
    codes <- df$final_code
    codes <- codes[!is.na(codes) & codes != "" & codes != "0.0.0.0.0.0"]

    if (length(codes) == 0) {
      stop("No valid classification codes found to plot.")
    }

    # Split each code and create a levels data.frame
    split_codes <- strsplit(codes, "\\.")
    max_len <- max(sapply(split_codes, length))

    levels_df <- data.frame(matrix(NA, nrow = length(codes), ncol = max_len))
    for (r in seq_along(split_codes)) {
      parts <- split_codes[[r]]
      non_zero_indices <- which(parts != "0")
      if (length(non_zero_indices) > 0) {
        last_non_zero <- max(non_zero_indices)
        for (c in 1:last_non_zero) {
          levels_df[r, c] <- paste(parts[1:c], collapse = ".")
        }
      }
    }
    names(levels_df) <- paste0("L_", 0:(max_len - 1), "_full")
    df <- levels_df
  }

  # Identify level columns ending in _full
  cols <- grep("_full$", names(df), value = TRUE)
  if (length(cols) == 0) {
    stop("Input data.frame must contain columns ending in '_full' (e.g., 'L_0_full') or inherit from 'btx_cls' or 'btx_code'.")
  }

  # Sort level columns by their index
  cols <- cols[order(as.integer(sub("^L_([0-9]+)_full$", "\\1", cols)))]

  # Build the data frame for KronaR
  df_levels <- df[, cols, drop = FALSE]
  df_levels$Count <- 1

  # Call KronaR plotting function
  if (interactive) {
    p <- KronaR::kronar_plot(
      df = df_levels,
      count_col = "Count",
      root_name = root_label,
      ...
    )
    return(p)
  } else {
    # Generate static snapshot
    if (is.null(file)) {
      file <- tempfile(fileext = ".png")
    }

    snap_path <- tryCatch({
      KronaR::kronar_snapshot(
        df = df_levels,
        file = file,
        count_col = "Count",
        root_name = root_label,
        ...
      )
    }, error = function(e) {
      msg <- e$message
      if (grepl("chrome|chromium|google-chrome", tolower(msg))) {
        stop(paste0(
          "Chrome/Chromium executable not found. The 'webshot2' package requires a Chromium-based browser to take snapshots.\n",
          "Please install Google Chrome or Chromium on your system:\n",
          "  - Ubuntu/Debian: sudo apt-get update && sudo apt-get install -y chromium-browser\n",
          "  - macOS: brew install --cask google-chrome\n",
          "  - Windows: Install Google Chrome\n",
          "Original error: ", msg
        ), call. = FALSE)
      } else {
        stop(e)
      }
    })

    # Read and plot the PNG to the current graphics device
    if (requireNamespace("png", quietly = TRUE)) {
      img <- png::readPNG(snap_path)
      grid::grid.newpage()
      grid::grid.raster(img)
    } else {
      warning("The 'png' package is not installed. Unable to render the snapshot to the Plots pane.")
    }

    return(invisible(snap_path))
  }
}
