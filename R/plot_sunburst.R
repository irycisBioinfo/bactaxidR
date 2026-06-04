#' Plot hierarchical sunburst chart of taxonomic classifications using sunburstR
#'
#' Generates an interactive sunburst chart representing the hierarchical structure and
#' frequency of each taxonomic level from a BacTaxID code table (`btx_code`) or classification results (`btx_cls`).
#'
#' @param df A data.frame of class `btx_code` (from `get_code_table()`) or `btx_cls` (from `classify()`).
#' @param root_label Character. Label for the center/root node of the sunburst chart.
#'   If specified, prepended to all paths. Defaults to `"Total"`.
#' @param ... Additional arguments passed to `sunburstR::sunburst()`.
#' @return A `sunburst` `htmlwidget` object displaying the interactive sunburst chart.
#' @export
#' @importFrom sunburstR sunburst
plot_sunburst <- function(df, root_label = "Total", ...) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Must specify a valid data.frame ('df').")
  }

  # Ensure sunburstR is available
  if (!requireNamespace("sunburstR", quietly = TRUE)) {
    stop("The 'sunburstR' package is required for plotting. Please install it using install.packages('sunburstR').")
  }

  # 1. If input is classification results (btx_cls), extract final_code
  if (inherits(df, "btx_cls")) {
    if (!"final_code" %in% names(df)) {
      stop("The classification table must contain a 'final_code' column.")
    }
    codes <- df$final_code
    codes <- codes[!is.na(codes) & codes != "" & codes != "0.0.0.0.0.0"]

    if (length(codes) == 0) {
      stop("No valid classification codes found to plot.")
    }

    # Remove trailing zeros and build paths
    paths <- sapply(strsplit(codes, "\\."), function(parts) {
      non_zero_indices <- which(parts != "0")
      if (length(non_zero_indices) > 0) {
        last_non_zero <- max(non_zero_indices)
        parts_clean <- parts[1:last_non_zero]
        paste(parts_clean, collapse = "-")
      } else {
        NA
      }
    })
    paths <- paths[!is.na(paths)]
    
    if (length(paths) == 0) {
      stop("No valid hierarchical paths found to plot.")
    }

    # Aggregate counts
    tbl <- table(paths)
    df_plot <- data.frame(
      path = names(tbl),
      count = as.numeric(tbl),
      stringsAsFactors = FALSE
    )
  }
  # 2. If input is reference code table (btx_code) or a general table with _full columns
  else {
    # Identify level columns ending in _full
    cols <- grep("_full$", names(df), value = TRUE)
    if (length(cols) == 0) {
      stop("Input data.frame must contain columns ending in '_full' (e.g., 'L_0_full') or inherit from 'btx_cls' or 'btx_code'.")
    }

    # Sort level columns by their index
    cols <- cols[order(as.integer(sub("^L_([0-9]+)_full$", "\\1", cols)))]

    # For each row, extract the deepest non-empty level path
    paths <- apply(df[, cols, drop = FALSE], 1, function(row) {
      non_empty <- row[row != "" & !is.na(row)]
      if (length(non_empty) == 0) {
        return(NA)
      }
      non_empty[length(non_empty)]
    })
    paths <- paths[!is.na(paths)]
    
    if (length(paths) == 0) {
      stop("No valid hierarchical paths found to plot.")
    }

    # Replace dot separators with hyphens
    paths <- gsub("\\.", "-", paths)

    # Aggregate counts (each row is one sample)
    tbl <- table(paths)
    df_plot <- data.frame(
      path = names(tbl),
      count = as.numeric(tbl),
      stringsAsFactors = FALSE
    )
  }

  # Prepend root label if specified
  if (!is.null(root_label) && nchar(trimws(root_label)) > 0) {
    df_plot$path <- paste(root_label, df_plot$path, sep = "-")
  }

  # Generate the sunburst chart
  p <- sunburstR::sunburst(
    data = df_plot,
    ...
  )

  return(p)
}
