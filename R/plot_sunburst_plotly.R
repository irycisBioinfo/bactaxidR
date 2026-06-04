#' Plot hierarchical sunburst chart of taxonomic classifications using Plotly
#'
#' Generates an interactive sunburst chart representing the hierarchical structure and
#' frequency of each taxonomic level from a BacTaxID code table (`btx_code`) or classification results (`btx_cls`).
#'
#' @param df A data.frame of class `btx_code` (from `get_code_table()`) or `btx_cls` (from `classify()`).
#' @param root_label Character. Label for the center/root node of the sunburst chart. Defaults to `"Total"`.
#' @return A plotly widget object displaying the interactive sunburst chart.
#' @export
#' @importFrom plotly plot_ly layout
plot_sunburst_plotly <- function(df, root_label = "Total") {
  if (missing(df) || !is.data.frame(df)) {
    stop("Must specify a valid data.frame ('df').")
  }

  # Ensure plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("The 'plotly' package is required for plotting. Please install it using install.packages('plotly').")
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

  # Build the tree node dictionary
  nodes <- list()

  for (i in seq_along(cols)) {
    level_col <- cols[i]
    tbl <- table(df[[level_col]], useNA = "no")

    for (name in names(tbl)) {
      if (name != "" && !is.na(name)) {
        parts <- strsplit(name, "\\.")[[1]]
        parent_name <- if (length(parts) > 1) paste(parts[-length(parts)], collapse = ".") else root_label

        # Aggregate values for unique node IDs
        if (name %in% names(nodes)) {
          nodes[[name]]$value <- nodes[[name]]$value + as.numeric(tbl[name])
        } else {
          nodes[[name]] <- list(id = name, parent = parent_name, value = as.numeric(tbl[name]))
        }
      }
    }
  }

  if (length(nodes) == 0) {
    stop("No hierarchical classification paths found to plot.")
  }

  # Aggregate virtual root node count from all children of root_label
  root_children_val <- sum(sapply(nodes[sapply(nodes, function(n) n$parent == root_label)], function(n) n$value))
  nodes[[root_label]] <- list(id = root_label, parent = "", value = root_children_val)

  # Extract vectors for plotly
  ids <- sapply(nodes, function(n) n$id)
  parents <- sapply(nodes, function(n) n$parent)
  values <- sapply(nodes, function(n) n$value)

  # Create the interactive sunburst plot using plotly (remainder mode)
  p <- plotly::plot_ly(
    labels = ids,
    parents = parents,
    values = values,
    type = "sunburst",
    branchvalues = "remainder"
  )

  # Configure layout
  p <- plotly::layout(
    p,
    title = list(text = "BacTaxID Hierarchical Classification Sunburst"),
    margin = list(l = 0, r = 0, b = 0, t = 40)
  )

  return(p)
}
