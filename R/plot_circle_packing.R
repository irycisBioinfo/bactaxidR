#' Plot hierarchical circle packing diagram of taxonomic classifications using ggraph
#'
#' Generates a static circle packing diagram representing the hierarchical structure and
#' frequency of each taxonomic level from a BacTaxID code table (`btx_code`) or classification results (`btx_cls`).
#'
#' @param df A data.frame of class `btx_code` (from `get_code_table()`) or `btx_cls` (from `classify()`).
#' @param root_label Character. Label for the center/root node of the diagram. Defaults to `"Total"`.
#' @param ... Additional arguments passed to `ggraph::ggraph()`.
#' @return A `ggplot` object displaying the circle packing diagram.
#' @export
#' @importFrom igraph graph_from_data_frame
#' @importFrom ggraph ggraph geom_node_circle
#' @importFrom ggplot2 theme aes theme_void
plot_circle_packing <- function(df, root_label = "Total", ...) {
  if (missing(df) || !is.data.frame(df)) {
    stop("Must specify a valid data.frame ('df').")
  }

  # Ensure ggraph, igraph, and ggplot2 are available
  if (!requireNamespace("ggraph", quietly = TRUE) ||
      !requireNamespace("igraph", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggraph', 'igraph', and 'ggplot2' packages are required for plotting. Please install them.")
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

    # Split each code and create paths starting from root_label
    paths_list <- lapply(strsplit(codes, "\\."), function(parts) {
      non_zero_indices <- which(parts != "0")
      if (length(non_zero_indices) > 0) {
        last_non_zero <- max(non_zero_indices)
        parts_clean <- parts[1:last_non_zero]
        path_vec <- c(root_label)
        for (i in seq_along(parts_clean)) {
          path_vec <- c(path_vec, paste(c(root_label, parts_clean[1:i]), collapse = "."))
        }
        path_vec
      } else {
        c(root_label)
      }
    })
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

    # For each row, build the path segments starting from root_label
    paths_list <- apply(df[, cols, drop = FALSE], 1, function(row) {
      non_empty <- row[row != "" & !is.na(row)]
      if (length(non_empty) == 0) {
        return(c(root_label))
      }
      path_vec <- c(root_label)
      for (i in seq_along(non_empty)) {
        path_vec <- c(path_vec, paste(c(root_label, non_empty[1:i]), collapse = "."))
      }
      path_vec
    })
  }

  # Build edge list (from parent to child)
  edges_list <- list()
  for (p in paths_list) {
    if (length(p) > 1) {
      for (i in 1:(length(p) - 1)) {
        edges_list[[length(edges_list) + 1]] <- data.frame(
          from = p[i],
          to = p[i+1],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(edges_list) == 0) {
    stop("No hierarchical classification paths found to plot.")
  }

  edges <- unique(do.call(rbind, edges_list))

  # Count frequency of each leaf path (the final node of each query/sample)
  leaves_raw <- sapply(paths_list, function(p) p[length(p)])
  tbl <- table(leaves_raw)

  # Build unique vertices list
  all_nodes <- unique(c(edges$from, edges$to))
  
  # Format label to only show the last category segment instead of the full path
  labels <- sapply(strsplit(all_nodes, "\\."), function(x) x[length(x)])

  vertices <- data.frame(
    name = all_nodes,
    label = labels,
    size = 0,
    stringsAsFactors = FALSE
  )

  # Assign sizes only to leaf nodes in the tree
  leaves_in_graph <- all_nodes[!all_nodes %in% edges$from]
  vertices$size[match(leaves_in_graph, vertices$name)] <- as.numeric(tbl[match(leaves_in_graph, names(tbl))])
  vertices$size[is.na(vertices$size)] <- 0

  # Create graph
  g <- igraph::graph_from_data_frame(edges, vertices = vertices)

  # Render layout with ggraph
  p <- ggraph::ggraph(g, layout = "circlepack", weight = size, ...) +
    ggraph::geom_node_circle(ggplot2::aes(fill = depth), color = "black", linewidth = 0.2) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "right")

  return(p)
}
