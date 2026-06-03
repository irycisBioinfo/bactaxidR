#' List available genus databases on Zenodo
#'
#' Retrieves the list of bacterial genera that have reference database files
#' available in the BacTaxID Zenodo record (17791772).
#'
#' @param use_api Logical. If `TRUE`, queries the Zenodo API dynamically. If `FALSE`
#'   or if the connection fails, uses a static local list of the 67 available genera.
#' @return A character vector of genus names.
#' @export
#' @importFrom jsonlite fromJSON
list_available_dbs <- function(use_api = TRUE) {
  fallback_list <- c(
    "Achromobacter", "Acinetobacter", "Actinobacillus", "Aeromonas", "Agrobacterium",
    "Alistipes", "Bacillus", "Bacteroides", "Bifidobacterium", "Bordetella",
    "Borreliella", "Brucella", "Burkholderia", "Campylobacter", "Chlamydia",
    "Citrobacter", "Clostridioides", "Clostridium", "Corynebacterium", "Cronobacter",
    "Cutibacterium", "Elizabethkingia", "Enterobacter", "Enterococcus", "Erysipelothrix",
    "Escherichia", "Flavobacterium", "Francisella", "Fusobacterium", "Glaesserella",
    "Haemophilus", "Helicobacter", "Klebsiella", "Lactiplantibacillus", "Lacticaseibacillus",
    "Lactobacillus", "Legionella", "Leptospira", "Limosilactobacillus", "Listeria",
    "Mammaliicoccus", "Mannheimia", "Mesorhizobium", "Moraxella", "Mycobacterium",
    "Mycoplasmopsis", "Neisseria", "Paenibacillus", "Parabacteroides", "Pasteurella",
    "Phocaeicola", "Proteus", "Providencia", "Pseudomonas", "Ralstonia",
    "Salmonella", "Sarcina", "Serratia", "Sinorhizobium", "Staphylococcus",
    "Stenotrophomonas", "Streptococcus", "Streptomyces", "Treponema", "Vibrio",
    "Xanthomonas", "Yersinia"
  )

  if (!use_api) {
    return(sort(fallback_list))
  }

  api_url <- "https://zenodo.org/api/records/17791772"
  res <- tryCatch({
    data <- jsonlite::fromJSON(api_url, simplifyVector = TRUE)
    files <- data$files
    if (!is.null(files) && "key" %in% names(files)) {
      keys <- files$key
      genera <- sub("\\.db\\.gz$", "", keys)
      # Filter for valid genus names
      genera <- genera[grep("^[A-Za-z]+$", genera)]
      if (length(genera) > 0) {
        return(sort(unique(genera)))
      }
    }
    NULL
  }, error = function(e) {
    warning("Failed to connect to the Zenodo API. Using static fallback genus list.")
    NULL
  })

  if (is.null(res)) {
    return(sort(fallback_list))
  }
  return(res)
}
