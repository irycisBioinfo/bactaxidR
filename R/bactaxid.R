#' Listar bases de datos de géneros disponibles en Zenodo
#'
#' Recupera la lista de géneros bacterianos que tienen archivos de base de datos de referencia
#' disponibles en el registro de Zenodo de BacTaxID (17791772).
#'
#' @param use_api Lógico. Si es TRUE, consulta dinámicamente la API de Zenodo. Si es FALSE o si falla
#'   la conexión a la API, utiliza una lista local estática de los 67 géneros disponibles.
#' @return Un vector de caracteres con los nombres de los géneros.
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
      # Filtrar solo palabras válidas que representen géneros
      genera <- genera[grep("^[A-Za-z]+$", genera)]
      if (length(genera) > 0) {
        return(sort(unique(genera)))
      }
    }
    NULL
  }, error = function(e) {
    warning("No se pudo conectar a la API de Zenodo. Usando lista de géneros estática de respaldo.")
    NULL
  })

  if (is.null(res)) {
    return(sort(fallback_list))
  }
  return(res)
}

#' Descargar el ejecutable BacTaxID
#'
#' Descarga el ejecutable precompilado de BacTaxID (versión 1.0 para Linux) desde el repositorio oficial de GitHub.
#'
#' @param dest_dir Ruta al directorio donde se guardará el ejecutable. Por defecto usa el directorio de datos del usuario (`tools::R_user_dir("bactaxidR", which = "data")`).
#' @param force Lógico. Si es TRUE, descarga el ejecutable incluso si ya existe localmente.
#' @return Ruta absoluta al ejecutable descargado.
#' @export
download_bactaxid <- function(dest_dir = tools::R_user_dir("bactaxidR", which = "data"), force = FALSE) {
  if (.Platform$OS.type != "unix") {
    warning("El ejecutable precompilado de BacTaxID provisto solo está compilado para sistemas Linux. En Windows/macOS, podría requerir compilarlo manualmente desde las fuentes de GitHub.")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_file <- file.path(dest_dir, "bactaxid")

  if (file.exists(dest_file) && !force) {
    return(normalizePath(dest_file))
  }

  url <- "https://github.com/irycisBioinfo/BacTaxID/releases/download/v1.0/bactaxid"
  message("Descargando ejecutable BacTaxID desde: ", url, "...")

  status <- utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE)
  if (status != 0) {
    stop("Error al descargar el ejecutable de BacTaxID.")
  }

  # Configurar permisos de ejecución
  Sys.chmod(dest_file, "0755")
  message("Ejecutable descargado y configurado correctamente.")

  return(normalizePath(dest_file))
}

#' Descargar base de datos de referencia desde Zenodo
#'
#' Descarga y descomprime la base de datos DuckDB de un género bacteriano específico desde el registro de Zenodo (17791772).
#'
#' @param genus Carácter. El género bacteriano de interés (ej. "Escherichia", "Salmonella"). No es sensible a mayúsculas/minúsculas.
#' @param dest_dir Ruta al directorio donde se guardará la base de datos descomprimida. Por defecto usa el directorio de datos del usuario (`tools::R_user_dir("bactaxidR", which = "data")`).
#' @param force Lógico. Si es TRUE, descarga la base de datos incluso si ya existe localmente.
#' @param verbose Lógico. Si es TRUE, muestra mensajes de progreso.
#' @return Ruta absoluta a la base de datos DuckDB descomprimida (`.db`).
#' @export
download_db <- function(genus, dest_dir = tools::R_user_dir("bactaxidR", which = "data"), force = FALSE, verbose = TRUE) {
  if (missing(genus) || is.null(genus) || nchar(trimws(genus)) == 0) {
    stop("Debe especificar un género bacteriano.")
  }

  genus_clean <- trimws(genus)
  available <- list_available_dbs(use_api = TRUE)

  # Búsqueda insensible a mayúsculas/minúsculas
  match_idx <- which(tolower(available) == tolower(genus_clean))
  if (length(match_idx) == 0) {
    stop(sprintf("El género '%s' no está disponible en las bases de datos de Zenodo. Géneros disponibles:\n%s",
                 genus_clean, paste(available, collapse = ", ")))
  }

  genus_std <- available[match_idx]

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  dest_db <- file.path(dest_dir, paste0(genus_std, ".db"))

  if (file.exists(dest_db) && !force) {
    if (verbose) message("La base de datos para ", genus_std, " ya existe en: ", dest_db)
    return(normalizePath(dest_db))
  }

  url <- sprintf("https://zenodo.org/api/records/17791772/files/%s.db.gz/content", genus_std)
  if (verbose) message("Descargando base de datos para ", genus_std, " desde Zenodo...")

  temp_gz <- tempfile(fileext = ".db.gz")
  on.exit({
    if (file.exists(temp_gz)) {
      unlink(temp_gz)
    }
  })

  status <- utils::download.file(url, destfile = temp_gz, mode = "wb", quiet = !verbose)
  if (status != 0) {
    stop("Error al descargar el archivo de base de datos desde Zenodo.")
  }

  if (verbose) message("Descomprimiendo base de datos...")

  # Descompresión nativa de R base leyendo el gz binario y escribiendo el destino
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

  if (verbose) message("Base de datos de ", genus_std, " descargada y descomprimida en: ", dest_db)
  return(normalizePath(dest_db))
}

#' Clasificar genomas usando BacTaxID
#'
#' Realiza la clasificación taxonómica de un conjunto de genomas (en formato FASTA o FASTA comprimido)
#' contra una base de datos de referencia DuckDB específica para un género.
#'
#' @param fasta_files Vector de caracteres con las rutas a los archivos FASTA de los genomas a clasificar.
#' @param genus Carácter (opcional). Género bacteriano de la base de datos a usar. Si se proporciona,
#'   se buscará automáticamente `<genus>.db` en `db_dir`. Si no se encuentra, se descargará automáticamente.
#' @param db_path Carácter (opcional). Ruta directa al archivo de base de datos DuckDB (`.db`). Si se especifica, tiene prioridad sobre `genus`.
#' @param db_dir Carácter. Directorio donde buscar o descargar las bases de datos de referencia.
#' @param bin_path Carácter (opcional). Ruta directa al ejecutable de BacTaxID. Si es NULL, se buscará en `bin_dir`.
#' @param bin_dir Carácter. Directorio donde buscar o descargar el ejecutable de BacTaxID.
#' @param verbose Lógico. Si es TRUE, muestra mensajes informativos del proceso y del ejecutable.
#' @return Un data.frame con los resultados de la clasificación conteniendo columnas como `query_id`, `signature`, `best_hit`, `similarity`, `levels_reached` y códigos taxonómicos.
#' @export
classify <- function(fasta_files,
                     genus = NULL,
                     db_path = NULL,
                     db_dir = tools::R_user_dir("bactaxidR", which = "data"),
                     bin_path = NULL,
                     bin_dir = tools::R_user_dir("bactaxidR", which = "data"),
                     verbose = TRUE) {
  
  if (missing(fasta_files) || length(fasta_files) == 0) {
    stop("Debe especificar al menos un archivo FASTA de entrada.")
  }

  # Validar que los archivos FASTA existen y resolver rutas
  fasta_paths <- sapply(fasta_files, function(f) {
    if (!file.exists(f)) {
      stop("El archivo FASTA no existe: ", f)
    }
    normalizePath(f)
  })

  # Solicitar género interactivamente si no se suministra género ni db_path
  if (is.null(genus) && is.null(db_path)) {
    if (interactive()) {
      genus_input <- readline("Por favor, introduzca el género bacteriano para usar con BacTaxID (ej. Escherichia): ")
      genus_input <- trimws(genus_input)
      if (nchar(genus_input) == 0) {
        stop("No se proporcionó ningún género.")
      }
      genus <- genus_input
    } else {
      stop("Debe especificar 'genus' o 'db_path' cuando se ejecuta en modo no interactivo.")
    }
  }

  # Configurar ejecutable
  if (is.null(bin_path)) {
    # 1. Intentar usar el binario integrado en el paquete
    bin_builtin <- system.file("bin/bactaxid", package = "bactaxidR")
    if (bin_builtin != "" && file.exists(bin_builtin)) {
      bin_path <- normalizePath(bin_builtin)
    } else {
      # 2. Si no está integrado, buscar en bin_dir o descargarlo
      bin_file_expected <- file.path(bin_dir, "bactaxid")
      if (!file.exists(bin_file_expected)) {
        if (verbose) message("El ejecutable de BacTaxID no se encontró localmente ni integrado. Descargándolo...")
        bin_path <- download_bactaxid(dest_dir = bin_dir)
      } else {
        bin_path <- normalizePath(bin_file_expected)
      }
    }
  } else {
    if (!file.exists(bin_path)) {
      stop("El ejecutable especificado en 'bin_path' no existe: ", bin_path)
    }
    bin_path <- normalizePath(bin_path)
  }

  # Configurar base de datos
  if (is.null(db_path)) {
    # Búsqueda o descarga basada en genus
    available <- list_available_dbs(use_api = TRUE)
    match_idx <- which(tolower(available) == tolower(trimws(genus)))
    if (length(match_idx) == 0) {
      stop(sprintf("El género '%s' no está disponible en Zenodo.", genus))
    }
    genus_std <- available[match_idx]
    
    db_expected <- file.path(db_dir, paste0(genus_std, ".db"))
    if (!file.exists(db_expected)) {
      if (verbose) message("La base de datos para ", genus_std, " no se encontró localmente. Descargándola...")
      db_path <- download_db(genus = genus_std, dest_dir = db_dir, verbose = verbose)
    } else {
      db_path <- normalizePath(db_expected)
    }
  } else {
    if (!file.exists(db_path)) {
      stop("La base de datos especificada en 'db_path' no existe: ", db_path)
    }
    db_path <- normalizePath(db_path)
  }

  # Crear archivos temporales para queries y salida
  queries_file <- tempfile(fileext = "_queries.txt")
  output_file <- tempfile(fileext = "_output.tsv")
  
  on.exit({
    if (file.exists(queries_file)) unlink(queries_file)
    if (file.exists(output_file)) unlink(output_file)
  })

  # Escribir las rutas absolutas de los fasta al archivo queries.txt
  writeLines(fasta_paths, con = queries_file)

  # Construir argumentos de comando
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
    message("Ejecutando clasificación con BacTaxID...")
  }

  # Redireccionar stdout/stderr si no verbose
  stdout_opt <- if (verbose) "" else FALSE
  stderr_opt <- if (verbose) "" else FALSE

  exit_code <- system2(bin_path, args = args, stdout = stdout_opt, stderr = stderr_opt)

  if (exit_code != 0) {
    stop("La ejecución de BacTaxID falló con código de salida ", exit_code)
  }

  if (!file.exists(output_file)) {
    stop("La ejecución finalizó pero no se generó el archivo de salida.")
  }

  # Cargar resultados en un data.frame
  results <- utils::read.delim(
    output_file,
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  return(results)
}

#' Importar la tabla 'code' de la base de datos DuckDB
#'
#' Conecta a la base de datos DuckDB especificada y recupera la tabla de clasificaciones jerárquicas `code` en un R `data.frame`.
#' Admite tanto el paquete `duckdb` de R como el ejecutable CLI `duckdb` de forma automática.
#'
#' @param db_path Carácter. Ruta a la base de datos DuckDB (`.db`).
#' @param full_table Lógico. Si es `TRUE`, importa la tabla completa (incluyendo `signature` y las columnas `_state`). Si es `FALSE` (por defecto), solo importa las columnas `sample` y las columnas de índices (`_int`) y códigos completos (`_full`) de cada nivel.
#' @return Un `data.frame` con los datos de la tabla `code`.
#' @export
get_code_table <- function(db_path, full_table = FALSE) {
  if (missing(db_path) || is.null(db_path) || nchar(trimws(db_path)) == 0) {
    stop("Debe especificar la ruta a la base de datos DuckDB ('db_path').")
  }

  if (!file.exists(db_path)) {
    stop("La base de datos especificada no existe: ", db_path)
  }

  db_path <- normalizePath(db_path)

  # 1. Intentar usar el paquete duckdb de R
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
    return(res)
  }
  # 2. Si no está disponible, intentar usar el ejecutable CLI de DuckDB
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
      return(res)
    } else {
      stop("No se pudieron obtener las columnas de la tabla 'code' usando el CLI de DuckDB.")
    }
  }
  # 3. Lanzar error si no hay ninguna opción disponible
  else {
    stop("Para importar la tabla de clasificación, por favor instale el paquete 'duckdb' en R (install.packages('duckdb')) o asegúrese de que el ejecutable 'duckdb' está en el PATH de su sistema.")
  }
}

