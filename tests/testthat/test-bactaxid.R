test_that("list_available_dbs returns valid genera", {
  # Test with static list (offline fallback)
  genera_static <- list_available_dbs(use_api = FALSE)
  expect_type(genera_static, "character")
  expect_true(length(genera_static) > 0)
  expect_true("Escherichia" %in% genera_static)
  expect_true("Salmonella" %in% genera_static)
  expect_true("Pseudomonas" %in% genera_static)

  # Test with API list (might fail if no connection, but should fallback gracefully)
  genera_api <- list_available_dbs(use_api = TRUE)
  expect_type(genera_api, "character")
  expect_true(length(genera_api) > 0)
  expect_true("Escherichia" %in% genera_api)
})

test_that("download_db validates genus and throws error if invalid", {
  expect_error(download_db(""), "Debe especificar un género bacteriano")
  expect_error(download_db(NULL), "Debe especificar un género bacteriano")
  expect_error(download_db("InvalidGenusNameThatDoesNotExist123"), "no está disponible en las bases de datos de Zenodo")
})

test_that("classify validates input arguments", {
  # Error when fasta_files is missing
  expect_error(classify(), "Debe especificar al menos un archivo FASTA de entrada")
  expect_error(classify(character(0)), "Debe especificar al menos un archivo FASTA de entrada")

  # Error when files do not exist
  expect_error(classify("non_existent_file_xyz.fasta"), "El archivo FASTA no existe")

  # Error in non-interactive mode when genus and db_path are NULL
  # Create a dummy fasta file to pass the first check
  dummy_fasta <- tempfile(fileext = ".fasta")
  writeLines(">seq\nACGT", dummy_fasta)
  on.exit(unlink(dummy_fasta))

  expect_error(
    classify(dummy_fasta, genus = NULL, db_path = NULL),
    "Debe especificar 'genus' o 'db_path' cuando se ejecuta en modo no interactivo"
  )
})
