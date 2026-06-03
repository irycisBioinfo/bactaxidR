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
  expect_error(download_db(""), "Must specify a bacterial genus")
  expect_error(download_db(NULL), "Must specify a bacterial genus")
  expect_error(download_db("InvalidGenusNameThatDoesNotExist123"), "is not available in the Zenodo databases")
})

test_that("classify validates input arguments", {
  # Error when fasta_files is missing
  expect_error(classify(), "Must specify at least one input FASTA file")
  expect_error(classify(character(0)), "Must specify at least one input FASTA file")

  # Error when files do not exist
  expect_error(classify("non_existent_file_xyz.fasta"), "FASTA file does not exist")

  # Error in non-interactive mode when genus and db_path are NULL
  # Create a dummy fasta file to pass the first check
  dummy_fasta <- tempfile(fileext = ".fasta")
  writeLines(">seq\nACGT", dummy_fasta)
  on.exit(unlink(dummy_fasta))

  expect_error(
    classify(dummy_fasta, genus = NULL, db_path = NULL),
    "Must specify 'genus' or 'db_path' when running in non-interactive mode"
  )
})

test_that("get_code_table validates input and reads data", {
  expect_error(get_code_table(), "Must specify the path to the DuckDB database")
  expect_error(get_code_table("non_existent_db.db"), "The specified database does not exist")

  # If Serratia.db exists locally, verify reading and S3 class btx_code
  serratia_db <- testthat::test_path("../../test_data/Serratia.db")
  if (file.exists(serratia_db)) {
    # Test reduced import (default)
    df_reduced <- get_code_table(serratia_db, full_table = FALSE)
    expect_s3_class(df_reduced, "btx_code")
    expect_s3_class(df_reduced, "data.frame")
    expect_true("sample" %in% names(df_reduced))
    expect_true("L_0_int" %in% names(df_reduced))
    expect_true("L_0_full" %in% names(df_reduced))
    expect_false("signature" %in% names(df_reduced))
    expect_false("L_0_state" %in% names(df_reduced))

    # Test full import
    df_full <- get_code_table(serratia_db, full_table = TRUE)
    expect_s3_class(df_full, "btx_code")
    expect_s3_class(df_full, "data.frame")
    expect_true("signature" %in% names(df_full))
    expect_true("L_0_state" %in% names(df_full))
  }
})

test_that("classify output returns data.frame with class btx_cls", {
  serratia_db <- testthat::test_path("../../test_data/Serratia.db")
  test_fasta <- list.files(testthat::test_path("../../test_data"), pattern = "\\.fna$", full.names = TRUE)[1]
  if (file.exists(serratia_db) && !is.na(test_fasta) && file.exists(test_fasta)) {
    res_cls <- classify(test_fasta, db_path = serratia_db, verbose = FALSE)
    expect_s3_class(res_cls, "btx_cls")
    expect_s3_class(res_cls, "data.frame")
  }
})

test_that("plot_sunburst validates input and creates plotly widget", {
  expect_error(plot_sunburst(NULL), "Must specify a valid data.frame")
  expect_error(plot_sunburst(data.frame(x = 1)), "Input data.frame must contain columns ending in")

  serratia_db <- testthat::test_path("../../test_data/Serratia.db")
  if (file.exists(serratia_db)) {
    # Test plotting from btx_code
    df_reduced <- get_code_table(serratia_db, full_table = FALSE)
    p_code <- plot_sunburst(df_reduced)
    expect_s3_class(p_code, "plotly")
    expect_s3_class(p_code, "htmlwidget")

    # Test plotting from btx_cls (using mock data frame)
    mock_cls <- data.frame(
      query_id = c("seq1", "seq2"),
      final_code = c("1.4.1.0.0.0", "1.1.1.4.0.0"),
      stringsAsFactors = FALSE
    )
    class(mock_cls) <- c("btx_cls", "data.frame")
    p_cls <- plot_sunburst(mock_cls)
    expect_s3_class(p_cls, "plotly")
    expect_s3_class(p_cls, "htmlwidget")
  }
})

