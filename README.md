# bactaxidR

`bactaxidR` is an R package containing utilities to facilitate the use of [BacTaxID](https://github.com/irycisBioinfo/BacTaxID), a universal bacterial genomic classification system.

The package includes:
- A precompiled, compatible `bactaxid` executable for Linux bundled directly inside the package (`inst/bin/bactaxid`).
- Helper functions to download and decompress reference DuckDB database files for specific genera from Zenodo ([17791772](https://zenodo.org/records/17791772)).
- A `classify` function to perform taxonomic classification on a set of FASTA files interactively or non-interactively, returning a structured R `data.frame` of S3 class `btx_cls`.
- A `get_code_table` function to import the reference classification table from DuckDB database files, returning an R `data.frame` of S3 class `btx_code`.
- A `plot_sunburst` function to generate an interactive Plotly sunburst chart representing the hierarchical structure and frequencies of taxonomic codes from either database references or classification outputs.

---

## Installation

You can install the development version of `bactaxidR` directly from GitHub using `devtools` or `remotes`:

```R
# Install devtools or remotes if needed
# install.packages("devtools")
# install.packages("remotes")

# Install bactaxidR from GitHub
devtools::install_github("irycisBioinfo/bactaxidR")
# or
remotes::install_github("irycisBioinfo/bactaxidR")
```

Alternatively, you can install `bactaxidR` locally from the package directory:

```R
# In R:
# devtools::install_local("path/to/bactaxidR")
```

Or directly from source using the terminal:

```bash
R CMD INSTALL .
```

---

## Basic Usage

### 1. List Available Databases

You can list all bacterial genera that have official reference databases on Zenodo:

```R
library(bactaxidR)

# Query the Zenodo API for the latest list of available genera
genera <- list_available_dbs()
print(genera)
```

### 2. Download Database for a Genus

To download the reference database for a genus (e.g. *Escherichia* or *Salmonella*), which will be downloaded compressed and automatically uncompressed into the user data directory:

```R
# Download database
db_path <- download_db(genus = "Escherichia")
message("Database saved at: ", db_path)
```

### 3. Classify Genomes (FASTA)

The `classify` function takes a vector of FASTA file paths, validates the presence of the database and executable, runs the BacTaxID hierarchical classification, and returns a data frame with the results of class `btx_cls`.

If `genus` and `db_path` are not specified and you are in an interactive session, the package will prompt you to enter the genus:

```R
# Paths to your FASTA genomes
my_genomes <- c("sample1.fasta", "sample2.fna")

# Run classification
results <- classify(
  fasta_files = my_genomes,
  genus = "Escherichia"
)

# Print classification results data frame
print(results)
```

The returned `data.frame` columns correspond to the BacTaxID DuckDB engine results:
- `query_id`: Identifier of the queried genome.
- `query_signature`: Computed query signature.
- `best_hit_id`: Identifier of the best matching reference genome.
- `best_hit_signature`: Signature of the best match.
- `similarity_score`: Estimated similarity score (based on ANI).
- `levels_reached`: Taxonomic depth/levels reached during classification.
- `final_code`: Assigned hierarchical taxonomic code (e.g., `1.3.1.8.12.1`).
- `best_hit_code`: Hierarchical taxonomic code of the best hit.

---

### 4. Get Reference Code Table (`get_code_table`)

You can import the full table of hierarchical reference classification codes from a genus-specific DuckDB database:

```R
# Get the reduced table (only sample and L_0 to L_5 level codes)
code_table <- get_code_table("test_data/Serratia.db", full_table = FALSE)
print(head(code_table))
```

The returned data frame inherits the custom `btx_code` S3 class, allowing for easy identification in analytical workflows.

---

### 5. Plot Interactive Sunburst Chart (`plot_sunburst`)

You can visualize the frequency and hierarchical distribution of taxonomic classifications (either reference tables from `get_code_table` or classification outputs from `classify`):

```R
# Plot the interactive sunburst using Plotly
fig <- plot_sunburst(code_table, root_label = "Serratia")

# Render the interactive plot in RStudio or your web browser
fig
```

The resulting sunburst chart allows for interactive navigation by clicking on internal sectors to zoom in on sub-levels and view frequencies.

---

## License

This package is licensed under the GPL-3 License.
