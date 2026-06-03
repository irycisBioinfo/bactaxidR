# bactaxidR

`bactaxidR` es un paquete de R con utilidades para facilitar el uso de [BacTaxID](https://github.com/irycisBioinfo/BacTaxID), un sistema universal de tipado bacteriano basado en genomas. 

El paquete incluye:
- El ejecutable compatible de `bactaxid` compilado para Linux integrado directamente en el paquete.
- Funciones para descargar y descomprimir de forma nativa las bases de datos DuckDB de géneros específicos publicadas en Zenodo ([17791772](https://zenodo.org/records/17791772)).
- Una función `classify` para realizar análisis taxonómico de un conjunto de archivos FASTA de forma interactiva o no interactiva, devolviendo un `data.frame` de R estructurado.

---

## Instalación

Puedes instalar `bactaxidR` localmente en tu sistema desde el directorio del paquete usando:

```R
# En R:
# devtools::install_local("ruta/a/bactaxidR")
```

O instalándolo directamente desde el código fuente o tarball en la terminal:

```bash
R CMD INSTALL bactaxidR
```

---

## Uso Básico

### 1. Listar bases de datos disponibles

Puedes listar todos los géneros bacterianos que tienen bases de datos de referencia oficiales en Zenodo:

```R
library(bactaxidR)

# Consulta la API de Zenodo para obtener la lista actualizada
generos <- list_available_dbs()
print(generos)
```

### 2. Descargar base de datos para un género

Para descargar la base de datos DuckDB de un género (por ejemplo, *Escherichia* o *Salmonella*), la cual se descargará comprimida y se descomprimirá en el directorio de datos del usuario de forma automática:

```R
# Descargar base de datos
db_path <- download_db(genus = "Escherichia")
message("Base de datos guardada en: ", db_path)
```

### 3. Clasificar Genomas (FASTA)

La función `classify` toma un vector de archivos FASTA, valida la presencia de la base de datos y del ejecutable, realiza la búsqueda jerárquica de BacTaxID y retorna un data.frame con los resultados.

Si no se especifica el parámetro `genus` ni `db_path`, y te encuentras en una sesión interactiva, el paquete te solicitará de forma interactiva que introduzcas el género deseado:

```R
# Rutas a tus genomas FASTA
mis_genomas <- c("sample1.fasta", "sample2.fna")

# Clasificación
resultados <- classify(
  fasta_files = mis_genomas,
  genus = "Escherichia"
)

# Visualizar la clasificación en un data.frame
print(resultados)
```

Las columnas devueltas en el `data.frame` corresponden a los resultados del motor DuckDB de BacTaxID:
- `query_id`: Identificador del genoma consultado.
- `query_signature`: Firma calculada para la query.
- `best_hit_id`: Identificador de la mejor coincidencia de referencia.
- `best_hit_signature`: Firma de la mejor coincidencia.
- `similarity_score`: Nivel de similitud estimado (basado en ANI).
- `levels_reached`: Profundidad/niveles jerárquicos alcanzados en la clasificación.
- `final_code`: Código taxonómico jerárquico asignado (ej. `1.3.1.8.12.1`).
- `best_hit_code`: Código taxonómico jerárquico del mejor hit.

### 4. Obtener Tabla de Códigos de Referencia (`get_code_table`)

Puedes importar la tabla completa de códigos taxonómicos de referencia jerárquicos desde la base de datos DuckDB de cualquier género:

```R
# Obtener la tabla reducida (solo sample y códigos de niveles L_0 a L_5)
tabla_codigos <- get_code_table("test_data/Serratia.db", full_table = FALSE)
print(head(tabla_codigos))
```

El data.frame devuelto tiene la clase personalizada `btx_code`, lo que permite identificarlo fácilmente en flujos analíticos.

### 5. Graficar Gráfico Interactivo Sunburst (`plot_sunburst`)

Puedes visualizar de forma interactiva la frecuencia y distribución jerárquica de las clasificaciones taxonómicas (ya sean referencias obtenidas con `get_code_table` o resultados de clasificación obtenidos con `classify`):

```R
# Graficar el sunburst interactivo usando Plotly
grafico <- plot_sunburst(tabla_codigos, root_label = "Serratia")

# Renderizar el gráfico interactivo en RStudio o tu navegador
grafico
```

El gráfico sunburst resultante permite navegar de forma interactiva haciendo clic en los sectores jerárquicos internos para explorar sub-niveles y ver las proporciones y abundancias correspondientes.

---

## Licencia

Este paquete está licenciado bajo los términos de la licencia GPL-3.
