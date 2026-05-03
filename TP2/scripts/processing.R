#########################

#title: "Trabajo Práctico 2: web scraping y procesamiento del lenguaje natural"
#author: "Juan Ignacio Gutiérrez"
#script: procesamiento de texto de comunicados de prensa de la OEA

#########################

# Invocar librerías
library(dplyr)
library(stringr)
library(here)
library(udpipe)
library(stopwords)

# Crear carpeta output si no existe
if (!dir.exists(here("TP2", "output"))) {
  dir.create(here("TP2", "output"))
  message("Carpeta output creada")
} else {
  message("Carpeta output ya existe")
}

# Cargar la tabla generada por el script de scraping
message("Leyendo archivo de comunicados scrapeados...")

comunicados_oea <- readRDS(
  here("TP2", "data", "comunicados_oea_enero_abril_2026.rds")
)

head(comunicados_oea)
dim(comunicados_oea)
sum(is.na(comunicados_oea$cuerpo))

# Limpiar el cuerpo de cada comunicado
message("Limpiando texto de los comunicados...")

comunicados_limpios <- comunicados_oea |>
  mutate(
    cuerpo_limpio = cuerpo,
    cuerpo_limpio = str_to_lower(cuerpo_limpio),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:punct:]]", " "),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:digit:]]", " "),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[^[:alpha:]áéíóúüñ\\s]", " "),
    cuerpo_limpio = str_squish(cuerpo_limpio)
  )

head(comunicados_limpios)

# Cargar modelo de lematización en español
message("Cargando modelo de lematización en español...")

modelo_descargado <- udpipe_download_model(
  language = "spanish",
  overwrite = FALSE
)

modelo_es <- udpipe_load_model(modelo_descargado$file_model)

# Lematizar el cuerpo limpio de cada comunicado
message("Lematizando comunicados...")

comunicados_lemas <- udpipe_annotate(
  modelo_es,
  x = comunicados_limpios$cuerpo_limpio,
  doc_id = comunicados_limpios$id
) |>
  as.data.frame() |>
  as_tibble() |>
  select(id = doc_id, lemma, upos)

head(comunicados_lemas)
dim(comunicados_lemas)
count(comunicados_lemas, upos, sort = TRUE)

# Cargar stopwords
stopwords_es <- stopwords::stopwords("es")

stopwords_es <- tibble(
  lemma = stopwords_es
)

# Filtrar sustantivos, verbos y adjetivos; pasar a minúscula; remover stopwords
message("Procesando lemmas relevantes...")

processed_text <- comunicados_lemas |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(
    lemma = str_to_lower(lemma),
    lemma = str_replace_all(lemma, "[[:punct:]]", ""),
    lemma = str_replace_all(lemma, "[[:digit:]]", ""),
    lemma = str_squish(lemma)
  ) |>
  anti_join(stopwords_es, by = "lemma") |>
  filter(
    lemma != "",
    !is.na(lemma),
    str_length(lemma) > 2
  ) |>
  left_join(
    comunicados_oea |> select(id, titulo),
    by = "id"
  ) |>
  select(id, titulo, lemma, upos)

head(processed_text)
dim(processed_text)
count(processed_text, upos, sort = TRUE)
sum(is.na(processed_text$lemma))

# Guardar texto procesado
saveRDS(
  processed_text,
  file = here("TP2", "output", "processed_text.rds")
)

message("Archivo processed_text.rds guardado correctamente en output")