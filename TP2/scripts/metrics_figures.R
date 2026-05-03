#########################

#title: "Trabajo Práctico 2: web scraping y procesamiento del lenguaje natural"
#author: "Juan Ignacio Gutiérrez"
#script: métricas y figuras de comunicados de prensa de la OEA

#########################

# Cargar de librerías
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Cargar el texto procesado generado en processing.R
message("Leyendo archivo processed_text.rds...")

processed_text <- readRDS(
  here("TP2", "output", "processed_text.rds")
)

head(processed_text)
dim(processed_text)
names(processed_text)

# Computar frecuencia de términos por comunicado
message("Computando frecuencia de términos...")

frecuencia_por_comunicado <- processed_text |>
  count(id, lemma, name = "frecuencia")

head(frecuencia_por_comunicado)
dim(frecuencia_por_comunicado)

# Construir matriz documento término
dtm_oea <- frecuencia_por_comunicado |>
  pivot_wider(
    names_from = lemma,
    values_from = frecuencia,
    values_fill = 0
  )

head(dtm_oea)
dim(dtm_oea)

# Guardar DTM
saveRDS(
  dtm_oea,
  file = here("TP2", "output", "dtm_oea.rds")
)

# Mirar términos más frecuentes del corpus para seleccionar palabras relevantes
terminos_frecuentes <- processed_text |>
  count(lemma, sort = TRUE)

head(terminos_frecuentes, 30)

# Seleccionar 5 términos relevantes para el contexto institucional de la OEA
terminos_relevantes <- c(
  "derecho",
  "elección",
  "democracia",
  "seguridad",
  "observación"
)

# Filtrar y condensar frecuencia total de los términos seleccionados
frecuencia_terminos <- frecuencia_por_comunicado |>
  filter(lemma %in% terminos_relevantes) |>
  group_by(lemma) |>
  summarise(
    frecuencia_total = sum(frecuencia),
    .groups = "drop"
  ) |>
  arrange(desc(frecuencia_total))

frecuencia_terminos

# Guardar tabla de frecuencia de términos
saveRDS(
  frecuencia_terminos,
  file = here("TP2", "output", "frecuencia_terminos.rds")
)

# Generar gráfico de barras
message("Generando gráfico de barras...")

grafico_frecuencia <- ggplot(
  frecuencia_terminos,
  aes(x = reorder(lemma, frecuencia_total), y = frecuencia_total)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Frecuencia total de términos seleccionados en comunicados de la OEA",
    x = "Término",
    y = "Frecuencia total"
  ) +
  theme_minimal()

grafico_frecuencia

# Guardar figura
ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = grafico_frecuencia,
  width = 8,
  height = 5
)

message("Figura frecuencia_terminos.png guardada correctamente en output")