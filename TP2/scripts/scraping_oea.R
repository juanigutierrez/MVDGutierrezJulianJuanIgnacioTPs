######################### 

#title: "Trabajo Práctico 2: web scraping y procesamiento del lenguaje natural"
#author: "Juan Ignacio Gutiérrez"
#script: scraping de comunicados de prensa de OEA

######################### 

# Carga de librerías
library(rvest)
library(dplyr)
library(purrr)
library(here)
library(xml2)

# Crear estructura de carpetas
dir.create(here("TP2"))
dir.create(here("TP2", "notebooks"))
dir.create(here("TP2", "data"))
dir.create(here("TP2", "scripts"))
dir.create(here("TP2", "output"))

# Crear archivos de scripts y notebooks
file.create(here("TP2", "scripts", "scraping_oea.R"))
file.create(here("TP2", "scripts", "processing.R"))
file.create(here("TP2", "scripts", "metrics_figures.R"))

file.create(here("TP2", "notebooks", "informe_oea.qmd"))

# Defino los meses a scrapear
meses_oea <- tibble(
  numero_mes = c(1, 2, 3, 4),
  nombre_mes = c("enero", "febrero", "marzo", "abril")
)

# Función para extraer el cuerpo de un comunicado
extraer_cuerpo <- function(link) {
  
  message("Leyendo comunicado: ", link)
  
  Sys.sleep(3)
  
  pagina <- read_html(link)
  
  cuerpo <- pagina |>
    html_elements("p:nth-child(5), p:nth-child(6)") |>
    html_text2()
  
  if (length(cuerpo) == 0) {
    return(NA_character_)
  } else {
    return(paste(cuerpo, collapse = " "))
  }
}

# Función para scrapear un mes completo
scrapear_mes <- function(numero_mes, nombre_mes) {
  
  url_mes <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    numero_mes,
    "&nAnio=2026"
  )
  
  message("Leyendo página de comunicados de ", nombre_mes, " de 2026")
  
  pagina_mes <- read_html(url_mes)
  
  message("Página de ", nombre_mes, " leída correctamente")
  
  fecha_descarga <- Sys.Date()
  
  write_html(
    pagina_mes,
    file = here(
      "TP2",
      "data",
      paste0("oea_comunicados_", nombre_mes, "_2026_", fecha_descarga, ".html")
    )
  )
  
  message("HTML de ", nombre_mes, " guardado en data")
  
  nodos_mes <- pagina_mes |>
    html_elements("#rightmaincol td:nth-child(2) a")
  
  titulos_mes <- nodos_mes |>
    html_text2()
  
  links_mes <- nodos_mes |>
    html_attr("href")
  
  links_completos <- links_mes |>
    url_absolute(url_mes)
  
  tabla_mes <- tibble(
    id = paste0(nombre_mes, "_", seq_along(titulos_mes)),
    titulo = titulos_mes,
    link = links_completos
  )
  
  head(tabla_mes)
  
  tabla_mes$cuerpo <- map_chr(tabla_mes$link, extraer_cuerpo)
  
  sum(is.na(tabla_mes$cuerpo))
  
  head(tabla_mes)
  
  tabla_mes_final <- tabla_mes |>
    select(id, titulo, cuerpo)
  
  return(tabla_mes_final)
}

# Scrapeo enero, febrero, marzo y abril
comunicados_oea <- map2_dfr(
  meses_oea$numero_mes,
  meses_oea$nombre_mes,
  scrapear_mes
)

# Checkeo que este todo bien 
sum(is.na(comunicados_oea$cuerpo))
head(comunicados_oea)
dim(comunicados_oea)
names(comunicados_oea)

# Guardo la tabla final
saveRDS(
  comunicados_oea,
  file = here("TP2", "data", "comunicados_oea_enero_abril_2026.rds")
)