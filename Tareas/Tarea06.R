# TAREA 06 - EDGAR API (SEC) & FINANCIAL TEXT MINING

library(edgar)
library(tidyverse)  
library(tidytext)
library(textdata)
library(httr)
library(jsonlite)

# 1. IDENTIFICACIÓN Y CONFIGURACIÓN DE EMPRESAS ARGENTINAS EN EL NYSE
empresas_argentinas <- tibble(
  empresa = c("YPF", "Grupo Financiero Galicia", "Banco Macro", "BBVA Argentina",
              "Pampa Energia", "TGS", "Central Puerto", "Loma Negra",
              "Telecom Argentina", "IRSA", "Cresud", "Despegar",
              "ADECOAGRO", "Mercado Libre"),
  ticker  = c("YPF", "GGAL", "BMA", "BBAR", "PAM", "TGS", "CEPU", "LOMA",
              "TEO", "IRS", "CRESY", "DESP", "AGRO", "MELI")
)

# Definir tu User-Agent institucional obligatorio por la SEC
user_agent <- "Uriel Kogan 20KO46914569@campus.economicas.uba.ar" 

# Consulta de CIKs directamente desde la API oficial de la SEC
respuesta <- GET(
  "https://www.sec.gov/files/company_tickers.json",
  add_headers(`User-Agent` = user_agent)
)

tickers_sec <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8")) %>%
  bind_rows() %>%
  select(ticker, cik = cik_str, nombre_sec = title)

empresas_argentinas <- empresas_argentinas %>%
  left_join(tickers_sec, by = "ticker")

# Asignación manual para Despegar (fuera del listado estándar actual de la SEC)
empresas_argentinas <- empresas_argentinas %>%
  mutate(cik = if_else(ticker == "DESP", 1692427, as.numeric(cik)))

ciks <- empresas_argentinas$cik %>% na.omit() %>% as.numeric()


# 2. DESCARGA DE REPORTES ANUALES 
descargar <- TRUE

if (descargar) {
  reportes <- getFilings(
    cik.no       = ciks,
    form.type    = c("20-F", "10-K"),
    filing.year  = 2020:2026,   
    quarter      = 1:4,
    downl.permit = "y",
    useragent    = user_agent
  )
}


# 3. CONSOLIDACIÓN Y LIMPIEZA DE ARCHIVOS
archivos <- list.files(
  "Edgar_filings",
  pattern    = "\\.txt$", 
  recursive  = TRUE,        
  full.names = TRUE         
)

tabla_archivos <- tibble(ruta = archivos, nombre = basename(archivos)) %>%
  separate(nombre, into = c("cik", "formulario", "anio", "numero"),
           sep = "_", extra = "merge") %>%
  mutate(cik = as.numeric(cik)) %>%
  left_join(empresas_argentinas, by = "cik") %>%
  mutate(empresa = coalesce(empresa, as.character(cik)))

leer_reporte <- function(ruta) {
  lineas <- readLines(ruta, warn = FALSE)
  texto  <- paste(lineas, collapse = " ")
  
  texto <- iconv(texto, from = "UTF-8", to = "ASCII", sub = " ")
  
  documentos <- str_split(texto, "<DOCUMENT>")[[1]]
  principal  <- documentos[str_detect(documentos, "^\\s*<TYPE>(20-F|10-K)")]
  
  if (length(principal) == 0) principal <- texto
  principal <- paste(principal, collapse = " ")
  
  principal <- str_remove_all(principal, "<[^>]+>")
  principal <- str_replace_all(principal, "&nbsp;|&#160;|&amp;", " ")
  
  str_to_lower(principal)
}


# 4. INCIDENCIA DE TÉRMINOS MACROECONÓMICOS
palabras_clave <- c("inflation", "devaluation", "exchange controls", "default",
                    "restructuring", "imf", "capital controls", "price controls",
                    "country risk", "embi")

lista_clave  <- list()   
lista_tokens <- list()   

for (i in 1:nrow(tabla_archivos)) {
  cat("Procesando", i, "de", nrow(tabla_archivos), ":",
      tabla_archivos$empresa[i], tabla_archivos$anio[i], "\n")
  
  texto <- leer_reporte(tabla_archivos$ruta[i])
  
  conteos <- sapply(palabras_clave, function(p) {
    str_count(texto, paste0("\\b", p, "\\b"))
  })
  
  lista_clave[[i]] <- tibble(
    empresa = tabla_archivos$empresa[i],
    anio    = tabla_archivos$anio[i],
    palabra = palabras_clave,
    veces   = as.numeric(conteos)
  )
  
  lista_tokens[[i]] <- tibble(texto = texto) %>%
    unnest_tokens(word, texto) %>%                     
    filter(str_detect(word, "^[a-z]+$"),               
           nchar(word) >= 3) %>%                       
    anti_join(stop_words, by = "word") %>%             
    count(word) %>%                                    
    mutate(empresa = tabla_archivos$empresa[i],
           anio    = tabla_archivos$anio[i])
}

conteo_clave <- bind_rows(lista_clave)
conteo_todas <- bind_rows(lista_tokens)

clave_por_empresa <- conteo_clave %>%
  group_by(empresa, palabra) %>%
  summarise(veces = sum(veces), .groups = "drop")

print("--- INCIDENCIA DE PALABRAS CLAVE ---")
print(clave_por_empresa, n = 50)


# 5. PALABRAS DISTINTIVAS POR EMPRESA (TF-IDF)
palabras_empresa <- conteo_todas %>%
  group_by(empresa, word) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  filter(n >= 10)      

tfidf <- palabras_empresa %>%
  bind_tf_idf(term = word, document = empresa, n = n)

top_tfidf <- tfidf %>%
  group_by(empresa) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup()

print("--- TOP TF-IDF POR EMPRESA ---")
print(top_tfidf, n = 150)


# 6. ANÁLISIS 3: SENTIMIENTO FINANCIERO 
loughran <- get_sentiments("loughran")

total_palabras <- palabras_empresa %>%
  group_by(empresa) %>%
  summarise(total = sum(n), .groups = "drop")

sentimientos <- palabras_empresa %>%
  inner_join(loughran, by = "word", relationship = "many-to-many") %>%
  group_by(empresa, sentiment) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  left_join(total_palabras, by = "empresa") %>%
  mutate(proporcion = n / total)      

print("SENTIMIENTOS LOUGHRAN-MCDONALD:")
print(sentimientos, n = 100)