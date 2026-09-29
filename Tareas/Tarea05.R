library(tidyverse)
library(tidytext)
library(topicmodels)
library(textdata)

# TAREA 05 TEXT MINING 

# 0. CARGA DE DATOS Y LIMPIEZA INICIAL

# Cargar dataset
mckinsey_data <- read_csv("DATA-T9-mckinsey-mind-the-gap-articles-20251020.csv")

# Renombro columna de texto para estandarizar
if ("article_text" %in% colnames(mckinsey_data)) {
  mckinsey_data <- mckinsey_data |> rename(text = article_text)
} else {
  col_texto <- mckinsey_data |> select(where(is.character)) |> colnames() |> head(1)
  mckinsey_data <- mckinsey_data |> rename(text = all_of(col_texto))
}

# Creo un id de documento por fila si no viene en el CSV
if (!"doc_id" %in% colnames(mckinsey_data)) {
  mckinsey_data <- mckinsey_data |> mutate(doc_id = row_number())
}



# 1. EXPLORACIÓN Y DESCRIPCIÓN DEL CORPUS


# RESPUESTAS:
# ¿De qué se trata este corpus?
# Es una recopilacion de notas y articulos publicados por la consultora McKinsey 
# centrados en la serie Mind the Gap. El tema principal son los problemas de 
# diversidad, equidad e inclusion en el entorno laboral, con foco en la brecha 
# de genero, representacion femenina en cargos directivos y liderazgo corporativo.
#
# ¿Son comparables los documentos?
# Si, son totalmente comparables. Provienen de la misma fuente institucional,
# manejan un vocabulario formal y corporativo uniforme y tratan sobre la misma 
# tematica general. Esto permite aplicar metricas como TF-IDF, analisis de 
# sentimiento y modelos de topicos sin sesgos graves por cambio de contexto.
#
# Aspectos destacados del corpus:
# Las palabras mas frecuentes giran en torno a liderazgo, empleo, brechas y genero.
# TF-IDF ayuda a ver la especificidad de cada nota (por ejemplo, algunas se enfocan 
# en ascensos, otras en brecha salarial o politicas de maternidad).

# Tokenizacion y saco stopwords en ingles
words_df <- mckinsey_data |> 
  unnest_tokens(output = word, input = text) |> 
  anti_join(stop_words, by = "word")

# Palabras mas repetidas 
words_df |> 
  count(word, sort = TRUE) |> 
  head(15) |> 
  print()

# Analisis TF-IDF para encontrar terminos distintivos por articulo
tf_idf_df <- words_df |> 
  count(doc_id, word) |> 
  bind_tf_idf(word, doc_id, n) |> 
  arrange(desc(tf_idf))

tf_idf_df |> head(10) |> print()

# 2. ANÁLISIS DE SENTIMIENTO

# 2.1 Diccionario Binario 
bing_sentiment <- words_df |> 
  inner_join(get_sentiments("bing"), by = "word") |> 
  count(doc_id, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  mutate(net_sentiment = positive - negative)

print(bing_sentiment)

# 2.2 Diccionario Graduado 
afinn_sentiment <- words_df |> 
  inner_join(get_sentiments("afinn"), by = "word") |> 
  group_by(doc_id) |> 
  summarize(score_promedio = mean(value), score_total = sum(value))

print(afinn_sentiment)



# 3. TOPIC MODELLING 


# Creo la DTM
dtm_mckinsey <- words_df |> 
  count(doc_id, word) |> 
  cast_dtm(doc_id, word, n)

# Modelo para K = 10 topicos
lda_k10 <- LDA(dtm_mckinsey, k = 10, control = list(seed = 1234))

top_terms_k10 <- tidy(lda_k10, matrix = "beta") |> 
  group_by(topic) |> 
  slice_max(order_by = beta, n = 5) |> 
  ungroup()

print(top_terms_k10)

# Modelo para K = 15 topicos
lda_k15 <- LDA(dtm_mckinsey, k = 15, control = list(seed = 1234))

top_terms_k15 <- tidy(lda_k15, matrix = "beta") |> 
  group_by(topic) |> 
  slice_max(order_by = beta, n = 5) |> 
  ungroup()

print(top_terms_k15)