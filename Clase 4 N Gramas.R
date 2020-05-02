
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)

#############################################
###### CLASE 4: Relation between words ######
#############################################

# Explicar los n-gramas

# Encontremos bigramas de las obras de Jane Austen

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

# Los separamos en dos columnas

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Quitamos stopwords de ambas columnas

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# contamos nuevamente
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Lo hacemos con tres palabras.

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Ahora busquemos los bigramas que la segunda palabra sea 'street'

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, word2,sort = TRUE)

# Hagamos un analisis tf-idf para los bigramos sin stopwords

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Un análisis de red seria interesante


bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Pares como "Elizabeth y "Darcy" son las palabras que más aparecen juntas,
# pero no es significativo porque son las palabras más comunes dentro del
# texto. Por tanto se desea ver la correlación entre palabras, al observar
# que tanto aparecen juntas vs el numero de veces que aparecen separadas.

# Por tanto observamos las palabras por sección en este caso cada 10 palabras.

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# Miramos los bigramas por sección.


word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs


# The phi coefficient is equivalent to the Pearson correlation, which you
# may have heard of elsewhere, when it is applied to binary data).

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Veamos la correlación de estas palabras con otras.

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


# Finalmente, ahora dibujamos nuevamente el gráfico de red solamente con las palabras con 
# alta correlación.

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Tarea: Encontrar los bigramas más comunes teniendo en cuenta y omitiendo los stop words, esto para el
#       texto en español. Realizar un análisis de correlación entre palabras.