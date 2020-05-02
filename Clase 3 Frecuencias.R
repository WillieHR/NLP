
library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)


############################################################
###### CLASE 3: Analyzing word and document frequency ######
############################################################

# Una pregunta central en la minería de texto y el procesamiento
# de lenguaje natural es como cuantificar un documento.
# Será que solo se logra al contar palabras? esta metodología
# en ingles se llama term frequency (tf) responde a la pregunta
# ¿Qué tan frecuente aparece una palabra en el capítulo 1 por ejemplo?

# Hay algunas palabras ocurren bastante en ingles como son 
# "the", "is", "of", entre otras. Podemos agregar estas 
# palabras en las stopwords y quitarlas, pero puede 
# que estas palabras son más importantes en unos textos
# que en otros. Técnicamente quitar lo que no nos conviene
# no es lo más sofistacado.

# Otro método es mirar la frecuencia inversa, el cual 
# aumenta el peso para palabras que no se usan en una coleccion
# de documentos pero decrece cuando se usa basante en 
# estos textos. Este método se conoce como inverse document
# frequency (idf). La idea es combinar estos dos métodos
# para mejorar la identificación de un texto.

##### tf-idf #####  DILEMA DEL CAPÍTULO

# Tokenizamos por libro

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# Tokenizamos en total.

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

# Comparar las dos tablas anteriores

book_words <- left_join(book_words, total_words)

book_words

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf's law states that the frequency that a word appears is inversely proportional to its rank.

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10()

# La idea del método tf-idf es encontrar palabras importantes 

# La idea de tf-idf es encontrar las palabras importantes para el contenido
# de cada documento al disminuir el peso de las palabras de uso común
# y aumentando el peso de las palabras que no se usan mucho en una colección
# o corpus de documentos, en este caso, el grupo de novelas de Jane Austen 
# en su conjunto. Calcular tf-idf intenta encontrar las palabras que son 
# importantes (es decir, comunes) en un texto, pero que no son demasiado comunes
# en los otros documentos.

# Dar el ejemplo con los hablados paisa, pastuso, santandereano.

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Seria interesante observar que textos a veces nos sirven para encontrar lo mas importante
# quitando lo mas similar.

# Aqui observamos los sustantivos más importantes debido a que aparecen en esta novela pero no 
# en las otras, es interesante comparar textos con los similares para obtener lo más importantes
# insights. 


book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Tarea: Escoga un libro de gutenberg y seleccione 3 libros adicionales que sean del mismo tipo 
#        para hacer un analisis tf-idf. Igualmente, escoga uno de los libros de harry potter,
#        e intente encontrar las palabras con mayor indice tf-idf.


