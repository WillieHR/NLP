
library(tidytext)
library(textdata)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(wordcloud)
library(wordcloud2)
library(reshape2)


########################################################
###### CLASE 2: Sentiment analysis with tidy data ######
########################################################

# Hay una gran variedad de m�todos y direccionarios que existen
# para evaluar la opini�n o emoci�n en texto. El paquete tidytext
# provee acceso a un l�xico de sentimientos.

# _AFINN_ from Finn �rup Nielsen,
# _bing_ from Bing Liu and collaborators, and
# _nrc_ from Saif Mohammad and Peter Turney.

# Los tres l�xicos son unigramas de single words. 
# Estos l�xicos contienen muchas palabras en ingles y estas palabras
# se les asignan score de positivo/negativo y tambi�n emociones como 
# alegr�a, furia, tristeza entre otros. El l�xico 'nrc' categoriza 
# en positivo, negativo, rabia, disgusto, miedo, alegr�a, tristeza,
# sorpresa, etc. El l�xico 'bing' clasifica las palabras binariamente
# en positivo y negativo. El l�xico 'afinn' por otro lado usa un score
# que va de -5 a 5, siendo -5 muy negativo y 5 muy positivo.

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")

# Nuevamente voy a trabajar con los libros de Jane Austen

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Solamente filtrar� del l�xico bing las palabras positivas y cruzarlas con los libros
# de Jane Austen

bing_pos <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(bing_pos) %>%
  count(word, sort = TRUE)

# Mirar las palabras negativas y positivas en las obras de Jane Austen


bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# Y claramente dibujar los resultados.

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# Contar a trav�s del texto el sentimiento.

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# Si hay palabras que sabemos que no son negativas- �C�mo las quitamos?

# Utilizar un wordcloud para observar las palabras (recordar quitar stopwords)


tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Ahora por colores


tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue1", "firebrick1"),
                   max.words = 100)


# El mundo no es blanco y negro. � Uno podr�a generar an�lisis sentimientos no bipolares?


# Tarea: Escoger un libro en espa�ol de gutenberg y realizar un wordcloud de sentimientos.
#        Implicitamente le estoy pidiendo que investigue como se consigue este m�todo en espa�ol.


