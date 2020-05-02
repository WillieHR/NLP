
library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidytext)
library(ggplot2)
library(gutenbergr)
library(tidyverse)


# devtools::install_github("bradleyboehmke/harrypotter")

library(harrypotter) # Este paquete se instala especialmente con el código de la anterior línea.


###########################################
###### CLASE 1: THE TIDY TEXT FORMAT ######
###########################################

# Vamos a intentar aprender el formaot Tidy text. Es bastante útil cuando se 
# utilizan textos tipo libros.

# Escribimos una de las citas más importantes de Edgar Alan Poe

textPoe <- c("I became insane, with long intervals of horrible sanity.",
             "All that we see or seem is but a dream within a dream.",
             "We loved with a love that was more than love.",
             "I was never really insane except upon occasions when my heart was touched.")

# Posteriormente la convertimos a tidy format, y la observamos.

text_df <- tibble(line = 1:4,text = textPoe)
text_df

# Posteriormente lo tokenizamos.Explicar que es tokenizar.

text_df %>%
  unnest_tokens(word,text)

# Hay un paquete chevere de los libros de Austen Jane

View(austen_books()) # Muestro que está organizado como un libro.

# Organizamos el texto en texto, libro, linea y capítulo.

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,regex("^chapter [\\divclc]",
                                                ignore_case = TRUE)))) %>%
  ungroup()

View(original_books)

# Proseguimos a tokenizarlos(Pero por libro, linea y capítulo.)

tidy_books <- original_books %>% 
  unnest_tokens(word,text)

View(tidy_books)

data("stop_words") # Explicar que son las stopwords.

# Quitamos las stop_words, para observar las palabras que más se repiten.

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE)

# Un gráfico interesante para ver las palabras que más se repiten.

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Ahora explicar que es el proyecto gutenber y mostrarlo.


View(gutenberg_metadata)

# Obsservar cuáles son los textos que estan en español

View(gutenberg_metadata[gutenberg_metadata$language == "es",])

# Por ejemplo los libros de Caroll Lewis

Carroll_Lewis <- gutenberg_metadata %>%
  filter(author == "Carroll, Lewis")

View(Carroll_Lewis)

# Descarguemos un libro  ### http://www.gutenberg.org/



### CASO HARRY POTTER


# philosophers_stone: Harry Potter and the Philosophers Stone (1997)
# chamber_of_secrets: Harry Potter and the Chamber of Secrets (1998)
# prisoner_of_azkaban: Harry Potter and the Prisoner of Azkaban (1999)
# goblet_of_fire: Harry Potter and the Goblet of Fire (2000)
# order_of_the_phoenix: Harry Potter and the Order of the Phoenix (2003)
# half_blood_prince: Harry Potter and the Half-Blood Prince (2005)
# deathly_hallows: Harry Potter and the Deathly Hallows (2007)

philosophers_stone[1:2]

text_tb <- tibble(chapter = seq_along(philosophers_stone),
                  text = philosophers_stone)

text_tb

text_tb %>%
  unnest_tokens(word, text)

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}


series$book <- factor(series$book, levels = rev(titles))


series %>%
  anti_join(stop_words) %>%
  group_by(book) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(book = factor(book, levels = titles),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(word, text_order), n, fill = book)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ book, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

# Tarea: Escoger un libro y realizar un grafico de las 10 primeras palabras con su frecuencia.
#        Escoger un libro del mismo autor y realizar un grafico de comparación palabras.

