## Text Mining

library(tm)

## creating sample text vector
text <- c("Dogs are the best pets",
          "Are these dogs yours?",
          "My dogs are the best dogs")

vs <- VectorSource(text)

corpus <- VCorpus(vs)
corpus

inspect(corpus[[2]])

corpus[[2]][1] # look at content of the second object

corpus[[2]][2] # access metadata of the second object

meta(corpus, tag = 'language') # language metadata

meta(corpus[[1]], tag = 'class') <- "CSE252" # adding your own metadata
corpus[[1]][2]

## document-term matrix
dtm <-DocumentTermMatrix(corpus)
inspect(dtm)

## term-document matrix (transposed version)
tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

mm <- as.matrix(dtm) # create a matrix

## Sparsity
sum(mm == 0)/(3 * 8)

library(stringr)

## remove punctuation to eliminate repetitions
text <- str_remove_all(text, pattern = "[:punct:]")
text

## same with 'tm' functionality
corpus <- tm_map(corpus, removePunctuation)

## constructing with tf-idf values
dtml1 <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
inspect(dtml1)

## tf-idf for pets (manual calculation)
(1 / 5) * log(3 / 1, base = 2)

## import lyrics data
lyrics <- read.csv("lyrics.csv", stringsAsFactors = F)
summary(lyrics)

library(dplyr)

summary(lyrics$year)


## look at some year statistics to see what needs to be cleaned

lyrics %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(year))
  
lyrics %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year)
  
  
library(ggplot2)
  
ggplot(data = lyrics, aes(x = genre)) +
  geom_bar() +
  ggtitle("Songs distribution by genre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
## most popular artists
lyrics %>%
  group_by(artist) %>%
  summarise(n_songs = n()) %>%
  arrange(desc(n_songs))

## variable with number of characters in lyrics of songs
lyrics$char_num <- nchar(lyrics$lyrics)
summary(lyrics$char_num)

lyrics <- lyrics %>%
  filter(char_num > 100 & genre != "Not Available" & year > 1968 & year <= 2016)

dim(lyrics) # look at dimensions

## delete all non ASCII characters
lyrics$lyrics <- iconv(lyrics$lyrics, to = "ASCII", sub = "")

## analyze lyrics of Beyonce
beyonce <- lyrics[lyrics$artist == 'beyonce-knowles', ]
beyonce_vs <- VectorSource(beyonce$lyrics)
beyonce_corpus <- VCorpus(beyonce_vs)

stopwords("english") # english stopwords

## stemming example
love <- c("love", "loving", "lovingly", "loved", "lover", "lovely", "love", "game", "gaming", "gamification")
stemDocument(love)

## create tdm with some control parameters
beyonce_tdm <- TermDocumentMatrix(beyonce_corpus,
                                  control = list(removeNumbers = T, removePunctuation = T, 
                                                 stopwords = T, stemming = T))

beyonce_tdm

## looking for most frequent words used by Beyonce
tdm_mat <- as.matrix(beyonce_tdm)
freqs <- rowSums(tdm_mat)
df_freq <- data.frame(terms = rownames(tdm_mat),
                      freq = freqs, stringsAsFactors = F)

## sort in decreasing order
df_freq <- df_freq[order(df_freq$freq, decreasing = T), ]
head(df_freq)

df_top10 <- df_freq[1:10, ]

## barplot of top 10 words for Beyonce
ggplot(df_top10, aes(x = reorder(terms, freq), y = freq)) +
  geom_bar(stat = 'identity', fill = brewer.pal(n = 10, name = 'Spectral')) +
  coord_flip()+
  labs(x = 'Terms', y = 'Frequency', title = 'Top 10 Words')


library(wordcloud)

## create a wordcloud
set.seed(1)
wordcloud(words = df_freq$terms, freq = df_freq$freq, 
          min.freq = 10, max.words = 200, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))



## create tdm again but with tf-idf
beyonce_tdm1 <- TermDocumentMatrix(beyonce_corpus,
                                  control = list(removeNumbers = T, removePunctuation = T, 
                                                 stopwords = T, stemming = T, weighting = weightTfIdf))

tdm_mat1 <- as.matrix(beyonce_tdm1)
freqs1 <- rowSums(tdm_mat1)
df_freq1 <- data.frame(terms = rownames(tdm_mat1),
                      freq = freqs1, stringsAsFactors = F)

df_freq1 <- df_freq1[order(df_freq1$freq, decreasing = T), ]

## we get a new wordcloud
set.seed(1)
wordcloud(words = df_freq1$terms, freq = df_freq1$freq, 
          min.freq = 10, max.words = 200, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


## comparing lyrics from metal and pop
metal <- lyrics[lyrics$genre == 'Metal' & lyrics$year > 2012, ] # after 2012 to save time
pop <- lyrics[lyrics$genre == 'Pop' & lyrics$year > 2012, ] # after 2012 to save time

metal_vc <- VCorpus(VectorSource(metal$lyrics))
pop_vc <- VCorpus(VectorSource(pop$lyrics))

metal_tdm <- TermDocumentMatrix(metal_vc,
                                control = list(removeNumbers = T, removePunctuation = T, 
                                                       stopwords = T, stemming = T))

pop_tdm <- TermDocumentMatrix(pop_vc,
                                control = list(removeNumbers = T, removePunctuation = T, 
                                               stopwords = T, stemming = T))

inspect(metal_tdm)
inspect(pop_tdm)

pop <- removeSparseTerms(pop_tdm, 0.99)
inspect(pop_tdm)

pop_mat <- as.matrix(pop_tdm)
pop_freq <- data.frame(terms = rownames(pop_mat),
                       freq = rowSums(pop_mat))

wordcloud(words = pop_freq$terms, freq = pop_freq$freq, 
          min.freq = 10, max.words = 200, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

#######
# add metal wodcloud and check pop
#######

metal1 <- paste(metal$lyrics, collapse = "")
pop1 <- paste(pop$lyrics, collapse = "")

metal_pop <- c(metal1, pop1)

metal_pop_vc <- VCorpus(VectorSource(metal_pop))

metal_pop_tdm <- TermDocumentMatrix(metal_pop_vc,
                                    control = list(removeNumbers = T, removePunctuation = T, 
                                                                 stopwords = T, stemming = T))

inspect(metal_pop_tdm)

metal_pop_mat <- as.matrix(metal_pop_tdm)
colnames(metal_pop_mat) <- c("Metal", "Pop")

comparison.cloud(metal_pop_mat, max.words = 200, random.order = FALSE,
          colors = c("black", "red"))

common <- data.frame(terms = rownames(metal_pop_mat), metal_pop_mat) %>%
  filter(Metal > 0 & Pop >0)

head(common)

ggplot(data = common, aes(x = Metal, y = Pop)) +
  geom_point() +
  geom_text(aes(label = terms)) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  ggtitle("Words in Pop vs Metal")


## Sentiment Analysis with R
library(qdap)

deamplification.words # de-amplifiers from 'qdap
amplification.words # amplifiers from 'qdap
negation.words # negation words from 'qdap
negative.words # negative words from 'qdap
positive.words # positive words from 'qdap

str <- c("This is a very good class", "I hate coming early in the morning",
         "I deeply love the course, but I hate Sports")
str_df <- data.frame(str, author = c(1:3))

## calculating polarity score
polarity(text.var = str_df$str, grouping.var = str_df$author)

## calculating polarity scores for beyonce
beyonce <- lyrics[lyrics$artist == 'beyonce-knowles', ]

beyonce <- beyonce[1:50, ]

beyonce_sent <- polarity(text.var = beyonce$lyrics, grouping.var = beyonce$song)
beyonce_scores <- scores(beyonce_sent)
head(beyonce_scores)

## most positive songs by Beyonce
beyonce_scores %>%
  arrange(desc(ave.polarity)) %>%
  select(song, ave.polarity) %>%
  head(n = 5)

## most negative songs by Beyonce
beyonce_scores %>%
  arrange(ave.polarity) %>%
  select(song, ave.polarity) %>%
  head(n = 5)

## Positive Songs
sum(beyonce_scores$ave.polarity > 0)

## negative Songs
sum(beyonce_scores$ave.polarity < 0)

## Positive Songs
sum(beyonce_scores$ave.polarity == 0)

ggplot(beyonce_scores, aes(x = ave.polarity)) +
  geom_histogram() +
  labs(x = "Polarity", "Polarity distribution for Beyonce's 50 songs")

## Comparing Bob Dylan and David Bowie
df_n <- lyrics[lyrics$artist %in% c('david-bowie', 'bob-dylan'), ]
df_n_sent <- polarity(text.var = df_n$lyrics, grouping.var = df_n$artist)
df_n_scores <- scores(df_n_sent)
df_n_scores











