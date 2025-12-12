setwd("C:/Users/Jeuryn Robeves/OneDrive/Desktop/wordcloud")
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

text <- readLines("restaurant_feedbacks.txt", encoding = "UTF-8", warn = FALSE)
text <- paste(text, collapse = " ")
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x)))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

df_1 <- subset(df, freq == 1)
least5 <- head(df_1[order(df_1$word), ], 5)

set.seed(1234)
png("wordcloud_rare.png", width = 800, height = 600)
wordcloud(words = least5$word,
          freq = least5$freq,
          min.freq = 1,
          max.words = 5,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

dev.off()

# png("wordcloud_exam.png", width = 800, height = 600)
# wordcloud(words = df$word,
#           freq = df$freq,
#           min.freq = 2,
#           max.words = 1000,
#           random.order = FALSE,
#           rot.per = 0.35,
#           colors = brewer.pal(8, "Dark2"))

top10 <- head(df, 10)
print(top10, row.names = FALSE)




