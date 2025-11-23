
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords) 
library(tibble)


tweets<- read.csv("USAirlinesTweets.csv")

#putting text into tibble format 
tweets <- as_tibble(tweets) %>% 
  mutate(document = row_number())

#creating tokens
tidy_data <- tweets %>%
  unnest_tokens(word, tweet) %>%
  group_by(word) %>%
  filter(n() > 10 & nchar(word)>3) %>%
  ungroup()

#identify and removing stopwords
stopword <- as_tibble(stopwords::stopwords("en")) 
stopword <- rename(stopword, word=value)
tb <- anti_join(tidy_data, stopword, by="word")

#plotting word cloud

library(ggplot2)
library(wordcloud)
tb %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50, colors=brewer.pal(8, "Dark2")))


#############################
# top 50 words by sentiment #
#############################

# merging words
tb$word[tb$word=="fleet's"] <- "fleet"
tb$word[tb$word=="flightled"] <- "flight"
tb$word[tb$word=="flights"] <- "flight"
tb$word[tb$word=="hours"] <- "hour"
tb$word[tb$word=="thanks"] <- "thank"
tb$word[tb$word=="fleet.s"] <- "fleet"
# removing fake words
tb<- tb[-which(tb$word %in% c('t.co', 'http')),]

neg<- tb[which(tb$sentiment=="negative"),] 
neu<- tb[which(tb$sentiment=="neutral"),] 
pos<- tb[which(tb$sentiment=="positive"),] 

neg_words<- neg%>% count(word, sort = TRUE)
neu_words<- neu%>% count(word, sort = TRUE)
pos_words<- pos%>% count(word, sort = TRUE)
words <- tb %>% count(word, sort = TRUE)

neg_words<- as.list(neg_words[1:20,1])
neu_words<- as.list(neu_words[1:20,1])
pos_words<- as.list(pos_words[1:20,1])
words <- as.list(words[1:100,1])

word_list<- c(neg_words$word, neu_words$word, pos_words$word)
#word_list <- c(words$word);
word_list<- word_list[!duplicated(word_list)]


#########################
#  regression modeling  #
#########################

#creating indicator variables for most frequent words

word_ind<- tb$word %in% word_list
tb<- tb[,c(1,4)]

clean_data<- cbind(tb, word_ind)
colnames(clean_data) <- c('sentiment','word','ind')
clean_data<- clean_data[which(clean_data$ind==TRUE),][,-3]

ind_vars <- fastDummies::dummy_cols(clean_data$word)
clean_data <- data.frame(cbind(clean_data$sentiment, ind_vars))

clean_data$sentiment<- ifelse(clean_data[,1]=="negative",0,
      ifelse(clean_data[,1]=="neutral",1,2))

clean_data<- clean_data[,-c(1,2)]
clean_data<- clean_data[,-36]

#fitting cumulative logit regression
library(ordinal)
summary(fitted.model<- clm(as.factor(sentiment) ~ ., data=clean_data, link="logit"))




