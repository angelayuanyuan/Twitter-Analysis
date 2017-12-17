library(dplyr)
library(tidytext)
library(stringi)
library(stringr)
library(wordcloud)
library(RColorBrewer)

tweets <- read.csv("twitter.df.csv", sep=',')
tweets <- tweets%>%
  select(-X)

get_sentiments("afinn")

AFINN <- get_sentiments("afinn") %>%
  dplyr::select(word,score)

tweets$text <- as.character(tweets$text)
tweet.word <- tweets %>%
  group_by(id)%>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  filter(str_detect(word, "^[a-z']+$"))%>%
  ungroup()

tweet.sentiment <- tweet.word %>%
  inner_join(AFINN, by = "word")%>%
  select(id,word,score)%>%
  group_by(word)%>%
  mutate(n=length(word))

tweet.sentiment <- data.frame(tweet.sentiment)

# positive/negative
tweet.sentiment <- tweet.sentiment[!duplicated(tweet.sentiment$word), ]
tweet.sentiment <- tweet.sentiment%>%
  arrange(n)

ggplot(subset(tweet.sentiment,tweet.sentiment$score>0&tweet.sentiment$n>10))+
  geom_col(aes(x=reorder(word,n),y=n,fill=desc(n)))+
  coord_flip()+
  xlab("word")

ggplot(subset(tweet.sentiment,tweet.sentiment$score<0&tweet.sentiment$n>10))+
  geom_col(aes(x=reorder(word,n),y=n,fill=desc(n)))+
  coord_flip()+
  xlab("word")
