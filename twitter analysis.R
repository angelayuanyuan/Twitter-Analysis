#################################
# set up Twitter Authentication #
#################################

library(base)
library(twitteR)
library(reshape)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(ggraph)
library(widyr)
library(wordcloud)
library(RColorBrewer)
library(stringi)
library(DataCombine)

# authenticate with twitter: get your credentials by creating an app at apps.twitter.com
api_key <- 'QssHCmITUShD2adfeXX3BHlbp'
api_secret <- 'E27wzViEHhHfVHPI9ybZk9ZTxW7J7222lVlBRNQ9RBdztU7wA4'
access_token <- '907775282060386304-8csyPt0eGfJgGjWWWr2bY3rZh018jRq'
access_token_secret <- 'V5nJV1cyHMgd8AccCY9dIEKVtqRL7t7L2zskc2D7NA5mE'
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


#################################
######### Get Tweets!! ##########
#################################

# get tweets that we are interested in
tweets <- searchTwitter('@KevinSpacey-filter:retweets',n= 3000, lang="en")

# transform tweets list into a data frame
tweets.df <- twListToDF(tweets)

# write it into a csv file
write.csv(tweets.df, "twitter.df.csv")
write.csv(tweets.df$text, "twitter.kevin.spacey.csv")

colnames(tweets.df)[2]<- "text"
#################################
######## Text Analysis ##########
#################################

# let's see what's the most used word in these tweets
my_stop_word <- data.frame(word=character(5))
my_stop_word$word <- c("https","kevin","spacey","rt","kevinspacey")

tweets.df$text <- as.character(tweets.df$text)
tweets.df.word <- tweets.df %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)%>%
  anti_join(my_stop_word)%>%
  filter(str_detect(word, "^[a-z']+$"))%>%
  ungroup()
  

# word cloud
word_freq <- tweets.df.word%>%
  count(word,sort=TRUE) 

word_freq%>%
  filter(n >500)%>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(word, n, fill=(desc(n))))+
  geom_col() +
  xlab(NULL) +
  labs(title="Word Frequency")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

word_freq %>%
  with(wordcloud(word, n, max.words = 100,colors=n)) 

# then let's see what's the most used bigram in these tweets

tweets.df.bigrams <- tweets.df %>%
  group_by(id)%>%
  mutate(linenumber = row_number())%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams.freq <- tweets.df.bigrams%>%
  ungroup()%>%
  count(bigram, sort = TRUE)

bigrams.seperated <- bigrams.freq%>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams.filtered <- bigrams.seperated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_word$word) %>%
  filter(!word2 %in% my_stop_word$word)%>%
  filter(str_detect(word1, "^[a-z']+$"))%>%
  filter(str_detect(word2, "^[a-z']+$"))

bigrams.united <- bigrams.filtered %>%
  unite(bigram, word1, word2, sep = " ")

# word cloud
bigrams.united %>%
  with(wordcloud(bigram, n, max.words = 50,colors=brewer.pal(8, "Dark2"),random.order=FALSE,rot.per=0.35)) 


#################################
###### Sentiment Analysis #######
#################################

get_sentiments("afinn")

AFINN <- get_sentiments("afinn") %>%
  dplyr::select(word,score)

twitter.sentiment <- tweets.df.word %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id, word) %>%
  summarize(sentiment = mean(score))

quantile(twitter.sentiment$sentiment,.5)

ggplot(twitter.sentiment)+
  geom_density(aes(sentiment))+
  geom_vline(xintercept = -1, linetype = 2) 


