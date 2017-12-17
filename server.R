
library(dplyr)
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(lubridate)
library(ggplot2)
library(plotly)
Sys.setlocale('LC_ALL','C')
tweets <- read.csv("twitter.df.csv", sep=',')
tweets <- tweets%>%
  select(-X, -favorited,created,-truncated, -replyToSID,-replyToUID,-isRetweet, -retweeted, -screenName, -latitude, -longitude)

sentiment.score <- readRDS("tweet.sentiment.RDS")


shinyServer <- function(input, output,session){
  tweets2 = tweets[sample(nrow(tweets), 1000), ]
  output$mytable <- DT::renderDataTable({
    DT::datatable(tweets2[, input$show_vars, drop = FALSE])
  })#renderTable
  
  
  v <- readLines(con = "twitter.kevin.spacey.csv", encoding = 'UTF-8')
  tw.text <- v
  tw.text <- sapply(tw.text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  tw.text <- tolower(tw.text)
  tw.text <- removeWords(tw.text,c("<",">"))
  tw.text <- removeNumbers(tw.text)
  tw.text <- removeWords(tw.text,c(stopwords('en'),'rt','kevin','spacey','kevinspacey','amp'))
  tw.text <- removePunctuation(tw.text,TRUE)
  tw.text <- unlist(strsplit(tw.text,' '))
  tw.text <- gsub("@\\w+ *", "", tw.text)
  tw.text <- gsub("#\\w+ *", "", tw.text)
  tw.text <- gsub("https://t.co/\\w+ *", "", tw.text)
  word.1 <- sort(tw.text,TRUE)
  
  
  output$wordcloud <- renderPlot({
    
    wordcloud(word.1,random.color=TRUE,
              min.freq = input$freq, max.words=input$max,
              colors=brewer.pal(8, "Dark2"))#wordcloud
  })#renderPlot
  
  w <- readLines(con = "sentiment.csv", encoding = 'Latin-1')
  sentiment <- w
  sentiment <- removeNumbers(sentiment)
  sentiment <- removePunctuation(sentiment,TRUE)
  sentiment <- unlist(strsplit(sentiment,' '))
  word.2 <- sort(sentiment,TRUE)
  
  
  output$sentiment.wordcloud <- renderPlot({
  
    wordcloud(word.2, scale=c(4,0.5),random.color=TRUE,
              min.freq = input$freq.2, max.words=input$max.2,
              colors=brewer.pal(8, "Dark2"))
              
    
  })#renderplot
  
  output$scoreplot <- renderPlot({
    
    tweet.sentiment <- tweet.sentiment[!duplicated(tweet.sentiment$word), ]
    
    selectedData <- reactive({
      if ( "positive" %in% input$selectbar) return(subset(tweet.sentiment,tweet.sentiment$score>0&tweet.sentiment$n>10))
      if ( "negative" %in% input$selectbar) return(subset(tweet.sentiment,tweet.sentiment$score<0&tweet.sentiment$n>10))
    })
      
    
    ggplot(selectedData())+
      geom_col(aes(x=reorder(word,n),y=n,fill=desc(n)))+
      coord_flip()+
      xlab("word")
    
          
  })
}
