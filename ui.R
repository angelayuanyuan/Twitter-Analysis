
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(plotly)

tweets <- read.csv("twitter.df.csv", sep=',')
tweets <- tweets%>%
  select(-X,-favorited,created,-truncated, -replyToSID,-replyToUID,-isRetweet, -retweeted, -screenName, -latitude, -longitude)

sentiment.score <- readRDS("tweet.sentiment.RDS")


shinyUI ( 
  navbarPage("Angela's App",
                      tabPanel("Home",
                               HTML('<center><img src="title.png" width="800"></center>'),
                               HTML('<center><img src="home page.jpg" width="800"></center>'),
                               actionButton(inputId='ab1', label="Learn More", 
                                            icon = icon("th"), 
                                            onclick ="window.open('http://www.cnn.com/2017/10/30/entertainment/kevin-spacey-allegations-anthony-rapp/index.html')")
                      ), #tabPanel.1
                      tabPanel("Tweets",
                           sidebarLayout(
                                sidebarPanel(
                                     checkboxGroupInput("show_vars", "Columns in tweets to show:",
                                                   colnames(tweets), selected = colnames(tweets))
                                ),# sidebarPanel
                                mainPanel(
                                     tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("tweets", DT::dataTableOutput("mytable"))
                                     ) 
                                 ) #mainPanel
                                ) # sidebarLayout
                      ),# tabPanel.2
                                   
                            
                      tabPanel("Word Cloud",
                               sliderInput("freq",
                                           label = "Minimum Frequency:",
                                           min = 50,  max = 100, value = 50),
                               sliderInput("max",
                                           label = "Maximum Number of Words:",
                                           min = 1,  max = 100,  value = 50),
                               textInput(inputId = "Word Cloud",
                                         label = "Word Cloud",
                                         value = "Word Cloud"),
                               plotOutput(outputId = "wordcloud")
                               
                      ),#tabPanel.3
                      navbarMenu("Sentiment Analysis",
                                 tabPanel("Sentiment Word Cloud",
                                          sliderInput("freq.2",
                                                      label = "Minimum Frequency:",
                                                      min = 0,  max = 10, value = 5),
                                          sliderInput("max.2",
                                                      label = "Maximum Number of Words:",
                                                      min = 1,  max = 100,  value = 50),
                                          textInput(inputId = "Sentiment Word Cloud",
                                                    label = "Sentiment Word Cloud",
                                                    value = "Sentiment Word Cloud"),
                                          plotOutput(outputId = "sentiment.wordcloud")
                                          
                                 ),
                                 tabPanel("More",
                                          sidebarLayout(
                                            sidebarPanel(
                                              selectInput("selectbar","Sentiment:",c("Positive"="positive","Negative"="negative"))#selectInput
                                            ),#sidebarPanel
                                            mainPanel(plotOutput("scoreplot"))
                                              
                                            )#sidebarPanel
                                          )#sidebarLayout
                                          
                                 
                      )#navbarMenu
)# navbarPage

)
