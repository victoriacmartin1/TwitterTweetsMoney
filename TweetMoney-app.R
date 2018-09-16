library(shiny)
library(plotly)

# ---
# title: "TwitterTweetsMoney"
# author: "Victoria Martin"
# output: word_document
# date: December 13, 2017
# ---
  
#This project seeks to determine if twitter data can be used to accurately predict how much money a congressional candidate can raise. 


#setwd('Users/victoriamartin/Desktop/Grad/Fall 17/BZAN 538 - Text Mining/Final Project')
#Let's pull in the relevant information
#The current candidates and their information
library(tm)
library(XML)
library(xml2)
library(rvest)
library(stringr)

url0 <-
  read_html('https://ballotpedia.org/List_of_current_members_of_the_U.S._Congress')

repName <- url0 %>%
  html_nodes('td:nth-child(1)') %>%
  html_text()

yearsServed <- url0 %>%
  html_nodes('td:nth-child(2)') %>%
  html_text()

party <- url0 %>%
  html_nodes('td:nth-child(3)') %>%
  html_text()

state <- url0 %>%
  html_nodes('td:nth-child(4)') %>%
  html_text()

#Only the first 100 entries are valid. The rest are white space or extra info (reps)

party <- party[1:100]
state <- state[1:100]
repName <- repName[1:100]
yearsServed <- yearsServed[1:100]

#need to fix this one
index <- which(repName == "[[Elizabeth Warren|]]")
party[index] <- " Democratic\n"
repName[index] <- "Elizabeth Warren"
yearsServed[index] <- 4
state[index] <- "Massachusetts\n"


politDeets <- data.frame(repName,state,party,yearsServed)

twitterURL <-  read_html('https://www.socialseer.com/resources/us-senator-twitter-accounts/')

senators <- twitterURL %>%
  html_nodes('td:nth-child(2)') %>%
  html_text()

handles <- twitterURL %>%
  html_nodes('td:nth-child(3)') %>%
  html_text()

#this is wrong and includes an extra senator from georgia. I remove it after.
senators <- senators[2:102]
handles <- handles[2:102]
senators <- senators[!senators %in% "Saxby Chambliss"]
handles <- handles[!handles %in% "@SaxbyChambliss"]
twitterDeets <- data.frame(senators,handles)

# Now let's grab the fundraiser data for the last six cycles. 

fundsURL <-  read_html('https://www.opensecrets.org/overview/topraise.php?cycle=2016&display=A&view=topraise&memb=S&sort=S')

senators2 <- fundsURL %>%
  html_nodes('td:nth-child(2)') %>%
  html_text()

fundsRaised <- fundsURL %>%
  html_nodes('.number') %>%
  html_text()

#remove special characters from the dollar amount
fundsRaised <- gsub('[[:punct:]]', '',fundsRaised)

fundsURL <-  read_html('https://www.opensecrets.org/overview/topraise.php?cycle=2016&display=A&type=A2&view=btmraise')

senators3 <- fundsURL %>%
  html_nodes('td:nth-child(2)') %>%
  html_text()

fundsRaised2 <- fundsURL %>%
  html_nodes('.number') %>%
  html_text()

#remove special characters from the dollar amount
fundsRaised2 <- gsub('[[:punct:]]', '',fundsRaised2)

senators4 <- c(senators2,senators3)
totfundsRaised <- c(fundsRaised,fundsRaised2)

fundDeets <- data.frame(senators4,totfundsRaised)


#Now let's sort the data frames according to senator name and join them.
politDeets <- politDeets[order(politDeets$repName),]
twitterDeets <- twitterDeets[order(twitterDeets$senators),]
fundDeets <- fundDeets[order(fundDeets$senators4),]


basicData <- cbind(politDeets,twitterDeets,fundDeets)
basicData$senators4 <- NULL
basicData$repName <- NULL

#Now we have an aggregate of all of the data we want. 


#Jeffrey Breen Sentiment Matching Code with positive and negative dictionary read in. 

# Download and extract dictionary files from here:
# http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar
pos.words <- scan('positive-words.txt', what='character', comment.char=';')
neg.words <- scan('negative-words.txt', what='character', comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence =  gsub("[^0-9A-Za-z///' ]","'", sentence,
                     ignore.case = TRUE) #remove special characters
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  #just want the last score
  scores.df = c(score=scores)
  return(scores.df)
  
}





#Now for each senator we need to be able to pull information from their account
#We will do this using the twitter API and their handles
library(rvest)
library(stringr)
library(twitteR)
library(ROAuth)
library(httr)

key = 'OPCv1dpXBAWnEz4QoSPWq2HSG'
secret = 'haoIdlMfG1XhRwkInRjYy1efC7SgIFrJV6GnB885eYWYYK0ayu'
token = '909960169945272320-nrf7IeyXprJfyjMznh5GcWDAvO9GDid'
tokensecret = 'vlX0XazZ1zpGReOmJfOqjhnUGu4bSD0KVBlezbjKZlTLh'

options(httr_oauth_cache=F)
CONSUMER_KEY = key
CONSUMER_SECRET = secret
OAUTH_TOKEN = token
OAUTH_TOKEN_SECRET = tokensecret
setup_twitter_oauth(CONSUMER_KEY,
                    CONSUMER_SECRET,
                    OAUTH_TOKEN,
                    OAUTH_TOKEN_SECRET)


#Collect tweets for each senator and compute sentiment score as well as number of unique words
for (senCount in 1:nrow(basicData))
{
  handleVal <- basicData$handles[senCount]
  extractTweets <- try(userTimeline(handleVal,
                                    n=10))
  
  # Loop over tweets and extract text
  tweetText = try(laply(extractTweets, function(t)
    t$getText()))
  #concatenate all of the individual vector entries into one string. 
  tweets <- NULL
  tweets <- try(paste(tweetText, sep="", collapse=""))
  basicData$sentScore[senCount]  = try(sum(score.sentiment(tweets,  pos.words, neg.words)))
  basicData$numUniqueWords[senCount] =
    try(length(unique(unlist(str_split(tweets,' ')))))
}


#convert to correct type for each numeric variable
basicData$yearsServed <- as.numeric(as.character(basicData$yearsServed))
basicData$totfundsRaised <- as.numeric(as.character(basicData$totfundsRaised))



#Now let's get to the prediction

fit <- lm(totfundsRaised ~ state + party + yearsServed, data=basicData)
sm <- summary(fit) # show results
sum(sm$residuals^2)

fit <- lm(totfundsRaised ~ state + party + yearsServed +sentScore + numUniqueWords, data=basicData)
sm <- summary(fit) # show results
sum(sm$residuals^2)


#basic summary stats
dems <- subset(basicData, basicData$party == " Democratic\n")
reps <- subset(basicData, basicData$party == " Republican\n")
inds <- subset(basicData, basicData$party == " Independent\n")

#now let's get info on a state and party level
state.score<- aggregate(basicData[c("sentScore")],
                        by = basicData[c("state")], FUN=median)

state.funds<- aggregate(basicData[c("totfundsRaised")],
                        by = basicData[c("state")], FUN=median)

state.words<- aggregate(basicData[c("numUniqueWords")],
                        by = basicData[c("state")], FUN=median)






ui <- fluidPage(
  titlePanel("Maps of Senatorial Information"),
  
  
  
  mainPanel(
        tabsetPanel(type = "tabs",
                tabPanel("Funds Raised", plotlyOutput("plot1"),verbatimTextOutput("summary1")),
                tabPanel("Sentiment Score", plotlyOutput("plot2"),verbatimTextOutput("summary2")),
                tabPanel("Unique Words", plotlyOutput("plot3"),verbatimTextOutput("summary3"))
    )
  ),
  
  
  
  
  
  
  plotlyOutput("plot"),
  
  verbatimTextOutput("click")
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
  plot_ly(z = state.funds$totfundsRaised, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$plot2 <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = state.score$sentScore, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$plot3 <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = state.words$numUniqueWords, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view data" else d
  })
  
  output$summary1 <- renderPrint({
    summary(state.funds$totfundsRaised)
  })
  output$summary2 <- renderPrint({
    summary(state.score$sentScore)
  })
  output$summary3 <- renderPrint({
    
    summary(state.words$numUniqueWords)
  })
  
}

shinyApp(ui, server)

