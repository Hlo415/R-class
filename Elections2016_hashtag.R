api_key<-""
api_secret<-""
access_token<-""
access_token_secret<-""

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

install.packages("stringr") #Use this code to install any packages that you don't have already installed i.e. ROAuth, twitterR.
#Open the libraries that you will use

install.packages("RCurl")
install.packages("ROAuth")
install.packages("twitteR")
install.packages("plyr")

library(RCurl)
library(ROAuth)
library(twitteR)
library(stringr)
library(plyr)
setwd("C:/Users/hlo/Desktop/twitteR")

tweets<-searchTwitter("#trump",n =8000)
tweets.text = laply(tweets, function(t)t$getText())
pos = scan("C:/Users/hlo/Desktop/twitteR/positive-words.txt", what="character",comment.char=";")
neg= scan("C:/Users/hlo/Desktop/twitteR/negative-words.txt", what="character",comment.char=";")



score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
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
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



analysis = score.sentiment(tweets.text, pos, neg, .progress="none")
table(analysis$score)
mean(analysis$score)
hist(analysis$score, main ="#Trump Sentiment Score", ylim=c(0,8000), col ="red", xlab="score" )
hist(analysis$score, main ="#Clinton Sentiment Score", ylim=c(0,8000), col ="blue", xlab="score" )
