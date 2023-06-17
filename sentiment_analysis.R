library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
library(plyr)
library(stringr)
##SENTIMENT FUNCTION
##NEW
score.sentiment <- function(sentences,pos.words,neg.words, .progress= 'none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentences,pos.words,ng.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words,pos.words)
    neg.matches <- match(words,neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores,text=sentences)
  return(scores.df)
}
##NEW

pos.words = scan("C:/Users/OWUSU/Desktop/ML n DL/kaggle/positive-words.txt",what = 'character',comment.char = ';')
neg.words = scan("C:/Users/OWUSU/Desktop/ML n DL/kaggle/negative-words.txt",what = 'character',comment.char = ';')
bscore <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress='text')
rscore <- score.sentiment(tweet2_df$text,pos.words,neg,words,.progress = 'text')
hist(rscore$score)
hist(bscore$score)
consumerKey <- "nhi3Yij54xVGCSxlJjI4zobJp"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerSecret <- "RaE0wPQtXWoQOZmq7GRdItKDajeJ5m6bOn2LJSn9bnalw1IsM3"
accessToken <- "991926225831710720-V258dWbJT562HdGn8PRGmMwYpmwOyr6c"
accessTokenSecret <- "yWU5APRLKWvmov3TACEumKiOIBW9QMDIBfhr3wGLZMBWP"
twiCred <- OAuthFactory$new(consumerKey = consumerKey,
                            consumerSecret = consumerSecret,
                            requestURL = reqURL,
                            accessURL = accessURL,
                            authURL= authURL)
twiCred$handshake()
  setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
  tweet1 <- userTimeline("@Sarkodie",n=100)
  tweet2 <- userTimeline("@Messi",n=100)
  tweet1_df <- tbl_df(map2_df(tweet1,as.data.frame))
  tweet2_df <- tbl_df(map2_df(twweet2,as.data.frame))
