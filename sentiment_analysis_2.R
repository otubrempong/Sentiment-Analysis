install.packages("rtweet")
library(rtweet)
library(dplyr)

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

consumerKey <- "1qwLzzk3JDTfLRIMqxeY9x7Go"
consumerSecret <- "sImJEEDk1ZhTILLREGuR2XxtmhQPReGanqmvsDVCwbWQVGDf8Q"
accessToken <- "991926225831710720-aGCSFzUG6ZX4sPd0iYxv8ZHHlgaVZMC"
accessTokenSecret <- "bF270yIjqss8okelTuksGZREBzeQrXojjqBPZaxVzyW0H"

token <- create_token(
  app = "otubrempong",
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accessToken,
  access_secret = accessTokenSecret
)

tweet1 <- get_timeline(
  "@Knust",
  n = 100,
  token = token
)


tweet2 <- search_tweets(
  "@Legon",
  n = 100,
  token = auth
)
tweet_df1 <- as_tibble(tweet1)
tweet_df2 <- as_tibble(tweet2)

bscore <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress='text')
rscore <- score.sentiment(tweet2_df$text,pos.words,neg,words,.progress = 'text')
hist(rscore$score)
hist(bscore$score)
