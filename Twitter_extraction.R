install.packages(c("devtools", "rjson", "bit64", "plyr", "httr", "ggplot2", "doBy", "XML","base64enc"))
install.packages("Quandl")

library(devtools)
install_github("geoffjentry/twitteR")
library(plyr)
library(httr)
library(doBy)
library(Quandl)
library(twitteR)

api_key <- "F4v4IABw1FFJ9iui0vmFGpDLb"
api_secret <- "MQnTCIUqbzB5cXWkUVSdF52UUKqqOjSPds1DDVGlulz4D4lZ7T"
access_token <- "979375479387017218-wV2OWpLypEsFY7tTkFgXvpoXmL4rZD3"
access_token_secret <- "um8sjBsX3Dpf6smJS5wtKVeCJYBu0eIo0G3mVg5W0OrQc"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

pos = scan('C:/Users/Nikhil Kamath/Documents/NCI/Semester 1/Data Warehousing and Business Intelligence/DWBI Project/positive-words.txt', what='character', comment.char=';')
neg = scan('C:/Users/Nikhil Kamath/Documents/NCI/Semester 1/Data Warehousing and Business Intelligence/DWBI Project/negative-words.txt', what='character', comment.char=';')

pos.words = c(pos, 'upgrade')
neg.words = c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

#data cleaning
require(plyr)
require(stringr)
score.sent <- function(sentence, pos.words, neg.words, .progress='none') {
  sentence <- gsub('[[:punct:]]', "", sentence)
  sentence <- gsub('[[:cntrl:]]', "", sentence)
  sentence <- gsub('\\d+', "", sentence)
  sentence <- tolower(sentence)
  word.list <- str_split(sentence, '\\s+')
  words <- unlist(word.list)
  pos.matches <- match(words, pos.words)
  neg.matches <- match(words, neg.words)
  pos.matches <- !is.na(pos.matches)
  neg.matches <- !is.na(neg.matches)
  score <- sum(pos.matches) - sum(neg.matches)
  return(score)
}

score.senti <- function(sentences, pos.words, neg.words, .progress='none') {
  require(plyr)
  require(stringr)
  
  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
    tryCatch(score.sentence(sentence,pos.words,neg.words),error=function(e) 0)  
  }, pos.words, neg.words)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

collect.and.score <- function(handle, code, game_publisher, pos.words, neg.words) {
  
  tweets <- searchTwitter(handle, n=500)
  text = laply(tweets, function(t) t$getText())
  
  score = score.sentiment(text, pos.words, neg.words)
  score$game_publisher = game_publisher
  score$code = code
  
  return (score)
}

nintendo.scores = collect.and.score("@NintendoAmerica", "NT", "Nintendo", pos.words, neg.words)
take_two.scores = collect.and.score("T2","Take-Two Interactive", pos.words, neg.words)
microsoft.scores = collect.and.score("@MSStudiosBlog", "MS","Microsoft Game Studios", pos.words, neg.words)
sony.scores = collect.and.score("@Sony", "SY","Sony Interactive Entertainment", pos.words, neg.words)
electronic_arts.scores = collect.and.score("@EA","EA", "Electronic Arts", pos.words, neg.words)
activision.scores = collect.and.score("@Activision","AV", "Activision", pos.words, neg.words)
sega.scores = collect.and.score("sega", "SE", "Sega", pos.words, neg.words)
warner_bros.scores = collect.and.score("@WarnerBrosEnt", "WB", "Warner Bros. Interactive Entertainment", pos.words, neg.words)
atari.scores = collect.and.score("@Atari", "AT", "Atari", pos.words, neg.words)
konami.scores = collect.and.score("@Konami", "KO", "Konami Digital Entertainment", pos.words, neg.words)
disney.scores = collect.and.score("@DisneyInteract", "DI", "Disney Interactive Studios", pos.words, neg.words)
thq.scores = collect.and.score("@THQNordic", "TN", "THQ games", pos.words, neg.words)
bethesda.scores = collect.and.score("@bethesda", "BD", "Bethesda Softworks", pos.words, neg.words)
bandai.scores = collect.and.score("@BandaiNamcoUK", "BN", "Bandai Namco Entertainment", pos.words, neg.words)
square_enix.scores = collect.and.score("@SquareEnix", "SE", "Square Enix", pos.words, neg.words)
rising_star.scores = collect.and.score("@RisingStarGames", "RS", "Rising Star Games", pos.words, neg.words)
ubisoft.scores = collect.and.score("@Ubisoft", "UB", "Ubisoft", pos.words, neg.words)
xseed.scores = collect.and.score("@XSEEDgames", "XS", "XSEED Games", pos.words, neg.words)
telltale.scores = collect.and.score("@telltalegames", "TT", "Telltale Games", pos.words, neg.words)


all.scores = rbind(nintendo.scores, microsoft.scores, sony.scores, electronic_arts.scores, activision.scores, ubisoft.scores, sega.scores, warner_bros.scores, atari.scores, konami.scores, thq.scores, disney.scores, bethesda.scores, bandai.scores, square_enix.scores, rising_star.scores, xseed.scores, telltale.scores)
all.scores$very.pos = as.numeric(all.scores$score >= 1)
all.scores$very.neg = as.numeric(all.scores$score <= 1)
tweet.df = ddply(all.scores, c('game_publisher','code'), summarise, pos.count = sum(very.pos), neg.count = sum(very.neg))
tweet.df$all.count = twitter.df$pos.count + twitter.df$neg.count
tweet.df$score = round(100 * twitter.df$pos.count / twitter.df$all.count)

write.csv(all.scores,file ='C:/Users/Nikhil Kamath/Documents/NCI/Semester 1/Data Warehousing and Business Intelligence/DWBI Project/twitter_sentiment1.csv',row.names = F)
