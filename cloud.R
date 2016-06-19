#this part of the code is used to generate word cloud with sentiment percentage

library(twitteR)
library(RCurl)
library(wordcloud)
library(RJSONIO)
library(stringr)
library(tm)

#Steps to be followed for twitter authentication
#the keys need to be changed as this data is confidential
consumer_key <- '**************'
consumer_secret <- '**************************************'
access_token <- '***************************************'
access_secret <- '*************************************'
setup_twitter_oauth(consumer_key , consumer_secret, access_token, access_secret)

#Cleaning the text (pre-processing steps)
text_cleaning <- function(text_gv)
{
  text_gv = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text_gv)
  text_gv = gsub("@\\w+", "", text_gv)
  text_gv = gsub("[[:punct:]]", "", text_gv)
  text_gv = gsub("[[:digit:]]", "", text_gv)
  text_gv = gsub("http\\w+", "", text_gv)
  text_gv = gsub("[ \t]{2,}", "", text_gv)
  text_gv = gsub("^\\s+|\\s+$", "", text_gv)
  text_gv = gsub("amp", "", text_gv)
  
  #It is impostant to convert all the text into lower case 
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  text_gv = sapply(text_gv, try.tolower)
  text_gv = text_gv[text_gv != ""]
  names(text_gv) = NULL
  return(text_gv)
}

#Sentiment estimation of the tweets
getSentiment <- function (text, key){
  
  text <- URLencode(text);
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  
  #using datumbox api for sentiment analysis  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  sentiment = js$output$result
  return(list(sentiment=sentiment))
}

#Searching tweets for people , including number of tweets and langauge of tweets
tweets = searchTwitter("@HillaryClinton", 100, lang="en") 
# getting the text
tweet_txt = sapply(tweets, function(x) x$getText())

# cleaning the text
tweet_txt = sapply(tweets, function(x) x$getText())
tweet_clean = text_cleaning(tweet_txt)
tweet_num = length(tweet_clean)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)


db_key= "**********************" #key to access datumbox api, this needs to be changed

sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], db_key)
  
  tweet_df$sentiment[i] = tmp$sentiment
  
  print(paste(i," of ", tweet_num))
  
}

#sentimnets attached with different words
sent_words = levels(factor(tweet_df$sentiment))

#generate pecentage of different sentiments
labels <- lapply(sent_words, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"))
us_sent = length(sent_words)
words_tx = rep("", us_sent)
for (i in 1:us_sent)
{
  tmp = tweet_df[tweet_df$sentiment == sent_words[i],]$text
  
  words_tx[i] = paste(tmp,collapse=" ")
}
#remove stopwords

words_tx = removeWords(words_tx, stopwords("english"))
corpus = Corpus(VectorSource(words_tx))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

#remove the rows with no sentiments
tweet_df <- tweet_df[tweet_df$sentiment!="",]

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(6, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.0)



######################################
