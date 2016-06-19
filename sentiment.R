#This code is used to find the tweets, pre- processing of data, predicting sentiment.
#Model implementation, accuracy prediction, ROC curve generation.
library(twitteR)
library(RCurl)
library(RJSONIO)
library(plyr)
library(lattice)
library(stringr)
library(caTools)
library(tm)
library(SnowballC)
library(rpart.plot)
library(ROCR)
library(randomForest)

#Steps to be followed for twitter authentication
#the keys need to be changed as this data is confidential
consumer_key <- '**********************'
consumer_secret <- '*******************************'
access_token <- '************************************'
access_secret <- '*************************************'
setup_twitter_oauth(consumer_key , consumer_secret, access_token, access_secret)

#Cleaning the text (pre-processing steps)
text_cleaning <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)

#It is important to convert all the text into lower case  
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "er"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
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
 
  
  #data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  sentiment = js$output$result
  return(list(sentiment=sentiment))
}
#Searching tweets for people , including number of tweets and langauge of tweets
Hillary = searchTwitter("hillary + clinton", n=1500, lang='en')
Donald = searchTwitter("donald + trump", n=1500, lang='en')
text_hc <- sapply(Hillary, function(x) x$getText()) #getting the text
text_dt <- sapply(Donald, function(x) x$getText())
NumTweets <- c(length(text_hc), length(text_dt))
tweets <- c(text_hc, text_dt) #combining tweets for all candidates
tweets <- sapply(tweets,function(x) iconv(x, "latin1", "ASCII", sub="")) #remove non alphanumeric char
head(tweets)

#lexicon based sentiment analysis approach
neg <- readLines("/Users/sachitmahajan/Documents/negative-words.txt")
pos <- readLines("/Users/sachitmahajan/Documents/positive-words.txt")
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none'){
  
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words){
                    
                    #removing punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    
                    #removing control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    
                    #removing digits
                    sentence <- gsub('\\d+', '', sentence)
                    tryTolower <- function(x){
                      
                    
                      y <- NA #for missing values
                      try_error <- tryCatch(tolower(x), error=function(e) e)
                      if (!inherits(try_error, "error"))
                        y <- tolower(x)
                      return(y)
                    }
                    
                    sentence <- sapply(sentence, tryTolower)
                    
                    word.list <- str_split(sentence, "\\s+") #generate word list by splitting the sentence
                    words <- unlist(word.list)
                    
                   
                    #comparison with positive and negative terms
                    positivematch <- match(words, pos.words)
                    negativematch <- match(words, neg.words)
                    positivematch <- !is.na(positivematch)
                    negativematch <- !is.na(negativematch)
                    
                    #score of sentiment
                    score <- sum(positivematch) - sum(negativematch)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  
  #convert into df
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores <- score.sentiment(tweets, pos, neg, .progress='text')
#view(scores)
scores$Candidate = factor(rep(c("Hillary", "Donald"), NumTweets))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)
g_score = paste0(round(100 * numpos / (numpos + numneg)),"%")
#g_score

# box plot and histogram for visualization
boxplot(score~Candidate, data=scores, col='black')
histogram(data=scores, ~score|Candidate, main="Twitter Sentiment Analysis Presidential candidates", xlab="", sub="Sentiment Score")

corpus <- Corpus(VectorSource(tweets))

tweetsCorpus <- tm_map(corpus, removePunctuation)
tweetsCorpus <- tm_map(tweetsCorpus, tolower)
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, c("trump", "donald", "hillary", "clinton", stopwords("english")))
tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)

#transform to text which wordcloud can use
tweet_dtm <- tm_map(tweetsCorpus, PlainTextDocument)

terms <- DocumentTermMatrix(tweet_dtm)#document matrix generation
#terms

length(findFreqTerms(terms, lowfreq=20)) #find the words that appears at least 20 times

#remove the less frequent terms
sparseTerms <- removeSparseTerms(terms, 0.995)

df <- as.data.frame(as.matrix(sparseTerms))
colnames(df) <- make.names(colnames(df))
df$Negative <- as.factor(scores$score <=-1)
df$score <- NULL
df$Score <-NULL
set.seed(1000)

#splitting into training an testing
split <- sample.split(df$Negative, SplitRatio=0.8)
trainData <- subset(df, split==TRUE)
testData <- subset(df, split==FALSE)

cart <- rpart(Negative ~., data=trainData, method="class")
prp(cart)

predictCART <- predict(cart, newdata = testData, type="class") #prediction using CART
table(testData$Negative, predictCART)


accu <- (465+67)/sum(table(testData$Negative, predictCART)) 
round(accu,3)

roc <- predict(cart, newdata = testData) #for roc curve
pred <- prediction(roc[,2], testData$Negative)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
performance(pred, "auc")@y.values

#comparing the accuracy using random forest model
mymodel <- randomForest(Negative ~ ., data = trainData, nodesize = 25, ntrees = 200)
predict_mymodel <- predict(mymodel, newdata = testData)
table(testData$Negative, predict_mymodel)

#calculating accuracy #the formula needs to be changed according to the values we get above
accu <- (452+83)/sum(table(testData$Negative, predict_mymodel))
round(accu,3)

#############################
