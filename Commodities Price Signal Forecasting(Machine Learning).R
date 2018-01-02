#load dataset
#-------------------------------------------------------------------------------------------------------------------
setwd( "C:/Users/yuezh/Desktop/R Working Dictionary")
library(twitteR)
library(plyr)
install.packages("RPAuth")
library(ROAuth)
library(ggplot2) 
library(stringr)
library(tidyr)
library(dplyr)

getwd()
tweet_data<-read.csv("tweets_data.csv")
head(tweet_data)
str(tweet_data)
head(corn_price_data)
corn_price_data<-read.csv("corn_price_data.csv")
head(corn_price_data)
str(corn_price_data)
str(tweet_data)

#sentiment analysis, assign score to each row in tweet dataset
#-----------------------------------------------------------------------------------------------------------------------
posText <- read.delim("C:/Users/yuezh/Desktop/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("C:/Users/yuezh/Desktop/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')
class(pos.words)
class(neg.words)
head(neg.words)
head(pos.words)
pos.words[1]
pos.words
class(pos.words)

tweet_data
class(tweet_data)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = (sum(pos.matches) - sum(neg.matches))
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores = score.sentiment(tweet_data$text, pos.words,neg.words , .progress='text')
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score <0)
scores$neutral <- as.numeric(scores$score==0)
scores$polarity <- ifelse(scores$score >0,"positive",ifelse(scores$score < 0,"negative",ifelse(scores$score==0,"Neutral",0)))

head(scores)

head(scores,5)


scores
#merge tweet dataset, sentiment score and corn price dataset, get train_set and test_set
#-----------------------------------------------------------------------------------------------------------------------
length(tweet_data)
str(tweet_data)
length(scores)
str(scores)
head(scores$text)


qplot(factor(polarity), data=scores, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments Analysis")
qplot(factor(score), data=scores, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores")
??seperate
tweet_data_clean <-separate(tweet_data, col = date, into = c("M","d","y"))
tweetdata<-unite(tweet_data_clean,date,M,d,y,sep = "/")
tail(tweetdata)
str(tweetdata)
class(tweet_data)
class(scores)
tweet_with_score<-merge(tweetdata,scores)
tweet_with_score[order(as.Date(tweet_with_score$date,format="%d/%m/%Y")),,drop=FALSE]
head(tweet_with_score)
selectname<-c("date","score")
newd<-tweet_with_score[,names(tweet_with_score)%in%selectname]
newd
newd1<-aggregate(score~date,data=newd,FUN=mean)
newd1
str(newd)
data_set<-merge(newd1,corn_price_data)
hist(data_set$score)
data_set$positive <- as.numeric(data_set$score >0)
data_set$negative <- as.numeric(data_set$score <0)
data_set$neutral <- as.numeric(data_set$score==0)
data_set$polarity <- ifelse(data_set$score >0,"positive",ifelse(data_set$score < 0,"negative",ifelse(data_set$score==0,"Neutral",0)))
data_set[order(as.Date(data_set$date,format="%d/%m/%Y")),,drop=FALSE]
head(data_set)
cor(data_set$settle,data_set$score)
str(data_set)
data_set_train<-data_set[1:1184,]
data_set_test<-data_set[1185:1694,]
str(data_set_train)
str(data_set_test)
tail(data_set)
tail(data_set_test)
tail(data_set_train)
data_set_test[order(as.Date(data_set_test$date,format = "%d/%m/%Y")),,drop=FALSE]
head(data_set_test)
data_set_train[order(as.Date(data_set_train$date,format = "%d/%m/%Y")),,drop=FALSE]
head(data_set_train)

mu_sentiment<-mean(data_set_train$score)
sd_sentiment<-sd(data_set_train$score)
data_set_train$score<-(data_set_train$score-mu_sentiment)/sd_sentiment
mu_settle<-mean(data_set_train$settle)
sd_settle<-sd(data_set_train$settle)
data_set_train$settle<-(data_set_train$settle-mu_settle)/sd_settle

#predict train_set and test_set with linear regression model
#--------------------------------------------------------------------------------------------------------------
lm_data_set_train <- lm(data_set_train$settle ~ data_set_train$score, data = data_set_train)
summary(lm_data_set_train)


lm_pred<-predict(lm_data_set_train,data=data_set_train$settle)
length(lm_pred)
table(data_set_train$settle,lm_pred)
#print out the accuracy
summary(lm_data_set_train)$r.squared
plot(data_set_train$settle~data_set_train$score,data = data_set_train)

install.packages("PRROC")
library(PRROC)

require(PRROC)
bg <- probs[data_set_train$polarity == "positive"]
fg <- probs[data_set_train$polarity == "negative"]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

bg1 <- probs[data_set_test$polarity == "positive"]
fg1 <- probs[data_set_test$polarity == "negative"]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg1, scores.class1 = bg1, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg1, scores.class1 = bg1, curve = T)
plot(pr)


#predict with SVM model
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("e1071")
library(e1071)
#build model – linear kernel and C-classification (soft margin) with default cost (C=1)
svm_model <- svm(data_set_train$settle~data_set_train$polarity, data=data_set_train, method="C-classification", kernel="linear")
svm_model
pred_train <-predict(svm_model,data_set_train)
#print out the accuracy
mean(pred_train==data_set_train$settle)

#predict with Decision tree
#----------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
library(rpart)
tree <- rpart(polarity ~ settle,data_set_train, method = "class")
# Predict the values of the test set: pred
pred1<-predict(tree,data_set_test,type="class")
pred5<-predict(tree,data_set_train,type="class")
all_probs<-predict(tree,data_set_train,type="prob")
# Construct the confusion matrix: conf
conf2<-table(data_set_train$polarity,pred5)
conf<-table(data_set_test$polarity,pred1)
all_probs
head(all_probs)
probs<-all_probs[,3]
install.packages("ROCR")
library(ROCR)
prediction<-prediction(probs,data_set_test$score)
pred3<-performance(pred)
# Print out the accuracy
sum(diag(conf2))/sum(conf2)
sum(diag(conf))/sum(conf)
require(PRROC)
bg3 <- probs[data_set_train$polarity == "positive"]
fg3 <- probs[data_set_train$polarity == "negative"]

# ROC Curve    
roc3 <- roc.curve(scores.class0 = fg3, scores.class1 = bg3, curve = T)
plot(roc3)

# PR Curve
pr3 <- pr.curve(scores.class0 = fg3, scores.class1 = bg3, curve = T)
plot(pr3)

bg4 <- probs[data_set_test$polarity == "positive"]
fg4 <- probs[data_set_test$polarity == "negative"]

