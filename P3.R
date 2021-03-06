library("streamR")
library("tm")
#library("SnowballC")
#loading oAuth data for twitter
load("my_oauth.Rdata")

#Step 1 : Data Retrieval
#filtering streams based on keywords "love" and "hate"
filterStream("tweets.json", track = c("love", "hate"), timeout = 10, oauth = my_oauth)

#parsing the tweets to a data.frame
tweets<-parseTweets("tweets.json", simplify = TRUE, verbose = TRUE)

#removing the tweets containing both love and hate
lovetweets <- tweets[grep("love", tweets$text, ignore.case = TRUE),]
hatetweets <- tweets[grep("hate", tweets$text, ignore.case = TRUE),]
ctweets<- merge(lovetweets,hatetweets,by=names(lovetweets))
for(i in 1:nrow(ctweets))
{
  lovetweets = lovetweets[!lovetweets$text == ctweets$text[i],]
  hatetweets = hatetweets[!hatetweets$text == ctweets$text[i],]
}
dtweets = merge(lovetweets,hatetweets,by=names(lovetweets),all=TRUE)
rm(tweets)
rm(lovetweets)
rm(hatetweets)
rm(ctweets)
rm(i)
#Step 2 : Data Preprocessing
#Removing on english characters from tweet text
dtweets$text <- sapply(dtweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#creating a vector source
txt<-VectorSource(dtweets$text)
rm(dtweets)

#Creating a corpus for using tm package
txt.corpus<-Corpus(txt)
rm(txt)

#to lower case
txt.corpus<-tm_map(txt.corpus, content_transformer(tolower))

#remove punctuation and numbers
txt.corpus<-tm_map(txt.corpus,removePunctuation)
txt.corpus<-tm_map(txt.corpus,removeNumbers)

#remove common stopwords
txt.corpus<-tm_map(txt.corpus,removeWords, stopwords("english"))

#remove Whitespace
txt.corpus<-tm_map(txt.corpus,stripWhitespace)

#Stemming
library("SnowballC")
#dict.corpus <- txt.corpus
txt.corpus<-tm_map(txt.corpus,stemDocument)
#txt.corpus<-tm_map(txt.corpus, stemCompletion, dictionary=dict.corpus)
detach(package:SnowballC, unload=TRUE)

#Step 3 : Feature Selection
tdm = TermDocumentMatrix(txt.corpus)
dictionary=findFreqTerms(x=tdm,lowfreq=1000,highfreq=Inf)
if(length(dictionary)<12)
  dictionary = c(dictionary, findFreqTerms(x=tdm,lowfreq = 500, highfreq=1000))
if(length(dictionary)<12)
  dictionary = c(dictionary,findFreqTerms(x=tdm,lowfreq=0,highfreq=500))
dictionary = dictionary[1:12]
if(!is.element("love",dictionary))
  dictionary[12] = "love"
if(!is.element("hate",dictionary))
  dictionary[11] = "hate"
#dictionary=c("love","hate","follow","world","tinyurl.com","today","dont","just","gym","pleas","much","like")
tdm = DocumentTermMatrix(txt.corpus,list(dictionary=dictionary))
rm(txt.corpus)

#Step 4 : Modeling and Prediction
require(RMOA)
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
train_data = as.data.frame(inspect(tdm))
#love column determines whether love tweet or not
for(i in 1:nrow(train_data)){
  if(train_data$love[i]==0)
    train_data$love[i] = 0;
  if(train_data$love[i]>0)
    train_data$love[i] = 1;
}
#coverting strings to factor
train_data<-factorise(train_data)
#converting love column to factor
train_data$love <- as.factor(train_data$love)
train_data <- train_data[,!names(train_data) %in% c("hate")]
#changing column names of train_data
j=1
for(i in 1:ncol(train_data)){
  if(colnames(train_data)[i]!="love"){
    colnames(train_data)[i]<-paste("word",j,sep="",collapse=NULL)
    j=j+1
  }
}
#datastream dataframe 
train_datastream<-datastream_dataframe(train_data)
#model
mymodel <- trainMOA(model = hdt,
                    formula = love ~ word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10,
                    data = train_datastream)
#prediction 
scores <- predict(mymodel, newdata=train_data, type="response")
table(scores, train_data$love)
scores <- predict(mymodel, newdata=train_data, type="votes")
head(scores)
