##################################################################
# Authors: 
# Joseph Decker - jdecker
# Sadhana Kannan - sadhan
# Sattwik Pati - spati2
# Shilpa Mairpady - smairpa
# Prathyusha Vadlamudi - pvadlam

# Description :
# Twitter tweets "love/hate" sentiment classifier in real-time
##################################################################

#function to load and install required packages
packages<-function()
{
#   install.packages("streamR")
#   install.packages("ROAuth")
#   install.packages("plyr")
#   install.packages("stringr")
#   install.packages("tm")
#   install.packages("SnowballC")
#   install.packages("devtools")
#   install.packages("RMOA")
#   install.packages("ff")
#   install.packages("rJava")
  
  require(streamR)
  require(ROAuth)
  require(plyr)
  require(stringr)
  require(tm)
  require(SnowballC)
  require(devtools)
  require(RMOA)
  require(ff)
  require(rJava)
}

#function to remove tweets having both the words love and hate
rmTweetWithBothLoveHate<-function(a)
{
  # check if text doesn't have both love and hate in it
  if(!((length(grep("love",a,ignore.case=TRUE))>0) & (length(grep("hate",a,ignore.case=TRUE))>0)))
  {
    # return it as data frame
    data.frame(a) 
  }
}

#function to remove extra white spaces
modifiedStripWhiteSpace<-function(a)
{
  if((a!="")&(a!="."))
  {
    a[,1]=stripWhitespace(a[,1])
    data.frame(a) 
  }
}

# function to retrieve tweets and parse them
retrieveTwitterData<-function(consumerKey,consumerSecret)
{
#   requestURL <- "https://api.twitter.com/oauth/request_token"
#   accessURL <- "https://api.twitter.com/oauth/access_token"
#   authURL <- "https://api.twitter.com/oauth/authorize"
#   my_oauth <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret, requestURL=requestURL,accessURL=accessURL, authURL=authURL)
#   my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  
  load("my_oauth.Rdata")
  
  #check if file "tweets.json" exists and if yes delete it
  if(file.exists("tweets.json")){
    file.remove("tweets.json")
  }  
  
  # read tweets with words love, hate in them and save to file "tweets.json"
  filterStream("tweets.json", track = c("Love", "Hate"), timeout = 40,oauth = my_oauth)
  
  #parse tweets into a dataframe
  oneData<-parseTweets("tweets.json")
  #view data in new tab
  View(oneData)
  #clean the tweets text for processing further
  txt.corpus=cleanTweets(oneData)
}

#function to remove non-english and non-UTF-8 characters from tweet text
cleanTweets<-function(oneData)
{
  #Removing non english characters from tweet text
  oneData[,1] <- as.data.frame(str_replace_all(oneData[,1],"[^[:graph:]]", " "))
  
  #Converting tweet encodings from ascii/latin/iso to utf-8 with unchangeables set to ""
  oneData$text <- sapply(oneData$text,function(row){ 
    iconv(row, "ISO_8859-2", "ASCII", sub="")
    iconv(row, "latin1", "ASCII", sub="")
    iconv(row, "LATIN2", "ASCII", sub="")
    })
  
  # remove tweets with both love and hate in them
  oneData<-ddply(oneData,.(text),rmTweetWithBothLoveHate)
  dataPreProcess(oneData)
}

#function to pre-process tweet text
dataPreProcess<-function(oneData)
{  
  #retrieve only text column
  tweetText<-oneData$text

  #create corpus
  txt.corpus <- Corpus(VectorSource(tweetText),readerControl=list(language="en"))
  #convert letters to lower case
  txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
  #txt.corpus <- tm_map(txt.corpus, tolower)
  #remove numeric characters
  txt.corpus <- tm_map(txt.corpus, removeNumbers)
  # remove common english stop words and common words like rt,http,retweet
  txt.corpus <- tm_map(txt.corpus, removeWords, c(stopwords("english"),"rt","http","retweet"))
  #remove punctuations
  txt.corpus <- tm_map(txt.corpus, removePunctuation)  
  # get root/stem words for words present
  txt.corpus <- tm_map(txt.corpus, stemDocument, language="english")
  # converting to plain text document
  
  # remove extra white spaces
  txt.corpus<-tm_map(txt.corpus,stripWhitespace)
  
  return(txt.corpus)
}

#function to perform feature Construction and Selection
featureConstructionSelection<-function(txt.corpus){

  ###### TRAINING
  
  #create a term document matrix from tweet text corpus
  tdm = TermDocumentMatrix(txt.corpus)
  #create matrix with terms and total count of word occurrence
  freqMat <- data.frame(apply(tdm, 1, sum))
  #rename columns as term and freq
  freqMat <- data.frame(term = row.names(freqMat), freq = freqMat[, 1])
  #sort matrix by freq
  freqMat <- freqMat[order(freqMat$freq, decreasing = T), ]
  #remove row names
  row.names(freqMat) <- NULL
  #view most frequent words
  View(freqMat)
  #input and output index
  inIndex=1
  outIndex=1
  # number of features k selected as 10% of total terms in document
  k=floor(dim(freqMat)[1]*10/100)
  #stores selected features
  dictionary=NULL
  
  while(outIndex<k){
    #skip words that have love in them
    if(length(grep("love",freqMat[inIndex,1],ignore.case=TRUE))>0){
      inIndex=inIndex+1;
    }
    #skip words that have hate in them
    else if(length(grep("hate",freqMat[inIndex,1],ignore.case=TRUE))>0){
      inIndex=inIndex+1;
    }
    #add word to dictionary
    else{
      dictionary=c(dictionary,as.character(freqMat[inIndex,1]))
      inIndex=inIndex+1;
      outIndex=outIndex+1;
    }
  }
  #add words love and hate to dictionary
  dictionary=c(dictionary,"love","hate")
  
  #create DocumentTermMatrix with only chosen feature terms
  tdm = DocumentTermMatrix(txt.corpus,list(dictionary=dictionary))
  
  #set type of classification tree as Hoeffding tree
  hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
  #convert tdm to dataframe
  train_data = as.data.frame(inspect(tdm))
  
  ###apply(matrix(train_data$love),2,sum)
  
  #love column determines whether love tweet or not
  for(i in 1:nrow(train_data)){
    if(train_data$love[i]==0){
      #hate tweet
      train_data$love[i] = 0
    }       
    else {
      #love tweet
      train_data$love[i] = 1
    }       
  }  
  
  #coverting strings to factor
  train_data<-factorise(train_data)
  #converting love column to factor
  train_data$love <- as.factor(train_data$love)
  #remove hate column from data as love=0 implies tweet has hate
  train_data <- train_data[,!names(train_data) %in% c("hate")]

  #create datastream of dataframe 
  train_datastream<-datastream_dataframe(data=train_data)
  #build model from training data
  mymodel <- trainMOA(model = hdt,
                      formula = love ~ .,
                      data = train_datastream)
  #predict sentiment of tweet using model built
  scores <- predict(mymodel, newdata=train_data, type="response")
  str(scores)
  #print confusion matrix as a sanity check
  print("While training: ")
  table(scores, train_data$love)
  

  ###### TESTING
  
  # retrieve and pre-process testing data
  txt.corpus=retrieveTwitterData(consumerKey,consumerSecret)
  
  #create DocumentTermMatrix with only chosen feature terms
  tdm = DocumentTermMatrix(txt.corpus,list(dictionary=dictionary))

  #convert tdm to dataframe
  test_data = as.data.frame(inspect(tdm))  
  
  #love column determines whether love tweet or not
  for(i in 1:nrow(test_data)){
    if(test_data$love[i]==0){
      #hate tweet
      test_data$love[i] = 0
    }       
    else {
      #love tweet
      test_data$love[i] = 1
    }       
  }  
  
  #coverting strings to factor
  test_data<-factorise(test_data)
  #converting love column to factor
  test_data$love <- as.factor(test_data$love)
  #remove hate column from data as love=0 implies tweet has hate
  test_data <- test_data[,!names(test_data) %in% c("hate")]

  #create datastream of dataframe 
  test_datastream<-datastream_dataframe(data=test_data)
  
  #update model using test data
  mymodel <- trainMOA(model = mymodel$model,
                      formula = love ~ .,
                      data = test_datastream,reset=FALSE)
  
  #predict sentiment of tweet using model built
  scores <- predict(mymodel, newdata=test_data, type="response")
  str(scores)
  #print confusion matrix
  print("While testing: ")
  table(scores, test_data$love)
  
  #total number of tweets in test data
  totalTweets=dim(test_data)[1]
  
  #get true positive(Love) and true negative(Hate) values
  trueLove=table(scores, test_data$love)["1","1"]
  trueHate=table(scores, test_data$love)["0","0"]
  
  #calculate accuracy as (tp+tn)/(tp+tn+fp+fn) where (tp+tn+fp+fn)=total
  Accuracy=(trueLove+trueHate)/totalTweets
  
  #print accuracy
  print(paste("Accuracy is :",Accuracy))
}

# script starting point for running
packages()

# create variables with your keys if my_oauth.Rdata doesnt work
# consumerKey <- "<replace with your twitter consumer key>"
# consumerSecret <- "<replace with your twitter consumer secret key>"

# retrieve and pre-process training data
txt.corpus=retrieveTwitterData(consumerKey,consumerSecret)

#perform feature Construction and Selection, model build and test with new data
sentimentAnalyser(txt.corpus)