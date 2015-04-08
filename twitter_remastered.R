packages<-function()
{
  install.packages("streamR")
  install.packages("ROAuth")
  install.packages("plyr")
  install.packages("stringr")
  install.packages("tm")
  install.packages("SnowballC")
  install.packages("devtools")
  install.packages("RMOA")
  install.packages("ff")
  install.packages("rJava")
  #fucntion to loand and install required packages
  library(streamR)
  library(ROAuth)
  library(plyr)
  library(stringr)
  library(tm)
  library(SnowballC)
  library(devtools)
  library(RMOA)
  library(ff)
  library(rJava)
}

fg<-function(a)
{
  if(!((length(grep("love",a,ignore.case=TRUE))>0) & (length(grep("hate",a,ignore.case=TRUE))>0)))
  {
    data.frame(a) 
  }
}

fg1<-function(a)
{
  if((a!="")&(a!="."))
  {
    a[,1]=stripWhitespace(a[,1])
    data.frame(a) 
  }
}

twitterData<-function(consumerKey,consumerSecret)
{
  my_oauth <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret, requestURL=requestURL,accessURL=accessURL, authURL=authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  filterStream("tweets.json", track = c("Love", "Hate"), timeout = 40,oauth = my_oauth)
  oneData<-parseTweets("tweets.json")
  View(oneData)
  step1(oneData)
}

step1<-function(oneData)
{
  #making love and hate mutually exclusive
  textData1<-ddply(oneData,.(text),fg)
  step2(textData1)
}

step2<-function(textData1)
{
  textData2[,1] <- as.data.frame(str_replace_all(textData2[,1],"[^[:graph:]]", " "))
  tweetText<-textData2$text
  tweetText = iconv(tweetText, "ASCII", "UTF-8", sub="")
  tweetText = iconv(tweetText, "ISO_8859-2", "UTF-8",sub="")
  tweetText = iconv(tweetText, "LATIN2", "UTF-8",sub="")
  s <- Corpus(VectorSource(tweetText),readerControl=list(language="en"))
  s <- tm_map(s, tolower)
  s <- tm_map(s, removePunctuation)
  s <- tm_map(s, removeNumbers)
  s <- tm_map(s, removeNumbers)
  s <- tm_map(s, removeWords, c(stopwords("english"),"rt","http","retweet"))
  s <- tm_map(s, stemDocument, language="english")
  s <- tm_map(s, PlainTextDocument)
  s<-data.frame(text=unlist(sapply(s,'[')),stringAsFactors=F)
  ##remove numbers
  s[,1]= removeNumbers(as.character(s[,1]))
  textData3<-ddply(s,.(text),fg1)
  View(textData3)
  tweetText<-textData3$text
  print(tweetText)
}

packages()
twitterData(consumerKey,consumerSecret)
