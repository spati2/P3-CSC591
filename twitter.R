# P3-CSC591
library(streamR)
library(ROAuth)
library(plyr)
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret, requestURL=requestURL,accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
filterStream("tweets.json", track = c("Love", "Hate"), timeout = 10,oauth = my_oauth)

oneData<-parseTweets("tweets.json")

textData1<-ddply(oneData,.(text),fg)
fg<-function(a)
{
  if(!((length(grep("love",a,ignore.case=TRUE))>0) & (length(grep("hate",a,ignore.case=TRUE))>0)))
  {
    data.frame(a) 
  }
}

textData<-as.data.frame(textData1[,1])
View(textData)

textData2=textData
textData2[,1] <- as.data.frame(str_replace_all(textData2[,1],"[^[:graph:]]", " "))
#to lower
s = Corpus(VectorSource(textData2[,1]))
s<-tm_map(s,tolower)
s<-data.frame(text=unlist(sapply(s,'[')),stringAsFactors=F)
textData2[,1]=s[,1]
##remove numbers
textData2[,1]= removeNumbers(as.character(textData2[,1]))
##remove puncs
textData2[,1] = removePunctuation(textData2[,1],preserve_intra_word_dashes=TRUE)
##remove stopwords
textData2[,1]<-removeWords(textData2[,1],stopwords("english"))
##stem completion
textData2[,1]<-stemDocument(textData2[,1])
