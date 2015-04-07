# P3-CSC591
library(streamR)
library(ROAuth)
library(plyr)
# Text mining packages
library(stringr)
library(tm)
library(SnowballC)
library(devtools)#for RMOA
install.packages("ff")
install.packages("rJava")
library(RMOA)


my_oauth <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret, requestURL=requestURL,accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
filterStream("tweets.json", track = c("Love", "Hate"), timeout = 10,oauth = my_oauth)

oneData<-parseTweets("tweets.json")


fg<-function(a)
{
  if(!((length(grep("love",a,ignore.case=TRUE))>0) & (length(grep("hate",a,ignore.case=TRUE))>0)))
  {
    data.frame(a) 
  }
}
textData1<-ddply(oneData,.(text),fg)
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


# Step 3
# Create term document matrix
s2 <- Corpus(VectorSource(textData2[,1]))
TDM <- TermDocumentMatrix(s2)

# sanity check to see what it looks like
inspect(TDM[1:100,1:10])

# find words used over 10 times
freqTerms <- findFreqTerms(TDM, 10)

# Remove sparse terms. Play with constant to get reasonable number of entries
TDM.common = removeSparseTerms(TDM, 0.99)
dim(TDM.common)

# Find words associated with love or hate, this is just to visualize
findAssocs(TDM.common, "love", 0.1)
findAssocs(TDM.common, "hate", 0.1)

#TODO, use TDM.common to classify tweets
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
hdt

#formula is what we need to update
mymodel <- trainMOA(model = hdt, 
                    formula = love ~ , 
                    data = TDM.common)
