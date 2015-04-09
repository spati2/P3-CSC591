library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "7SwWq9vOqhaHXj9UHFQGS0tdu"
consumerSecret <- "J8nCdLDxksEvsyVL6F5oIb05jCM0Pvkqoi17HdsTgMeZsGhJcy"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)
save(my_oauth, file = "my_oauth.Rdata")