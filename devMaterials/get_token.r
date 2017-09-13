library(httr)
uName <- "cmeier@BattelleEcology.org"
pw <- "b7TGbGE$g8mudbyP@YqN"

req <- GET("https://api.fulcrumapp.com/api/v2/users.json", 
           authenticate(uName, pw, type = "basic"))

# Extract user tokens
if(length(content(req)$user$contexts) > 0){
  api_token <- ""
  for (i in 1:length(content(req)$user$contexts)){
    api_token <- c(api_token, content(req)$user$contexts[[i]]$api_token)
  }
  api_token <- api_token[nchar(api_token) > 0]
}


# Token Cody F. generated that has admin privileges
api_token <- "3ab235047ec293b27f06f6819e81b291435f9c61282345ff1de9624f744034b4233a6fcd1b87c3c2"
