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