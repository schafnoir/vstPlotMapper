# My personal API token, generated via 'get_token.R'
api_token = "6ce6d01c4ec984e68fcecbb04d96f71a229f1db388fe1acd90c1fb6ba77a1daf"

# Defining SQL query to get data from the VST Map & Tag PROD table
vst = paste0(URLencode('SELECT * FROM "(TOS) VST: Mapping & Tagging [PROD]"'))
vst <- paste0(URLencode('SELECT * FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae"'))

get_Fulcrum_data <- function(api_token, sql){
  require(httr)
  url = paste0("https://api.fulcrumapp.com/api/v2/query?token=", 
               api_token, "&format=json", "&q=", sql, "&headers=true")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, 
                                        Accept = "application/json"))
  content <- jsonlite::fromJSON(httr::content(request, as = "text"))
  return(content$rows)
}

v = get_Fulcrum_data(api_token = api_token, sql = vst)