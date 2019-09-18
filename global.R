library(dplyr)
library(httr)
library(jsonlite)
library(stringr)
library(ggplot2)
library(ggrepel)
library(shiny)
library(shinythemes)

######################################################
###### CONSTANTS ######
#######################
### Setup Fulcrum querying to obtain data for drop-downs and for plotting and download
# Define api-token
api_token = "3ab235047ec293b27f06f6819e81b291435f9c61282345ff1de9624f744034b4233a6fcd1b87c3c2"



### Domain/site lookup - filtered below to only sites with data
domain_site <- read.csv("data/domain_site_lookup.csv", stringsAsFactors = FALSE, header = TRUE)



###  Spatial data input
## Read in plot spatial data, filter to applicableModules with 'vst', select desired columns, and rename
# fields to match Fulcrum data. Plot spatial data needed for 'plotSize' variable, and assigning of 
# new plottype variable to account for different types of Tower Plots - smStat, lgStat
plotSpatial <- read.csv("data/plotSpatialData_20180827.csv", stringsAsFactors = F, header = T)
plotSpatial %>%
  filter(grepl('vst', applicableModules)) %>%
  select(siteID, plotID, plotType, plotSize) %>%
  rename(siteid=siteID, plotid=plotID, plottype=plotType, plotsize=plotSize) %>%
  arrange(siteid, plotid) -> plotSpatial

## In 'plotSpatial' data, create new 'plottype' variable that accounts for type of Tower Plot
plotSpatial$newType <- ""

# Assign 'newType = distributed' for all 'distributed' plots
plotSpatial[plotSpatial$plottype=="distributed",][,"newType"] <- "distributed"

# Assign 'newType = smStatTower | lgStatTower' based on plotsize
sites <- unique(plotSpatial$siteid)
for (i in 1:length(sites)){
  sitePlots <- filter(plotSpatial, siteid==sites[i])
  if(400 %in% sitePlots$plotsize){
    plotSpatial[plotSpatial$siteid==sites[i] & plotSpatial$plottype=="tower",][,"newType"] <- "smTower"
  } else {
    plotSpatial[plotSpatial$siteid==sites[i] & plotSpatial$plottype=="tower",][,"newType"] <- "lgTower"
  }
}

# Remove original 'plottype', 'siteid', and 'plotsize', and rename 'newType -> plottype'
plotSpatial %>% select(-plottype, -siteid, -plotsize) %>% rename(plottype=newType) -> plotSpatial


## Read in point spatial data, select desired columns, filter to applicableModule with 'vst', and 
## rename fields to match Fulcrum data. Then create 'plotpointid' for joining with 'vstInput'
pointSpatial <- read.csv("data/pointSpatialData_20180827.csv", stringsAsFactors = F, header = T)
pointSpatial %>% 
  filter(grepl('vst', applicableModules)) %>%
  select(plotID, pointID, decimalLatitude, decimalLongitude, easting, northing) %>%
  rename(plotid=plotID, pointid=pointID, latitude=decimalLatitude, longitude=decimalLongitude, 
         pointeasting=easting, pointnorthing=northing) %>%
  arrange(plotid, pointid) %>%
  mutate(plotpointid = paste(plotid, pointid, sep = "_")) -> pointSpatial

pointSpatial$pointid <- as.integer(pointSpatial$pointid)



### Define additional parameters needed to work with site VST data in server.R
# Define expected pointid values for Distributed/smTower Plots, and lgTower Plots
expSmall <- c(31,33,41,49,51,21,25,57,61)
expLarge <- c(expSmall,23,39,43,59)






######################################################
###### FUNCTIONS ######
#######################
# Define function for converting azimuth degrees to radians
radians = function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}

# Fulcrum query function for getting data
get_Fulcrum_data <- function(api_token, sql, urlEncode = T){
  require(httr)
  require(jsonlite)
  if(urlEncode){
    sql = URLencode(sql)
  }
  url  <-  paste0("https://api.fulcrumapp.com/api/v2/query?token=", api_token, 
                  "&format=json", "&q=", sql, "&headers=true")
  request <- httr::GET(url, add_headers("X-ApiToken" = api_token, 
                                        Accept = "application/json"))
  content <- jsonlite::fromJSON(httr::content(request, as = "text")) 
  return(content$rows)
}



######################################################
###### APP DATA ######
######################
### Filter 'domain_site' list to only those domains/sites with data in Fulcrum VST M&T table
# Define SQL query to generate a list of sites that have Mapping & Tagging data
# VST: Mapping and Tagging [PROD] form_id is used for query.
siteidQuery <- URLencode('SELECT DISTINCT siteid FROM "VST: Mapping and Tagging [PROD]"')

# Query Fulcrum for list of siteid values that exist in the data, remove 'NA'
sitesWithData <- get_Fulcrum_data(api_token = api_token, sql = siteidQuery)
sitesWithData %>% 
  filter(!is.na(siteid)) %>%
  arrange(siteid) -> sitesWithData

# Filter `domain_site` to those with data
domain_site %>%
  filter(siteid %in% sitesWithData$siteid) -> domain_site

# Create domain list for drop-down for domains with data
theDomains <- unique(domain_site$domainid)

# Define nested subplot names for Plot Map nestedsubplotsize table
nestedNames <- c("smt+sap+sis+sms", "lia", "other")






