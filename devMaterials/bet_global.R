# Spatial Data
plots <- read.csv("data/plotSpatialData_20160527.csv", stringsAsFactors = F) 
points <- read.csv("data/beetlePitfallLocations.csv", stringsAsFactors = F)
postalCodes <- read.csv("data/postalCodes.csv", stringsAsFactors = F)
domainStaff <- read.csv("data/domainStaff.csv", stringsAsFactors = F)

plots <- plots[grepl("distributed", plots$plotType) & grepl("basePlot", plots$subtype), ]
points <- points[grepl("bet", points$applicableModules), ]
points$elevation <- round(points$elevation, digits = 0)
postalCodes$state <- toupper(postalCodes$state)

# Find plot level info 
points$country <- "USA"
for (i in points$plotID){
  points$stateProvince[points$plotID==i] <- plots$stateProvince[plots$plotID==i][1]
  points$county[points$plotID==i] <- plots$county[plots$plotID==i][1]
  points$domainID[points$plotID==i] <- plots$domainID[plots$plotID==i][1]
  points$siteName[points$plotID==i] <- plots$siteName[plots$plotID==i][1]
}

# Retrieve proper names for states
for (i in points$stateProvince){
  points$stateProvince[points$stateProvince==i] <- postalCodes$state[postalCodes$postalCode==i]
}

# Retrieve collector info
for (i in points$domainID){
  points$collector[points$domainID==i] <- paste0(substr(domainStaff$firstName[domainStaff$domainID==i],1,1),
                                                 domainStaff$lastName[domainStaff$domainID==i])
}
points$collector[grepl("MOAB", points$plotID)] <- "AJacobs"
rm(i, plots, postalCodes, domainStaff)

# Final database
points <- points[grepl("[A-Z]", points$pointID), c("country", "stateProvince", "county", "siteName", 
                                                   "elevation", "decimalLatitude", "decimalLongitude", "collector",
                                                   "plotID", "pointID", "domainID")]
points$siteID <- substr(points$plotID, 1, 4)
points$trapID <- paste(points$plotID, points$pointID, sep=" ")

# Tweaks to some names
points$siteName[grepl("BART", points$siteID)] <- "Bartlett Expm Forest"
points$siteName[grepl("BLAN", points$siteID)] <- "Blandy Expm Farm"
points$siteName[grepl("CPER", points$siteID)] <- "Central Plains Expm Rng"
points$siteName[grepl("GRSM", points$siteID)] <- "Great Smoky Mtn Natl Park"
points$siteName[grepl("JERC", points$siteID)] <- "Jones Ecol Res Ctr"
points$siteName[grepl("OAES", points$siteID)] <- "Klemme Rng Res Stn"
points$siteName[grepl("CLBJ", points$siteID)] <- "LBJ Natl Grassland"
points$siteName[grepl("KONZ", points$siteID)] <- "Konza Prairie Biol Stn"
points$siteName[grepl("LAJA", points$siteID)] <- "Lajas Expm Stn"
points$siteName[grepl("NIWO", points$siteID)] <- "Niwot Ridge Mtn Res Stn"
points$siteName[grepl("STER", points$siteID)] <- "Sterling"
points$siteName[grepl("NOGP", points$siteID)] <- "Northern Great Plains Res Lab"
points$siteName[grepl("OSBS", points$siteID)] <- "Ordway-Swisher Biol Stn"
points$siteName[grepl("SJER", points$siteID)] <- "San Joaquin Expm Rng"
points$siteName[grepl("SRER", points$siteID)] <- "Santa Rita Expm Rng"
points$siteName[grepl("SCBI", points$siteID)] <- "Smithsonian Cons Biol Inst"
points$siteName[grepl("SERC", points$siteID)] <- "Smithsonian Env Res Ctr"
points$siteName[grepl("STEI", points$siteID)] <- "Chequamegon Natl Forest"
points$siteName[grepl("TALL", points$siteID)] <- "Talladega Natl Forest"
points$siteName[grepl("UKFS", points$siteID)] <- "Univ of Kansas Field Stn"
points$siteName[grepl("UNDE", points$siteID)] <- "Notre Dame Env Res Ctr" #"UNDERC"