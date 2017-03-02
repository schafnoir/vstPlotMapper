library(dplyr)

###  Spatial data input
## Read in plot spatial data, filter to applicableModules with 'vst', select desired columns, and rename
# fields to match Fulcrum data. Plot spatial data needed for 'plotSize' variable, and assigning of 
# new plottype variable to account for different types of Tower Plots - smStat, lgStat
plotSpatial <- read.csv("data/plotSpatialData_20161206.csv", stringsAsFactors = F, header = T)
plotSpatial %>%
  filter(grepl('vst', applicableModules)) %>%
  select(siteID, plotID, plotType, plotSize, decimalLatitude, decimalLongitude) %>%
  rename(siteid=siteID, plotid=plotID, plottype=plotType, plotsize=plotSize, latitude=decimalLatitude, longitude=decimalLongitude) %>%
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
pointSpatial <- read.csv("data/pointSpatialData_20161206.csv", stringsAsFactors = F, header = T)
pointSpatial %>% 
  filter(grepl('vst', applicableModules)) %>%
  select(plotID, pointID, easting, northing) %>%
  rename(plotid=plotID, pointid=pointID, pointeasting=easting, pointnorthing=northing) %>%
  arrange(plotid, pointid) %>%
  mutate(plotpointid = paste(plotid, pointid, sep = "_")) -> pointSpatial

pointSpatial$pointid <- as.integer(pointSpatial$pointid)


###  VST mapping and tagging data input
## Read in Fulcrum data, and select relevant fields
# Did not select 'plottype' from the data because not all records have plottype assigned
# Did not select 'plotsize' from the data because most records have no plotsize assigned
# Did not select 'applicableModules' because most records are "" in this table
# Subplot is re-assigned based plottype and plotsize information, existing subplotid values include 10m x 10m 'subplots'
# from Plant Div that are not used in VST.
vstInput <- read.csv("data/tos_vst_mapping_tagging_prod.csv", header=T, stringsAsFactors = F)
vstInput %>% select(randomsubplota, randomsubplotb, nestedshrubsapling, nestedliana, nestedother, 
                   bouttype, plotid, siteid, taxonid, subplotid, nestedsubplotid, tagid, 
                   supportingstemtagid, pointid, stemdistance, stemazimuth) -> vstInput

# Join 'vstInput' with 'plotSpatial', and only keep 'vstInput' records with matching plotid
# Some records lacked matching plotid -> don't need in future with controlled plotid input?
vstInput <- inner_join(vstInput, plotSpatial, by = "plotid")

# Some subplotIDs were assigned at the 10m x 10m level, rather than 20m x 20m level;
# Assign new `subplotid` based on plotType using dplyr::mutate
vstInput %>% mutate(newsubplotid = ifelse(plottype!="lgTower", 31,
                                    ifelse(subplotid %in% c(21,22,30,31), 21,
                                    ifelse(subplotid %in% c(23,24,32,33), 23,
                                    ifelse(subplotid %in% c(39,40,48,49), 39,
                                    ifelse(subplotid %in% c(41,42,50,51), 41,
                                           "NA")))))) %>%
  select(-subplotid) %>%
  rename(subplotid=newsubplotid) -> vstInput

vstInput$subplotid <- as.integer(vstInput$subplotid)
vstInput$nestedshrubsapling <- as.integer(vstInput$nestedshrubsapling)

# After mutate function above, there are 10 records with 'corrSubplotID == NA' -> remove
vstInput %>% filter(!subplotid=="NA") -> vstInput

# Make 'plotsubplotid' to pipe to plotChoice drop-down: e.g., "(T) BART_050: 21"
vstInput %>% mutate(plotsubplotid = ifelse(plottype=="distributed", paste0("(D) ", plotid),
                                ifelse(plottype=="smTower", paste0("(T) ", plotid),
                                paste0("(T) ", plotid, ": ", subplotid)))) -> vstInput

# Create `pointstatus` variable to detect invalid pointid values based on plottype
expSmall <- c(31,33,41,49,51,21,25,57,61)
expLarge <- c(expSmall,23,39,43,59)
vstInput %>% mutate(pointstatus = ifelse(pointid=="NA", "notMapped",
                                ifelse(plottype=="lgTower" & pointid %in% expLarge, "validPointID",
                                ifelse(pointid %in% expSmall, "validPointID",
                                "errorPointID")))) -> vstInput

# Create 'plotpointid' variable for valid pointids only, to enable join with 'pointSpatial' data table
# Join to create `pointeasting` and `pointnorthing` in vstInput
vstInput %>% mutate(plotpointid = ifelse(pointid=="NA", "NA", 
                                  ifelse(pointstatus=="validPointID", paste(plotid, pointid, sep="_"),
                                  "NA"))) -> vstInput

vstInput <- left_join(vstInput, pointSpatial)


## Calculate 'stemeasting' and 'stemnorthing' using 'stemdistance' and 'stemazimuth'
# Define function for converting azimuth degrees to radians
radians = function(degrees) {
  rad = (degrees*pi)/180
  return(rad) 	
}

# Calculate 'stemeasting' and 'stemnorthing', and sort final dataset
vstInput %>% 
  mutate(stemeasting = round(pointeasting + stemdistance*sin(radians(stemazimuth))), digits = 2) %>%
  mutate(stemnorthing = round(pointnorthing + stemdistance*cos(radians(stemazimuth))), digits = 2) %>%
  arrange(siteid, plotid, subplotid) -> vstInput
  
  










