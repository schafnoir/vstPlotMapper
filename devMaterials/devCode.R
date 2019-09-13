###   Scratchpad for developing VST Mapper app

##    Fulcrum query to obtain M&T data by site, then clean up to remove duplicate individualIDs
##    based on most recent vstID

siteChoice <- "ABBY"
mtQuery <- paste(URLencode('SELECT eventid, individualid, load_status, nestedliana, nestedother, nestedshrubsapling, nestedsubplotid, plotid, pointid, _record_id, recordtype, siteid, stemdistance, stemazimuth, subplotid, supportingstemtagid, tagid, taxonid, vstid, yearboutbegan
                           FROM "d0b95d92-3345-4b40-9767-c28ddbbacfae"'),
                 URLencode(paste0("WHERE siteid LIKE '", siteChoice, "'")),
                 sep = "%20")

mtData <- get_Fulcrum_data(api_token = api_token, sql = mtQuery)

#   Check whether there are duplicates based on individualid
mtData %>% distinct(individualid) -> test # Appear to be, though this code only keeps individualid
mtData %>% group_by(individualid) %>% filter(n()>1) %>% arrange(individualid) -> test #   Identifies duplicated entries

#   Filters out duplicated individualid and keeps record with latest yearboutbegan
mtData %>% group_by(individualid) %>% filter(yearboutbegan == max(yearboutbegan)) %>% arrange(individualid) -> filMtData



##    Fulcrum query to use siteChoice and obtain available eventids in Apparent Individuals
#     Fulcrum formID for the VST AI table "889110ae-f81b-41bc-a3bb-1686995b052e"
#     Needs to be done on the join to make sure eventids contain woody individual data
aiEventQuery <- paste(URLencode('SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'),
                      URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE siteid LIKE '", siteChoice, "'")),
                      sep = "%20")

aiEvents <- get_Fulcrum_data(api_token = api_token, sql = aiEventQuery) %>% arrange(eventid)


##    Fulcrum query to use siteChoice and eventChoice (new drop-down to be added) to get specific AI data.
eventChoice <- "vst_ABBY_2018"
aiQuery <- paste(URLencode('SELECT parent._record_id, parent.load_status, parent.domainid, parent.siteid, parent.plotid, parent.eventid, child._child_record_id, child.tagid, child.individualid, child.subplotid, child.nestedsubplotid, child.tagstatus, child.taxonid, child.plantstatus, child.growthform, child.shape, child.stemdiameter, child.measurementheight, child.basalstemdiameter, child.basalstemdiametermeasurementheight, child.vdapexheight, child.vdbaseheight, child.maxcrowndiameter, child.ninetycrowndiameter, child.canopyposition FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'),
                 URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE siteid LIKE '", siteChoice, "'",
                                  "AND eventid LIKE '", eventChoice, "'")),
                 sep = "%20")
  
 aiData <- get_Fulcrum_data(api_token = api_token, sql = aiQuery)   # This works
  
##  Queries that work using Fulcrum Query Utility
#   Query to get distinct eventids present in the woody child table to pass to drop-down
SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY';

SELECT * FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY' AND eventid LIKE 'vst_ABBY_2018'; #  This works

SELECT parent._record_id, parent.load_status, parent.domainid, parent.siteid, parent.plotid, parent.eventid, child._child_record_id, child.tagid, child.individualid, child.subplotid, child.nestedsubplotid, child.tagstatus, child.taxonid, child.plantstatus, child.growthform, child.shape, child.stemdiameter, child.measurementheight, child.basalstemdiameter, child.basalstemdiametermeasurementheight, child.vdapexheight, child.vdbaseheight, child.maxcrowndiameter, child.ninetycrowndiameter, child.canopyposition FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY' AND eventid LIKE 'vst_ABBY_2018'; #  This works
