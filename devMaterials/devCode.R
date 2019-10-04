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
  


### SQL Query development
##  Different queries to get distinct eventids present in the woody child table to pass to drop-down
#   Query obtains eventids after joining parent AI data with Woody child data to only find those eventids associated with woody sampling
SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY';

#   Query obtains distinct eventids from parent records associated with woody child records, no need to join
SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" WHERE siteid LIKE 'ABBY' AND total_woody_stems IS NOT NULL;

###   Queries to obtain Apparent Individual data given user-selected siteid and eventid
# Retrieves all columns from joined parent/child records
SELECT * FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY' AND eventid LIKE 'vst_ABBY_2018';

# Retrieves specified columns from joined parent/child records
SELECT parent._record_id, parent.load_status, parent.domainid, parent.siteid, parent.plotid, parent.eventid, child._child_record_id, child.tagid, child.individualid, child.subplotid, child.nestedsubplotid, child.tagstatus, child.taxonid, child.plantstatus, child.growthform, child.shape, child.stemdiameter, child.measurementheight, child.basalstemdiameter, child.basalstemdiametermeasurementheight, child.vdapexheight, child.vdbaseheight, child.maxcrowndiameter, child.ninetycrowndiameter, child.canopyposition FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE siteid LIKE 'ABBY' AND eventid LIKE 'vst_ABBY_2018'; #  This works


SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" WHERE plotid LIKE 'BART_001';

SELECT parent._record_id, parent.eventid, parent.plotid, child._child_record_id, child.individualid, child.tagstatus, child.plantstatus, child.growthform FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child ON (parent._record_id = child._parent_id) WHERE eventid LIKE 'vst_BART_2018' AND plotid LIKE 'BART_033';

####   Example of Shiny query using two input variables
##  Construct Fulcrum query for plotIDs
aiPlotQuery <- reactive({
  # Account for null input before user selects a site
  shiny::validate(
    need(input$eventChoice != "", "")
  )
  # Join is needed to get only plotIDs from woody stem measurement events
  paste(URLencode('SELECT DISTINCT plotid FROM "VST: Apparent Individuals [PROD]" AS parent 
                    JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'), URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE siteid LIKE '", input$siteChoice, "'", "AND eventid LIKE '", input$eventChoice, "'")),
        sep = "%20")
})


### Method for aligning multiple ggplot2 objects on the page
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/