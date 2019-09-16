### Define server logic required to create plot map
shinyServer(function(input, output, session) {
  
  
  ###################################################################################################
  ### Construct menu content for siteID sidebar drop-down
  
  ##  Create filtered siteSelect dropdown from selected domainID
  theSites <- reactive({
    # Provide full list of sites if no domain is selected
    temp <- if (input$domainSelect==''){
      domain_site %>% select(siteid)
    } else {
      # reduce siteIDs to the domainID chosen
      domain_site %>% filter(domainid==input$domainSelect) %>% select(siteid)
    }
  })
  
  ##  Render output to siteID dropdown
  output$siteSelect <- renderUI({
    if(input$domainSelect==''){
      selectInput("siteChoice", "Select a NEON site:", c(Choose = '', choices = theSites()))
    } else if (nrow(theSites())==1) {
      textInput("siteChoice", "Select a NEON site:", value = theSites())
    } else {
      selectInput("siteChoice", "Select a NEON site:", c(Choose = '', choices = theSites()),
                  selectize = TRUE, multiple = FALSE)
    }
  })
  
  
  
  
  ###################################################################################################
  ### Construct menu content for eventID sidebar drop-down using eventIDs in Apparent Individuals
  ### for the site selected by the user
  
  ##  Construct Fulcrum query for eventIDs
  aiEventQuery <- reactive({
    # Account for null input before user selects a site
    shiny::validate(
      need(input$siteChoice != "", "")
    )
    # Join is needed to get only eventIDs from woody stem measurement events
    paste(URLencode('SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]" AS parent 
                    JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'), URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE siteid LIKE '", input$siteChoice, "'")),
          sep = "%20")
  })
  
  ##  Query Fulcrum to obtain list of Apparent Individual eventIDs with woody individuals for site
  aiEvents <- reactive({
    # Account for null input before user selects a site
    shiny::validate(
      need(input$siteChoice != "", "")
    )
    # Get distinct events for site from Fulcrum and sort for drop-down with latest eventid on top
    temp <- data.frame(get_Fulcrum_data(api_token = api_token, sql = aiEventQuery()), stringsAsFactors = FALSE)
    temp <- temp %>% dplyr::arrange(dplyr::desc(eventid))
  })
  
  ##  Render output to eventID dropdown
  output$eventSelect <- renderUI({
    selectInput("eventChoice", "Select a VST event:", c(Choose = '', choices = aiEvents()),
                  selectize = TRUE, multiple = FALSE)
  })
  
  
  
  
  ###################################################################################################
  ### Construct joined M&T and AI dataset for the siteID and eventID selected by the user.
  ### Make `plotsubplotID` variable and pipe to plotID drop-down for plot selection by user.
  
  ### Query and retrieve Mapping and Tagging data based on user-selected siteID
  ##  Construct Fulcrum query
  mtQuery <- reactive({
    # Account for the fact that input is null before user selects a site
    shiny::validate(
      need(input$siteChoice != "", "")
    )
    
    # Define Fulcrum query and specify required columns
    paste(URLencode('SELECT _record_id, cfconlytag, date, domainid, individualid, load_status, nestedliana, nestedother, nestedshrubsapling, nestedsubplotid, plotid, pointid, recordtype, samplingprotocolversion, siteid, stemazimuth, stemdistance, subplotid, tagid, taxonid, vstid, yearboutbegan FROM "VST: Mapping and Tagging [PROD]"'),
          URLencode(paste0("WHERE siteid LIKE '", input$siteChoice, "'")),
          sep = "%20")
  })
  
  ##  Retrieve Fulcrum M&T data for site using query
  mtData <- reactive({
    # Account for the fact that input is null before user selects a site
    shiny::validate(
      need(input$siteChoice != "", "")
    )
    # Get M&T data and keep only individualids with latest yearboutbegan
    temp <- get_Fulcrum_data(api_token = api_token, sql = mtQuery())
    temp <- temp %>% group_by(individualid) %>% filter(yearboutbegan == max(yearboutbegan)) %>% arrange(individualid) %>% distinct()
  })
  
  
  
  ### Query and retrieve AI data based on user-selected siteID and eventID
  ##  Construct Fulcrum query
  aiQuery <- reactive({
    # Account for the fact that input is null before user selects a site
    shiny::validate(
      #need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    
    # Define Fulcrum query and specify required columns
    paste(URLencode('SELECT parent._record_id, parent.load_status, parent.eventid, child._child_record_id, child.individualid, child.tagstatus, child.plantstatus, child.growthform, child.shape, child.stemdiameter, child.measurementheight, child.basalstemdiameter, child.basalstemdiametermeasurementheight, child.vdapexheight, child.vdbaseheight, child.maxcrowndiameter, child.ninetycrowndiameter, child.canopyposition FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'),
                   URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE siteid LIKE '", input$siteChoice, "'",
                                    "AND eventid LIKE '", input$eventChoice, "'")),
                   sep = "%20")
  })
  
  ##  Retrieve Fulcrum AI data for site x event using query
  aiData <- reactive({
    # Account for the fact that input is null before user selects an eventid
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    # Get AI data
    temp <- get_Fulcrum_data(api_token = api_token, sql = aiQuery())
  })
  
  
  
  ### Join AI and M&T data on individualid, and create plotsubplotid variable required for creating plot map
  joinData <- reactive({
    # Account for the fact that input is null before user selects an eventid
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    # Join aiData, mtData, and plotSpatial data frames
    jd <- dplyr::left_join(aiData(), mtData(), by = "individualid")
    jd <- dplyr::inner_join(jd, plotSpatial, by = "plotid")
    jd$pointid <- as.integer(jd$pointid)
    
    # Standardize `noneSelected` strings for nestedliana, nestedother, and nestedshrubsapling
    jd <- jd %>% 
      dplyr::mutate(nestedliana = ifelse(grepl("none", nestedliana), "noneSelected", nestedliana)) %>%
      dplyr::mutate(nestedother = ifelse(grepl("none", nestedother), "noneSelected", nestedother)) %>%
      dplyr::mutate(nestedshrubsapling = ifelse(grepl("none", nestedshrubsapling), "noneSelected", nestedshrubsapling))
    
    # Standardize 'tagStatus' strings
    jd <- jd %>%
      dplyr::mutate(recordtype = ifelse(grepl("mapping", recordtype), "mapAndTag", 
                                        ifelse(grepl("map and tag", recordtype), "mapAndTag",
                                               ifelse(grepl("tagging", recordtype), "tagOnly",
                                                      ifelse(grepl("tag only", recordtype), "tagOnly", recordtype)))))
    
    # Create new `ddsubplotid` variable for plot-selection drop-down
    jd <- jd %>%
      dplyr::mutate(ddsubplotid = ifelse(plottype != "lgTower", 31, 
                                         ifelse(subplotid %in% c(21,22,30,31), 21,
                                                ifelse(subplotid %in% c(23,24,32,33), 23,
                                                       ifelse(subplotid %in% c(39,40,48,49), 39,
                                                              ifelse(subplotid %in% c(41,42,50,51), 41,
                                                                     "NA"))))))
    
    # Create 'plotsubplotid' to pipe to plotChoice drop-down: e.g., "(T) BART_050: 21"
    jd <- jd %>%
      dplyr::mutate(plotsubplotid = ifelse(ddsubplotid=="NA", "NA",
                                           ifelse(plottype=="distributed", paste0("(D) ", plotid),
                                                  ifelse(plottype=="smTower", paste0("(T) ", plotid),
                                                         paste0("(T) ", plotid, ": ", ddsubplotid)))))
    
    # Continue here with creating plotpointid to join with pointspatial data frame...
    
    
  })
  
  
  
  ### Temporary output to see intermediate data and text
  # Temp text
  output$tempText <- renderPrint(
    # Account for the fact that input is null before user selects an eventid
    class(joinData()$pointid)
  )
  
  # Temp table
  output$tempTable <- renderTable(
    head(joinData())
  )
  
  
  
}) #  End ShinyServer function
