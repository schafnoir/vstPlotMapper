### Define server logic required to create plot map
shinyServer(function(input, output, session) {
  
  ###################################################################################################
  ###################################################################################################
  ##### Query data based on user input and create content for Plot Map tab ##########################
  ###################################################################################################
  ###################################################################################################
  
  
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
    # Account for null input before user selects a site or when user changes domains
    shiny::validate(
      need(input$siteChoice != "", "")
    )
    # Query eventids for those AI records that are associated with woody stem child records
    paste(URLencode('SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]"'),
          URLencode(paste0("WHERE siteid LIKE '", input$siteChoice, "'",
                           "AND total_woody_stems IS NOT NULL")),
          sep = "%20")
  })
  
  ##  Query Fulcrum to obtain list of Apparent Individual eventIDs with woody individuals for site
  aiEvents <- reactive({
    # Account for null input before user selects a site or when user changes domains
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
  ### Use `plotID`s present in joined dataset to populate plot selection drop-down
 
  ### Query and retrieve Mapping and Tagging data based on user-selected siteID
  ##  Construct Fulcrum query
  mtQuery <- reactive({
    # Account for null input before user selects a site or when user changes domains
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
    # Account for null input before user selects a site or when user changes domains
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
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
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
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    # Get AI data
    temp <- get_Fulcrum_data(api_token = api_token, sql = aiQuery())
  })
  
  
  
  ### Prepare siteID x eventID dataset: Join AI and M&T data on individualid, and perform subsequent joints with plotSpatial and pointSpatial
  joinData <- reactive({
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    # Join aiData, mtData, and plotSpatial data frames
    temp <- dplyr::left_join(aiData(), mtData(), by = "individualid")
    temp <- dplyr::inner_join(temp, plotSpatial, by = "plotid")
    temp$pointid <- as.integer(temp$pointid)
    
    # Standardize `noneSelected` strings for nestedliana, nestedother, and nestedshrubsapling
    temp <- temp %>% 
      dplyr::mutate(nestedliana = ifelse(grepl("none", nestedliana), "noneSelected", nestedliana)) %>%
      dplyr::mutate(nestedother = ifelse(grepl("none", nestedother), "noneSelected", nestedother)) %>%
      dplyr::mutate(nestedshrubsapling = ifelse(grepl("none", nestedshrubsapling), "noneSelected", nestedshrubsapling))
    
    # Standardize 'tagStatus' strings
    temp <- temp %>%
      dplyr::mutate(recordtype = ifelse(grepl("mapping", recordtype), "mapAndTag", 
                                        ifelse(grepl("map and tag", recordtype), "mapAndTag",
                                               ifelse(grepl("tagging", recordtype), "tagOnly",
                                                      ifelse(grepl("tag only", recordtype), "tagOnly", recordtype)))))
    
    # Create new `ddsubplotid` variable for plot-selection drop-down
    temp <- temp %>%
      dplyr::mutate(ddsubplotid = as.integer(ifelse(plottype != "lgTower", 31, 
                                         ifelse(subplotid %in% c(21,22,30,31), 21,
                                                ifelse(subplotid %in% c(23,24,32,33), 23,
                                                       ifelse(subplotid %in% c(39,40,48,49), 39,
                                                              ifelse(subplotid %in% c(41,42,50,51), 41,
                                                                     "NA")))))))
    
    # Create `plotsubplotid` to later pipe to plotChoice drop-down: e.g., "(T) BART_050: 21"
    temp <- temp %>%
      dplyr::mutate(plotsubplotid = ifelse(ddsubplotid=="NA", "NA",
                                           ifelse(plottype=="distributed", paste0("(D) ", plotid),
                                                  ifelse(plottype=="smTower", paste0("(T) ", plotid),
                                                         paste0("(T) ", plotid, ": ", ddsubplotid)))))
    
    # Create `pointstatus` variable to identify if mapping has occurred from incorrect pointids
    temp <- temp %>%
      dplyr::mutate(pointstatus = ifelse(is.na(pointid), "notMapped",
                                         ifelse(plottype=="lgTower" & pointid %in% expLarge, "validPointID",
                                                ifelse(pointid %in% expSmall, "validPointID",
                                                       "errorPointID"))))
    
    # Create `plotpointid` to join with pointspatial data frame to enable tree easting/northing calculation
    temp <- temp %>%
      dplyr::mutate(plotpointid = ifelse(is.na(pointid), "NA",
                                         ifelse(pointstatus=="validPointID", paste(plotid, pointid, sep = "_"),
                                                "NA")))
    
    # Join with pointSpatial data to obtain `pointeasting` and `pointnorthing` data then calculate `stemeasting` and `stemnorthing`
    temp <- dplyr::left_join(temp, pointSpatial, by = "plotpointid") %>% select(-plotid.y, -pointid.y) %>% 
      rename(plotid = plotid.x, pointid = pointid.x)
    
    temp <- temp %>%
      mutate(stemeasting = round(pointeasting + stemdistance*sin(radians(stemazimuth)), digits = 2)) %>%
      mutate(stemnorthing = round(pointnorthing + stemdistance*cos(radians(stemazimuth)), digits = 2)) %>%
      arrange(plotid, subplotid, nestedsubplotid)
  })
  
  
  
  ### Construct plot selection drop-down menu using plotsubplotid values in joined dataset
  ##  Obtain a list of plots available for the user-selected site
  thePlots <- reactive({
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    
    # Obtain list of distinct `plotsubplotid` values for selected eventid
    temp <- joinData() %>% filter(!is.na(plotsubplotid)) %>% distinct(plotsubplotid) %>% arrange(plotsubplotid)
  })
  
  ##  Populate drop-down with plot list from above
  output$plotSelect <- renderUI({
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    
    # Create drop-down
    selectInput("plotChoice", "Select a plot to map:", c(Choose='', choices = thePlots()),
                  selectize = TRUE, multiple = FALSE)
  })
  
  
  
  
  ###################################################################################################
  ### Generate map content and create plot map
  
  ### Filter joinData() to get mapped individuals
  mapData <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Filter joinData() and remove leading zeroes from `tagid` field
    temp <- joinData() %>% filter(plotsubplotid==input$plotChoice, pointstatus=="validPointID")
    temp$tagid <- substr(temp$tagid, regexpr("[^0]", temp$tagid), nchar(temp$tagid))
    temp
  })
  
  
  
  ### Generate warnings if there are problems with the input dataset
  
  ##  Map Point Warning: Identify presence of individuals with `pointstatus` = errorPointID
  output$mapPointWarning <- renderText({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Check for presence of `pointstatus` = errorPointID
    temp <- joinData() %>% filter(plotsubplotid==input$plotChoice)
    if("errorPointID" %in% temp$pointstatus){
      warning <- "Mapping Error: Bother, an invalid pointID was used in the field to map at least one woody individual."
      return(warning)
    }
    
  })
  
  
  ##  No Map Data Warning: Identify when there are no mapped individuals within the plot
  output$mapDataWarning <- renderText({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Check for lack of mapped stems with validPointIDs in the plot
    if(nrow(mapData())==0){
      warning <- "Input Data Warning: No mapped individuals with valid pointIDs in this plot."
      return(warning)
    }
    
  })
  
  
  
  ### Given plotChoice from user, define the set of pointIDs associated with the plot or subplot to map
  subplotPoints <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else if (nchar(input$plotChoice)==12){
      c(31,33,41,49,51)
    } else if (str_sub(input$plotChoice, start=-2, end=-1)==21){
      c(21,23,31,39,41)
    } else if (str_sub(input$plotChoice, start=-2, end=-1)==23){
      c(23,25,33,41,43)
    } else if (str_sub(input$plotChoice, start=-2, end=-1)==39){
      c(39,41,49,57,59)
    } else {
      c(41,43,51,59,61)
    }
  })
  
  
  ##  Determine pointIDs for mapping the corners of the plot/subplot using `plotChoice` input from user
  cornerPoints <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else if (nchar(input$plotChoice)==12){
      setdiff(subplotPoints(), 41)
    } else {
      setdiff(subplotPoints(), c(31,33,49,51))
    }
  })
  
  
  ##  Extract pointID easting and northing data to add to the map
  currentPlotID <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Find current plotID
    temp <- unique(mapData()$plotid)
  })
  
  
  ##  Retrieve easting and northing data for points defining the selected plot
  mapPoints <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # For pointIDs defining selected plot, retrieve point spatial data
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      filter(pointSpatial, plotid==currentPlotID() & pointid %in% subplotPoints())
    }
  })
  
  
  ##  Isolate easting and northing coordinates required for making geom_segments that draw plot perimeter
  E1 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[1]])})
  N1 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[1]])})
  E2 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[2]])})
  N2 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[2]])})
  E3 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[3]])})
  N3 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[3]])})
  E4 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[4]])})
  N4 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[4]])})
  
  
  ##  Retrieve Plot Meta-Data nestedSubplot sizes for display below plot map
  # Obtain `nestedshrubsapling` size for selected plotsubplotid, omitting "NA" and "" values
  nestedShrubSapling <- reactive({
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      nss <- joinData() %>%
        filter(plotsubplotid==input$plotChoice, nestedshrubsapling != "NA", nestedshrubsapling != "") %>%
        distinct(nestedshrubsapling)
      nss <- nss[1,1]
    }
  })
  
  ##  Continue with retrieving additional Plot Meta-Data for nested other, etc. then build table for display
  
  
  
  ###################################################################################################
  ### Temporary output to see intermediate data and text
  # Temp text
  output$tempText <- renderText(
   nestedShrubSapling()
  )
  
  # Temp table
  output$tempTable <- renderTable(
    mapPoints()
  )
  
  
  
}) #  End ShinyServer function
