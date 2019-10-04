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
    temp <- if (input$mapDomainChoice==''){
      domain_site %>% select(siteid)
    } else {
      # reduce siteIDs to the domainID chosen
      domain_site %>% filter(domainid==input$mapDomainChoice) %>% select(siteid)
    }
  })
  
  ##  Render output to siteID dropdown
  output$siteSelect <- renderUI({
    if(input$mapDomainChoice==''){
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
  
  
  
  ### Prepare siteID x eventID dataset: Join AI and M&T data on individualid, and perform subsequent joins with plotSpatial and pointSpatial
  joinData <- reactive({
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    # Join aiData, mtData, and plotSpatial data frames and re-assign data types for selected columns
    temp <- dplyr::left_join(aiData(), mtData(), by = "individualid")
    temp <- dplyr::inner_join(temp, plotSpatial, by = "plotid")
    temp$pointid <- as.integer(temp$pointid)
    temp$plantstatus <- as.numeric(temp$plantstatus)
    
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
    selectInput("plotChoice", "Select a plot:", c(Choose='', choices = thePlots()),
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
  output$mapPointError <- renderText({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Check for presence of `pointstatus` = errorPointID
    temp <- joinData() %>% filter(plotsubplotid==input$plotChoice)
    if("errorPointID" %in% temp$pointstatus){
      warning <- "Mapping Error: An invalid pointID was used in the field to map at least one woody individual. To find errors, go to the Plot Data tab and search by 'mapStatus'."
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
  
  
  ###  Retrieve easting and northing data for points defining the selected plot
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
  
  
  ###  Isolate easting and northing coordinates required for making geom_segments that draw plot perimeter
  E1 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[1]])})
  N1 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[1]])})
  E2 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[2]])})
  N2 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[2]])})
  E3 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[3]])})
  N3 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[3]])})
  E4 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[4]])})
  N4 <- reactive({ifelse(input$plotChoice=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[4]])})
  
  
  
  ### Build plot title and ggplot object for display
  ##  Build plot title
  output$plotTitle <- renderText({
    shiny::validate(
      #need(input$siteChoice != "", ""),
      need(input$siteChoice != "" && input$eventChoice != "" && input$plotChoice != "", "Please select a plot to map")
    )
    paste0("Plot Map: ", input$plotChoice)
  })
  
  
  ##  Build ggplot object
  ggMap <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$plotChoice != "", "")
    )
    # Create the basic ggplot from the mapPoints data
    ggplot(mapPoints(), aes(pointeasting, pointnorthing)) +
      
      # Set the aspect ratio using the mapPoint data
      coord_fixed() +
      
      # Add axis labels
      labs(x = "Easting (meters)",
           y = "Northing (meters)") +
      
      # Remove axis and grid lines from panel
      theme(panel.grid = element_blank()) +
      theme(axis.text = element_text(size = 10)) +
      theme(axis.title = element_text(size = 12, face = "bold")) +
      theme(legend.text = element_text(size=10)) +
      theme(title = element_text(size=14)) +
      
      # Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
      scale_x_continuous(breaks=seq(round(min(mapPoints()$pointeasting)), max(mapPoints()$pointeasting),5)) +
      scale_y_continuous(breaks=seq(round(min(mapPoints()$pointnorthing)), max(mapPoints()$pointnorthing),5)) +
      
      # Draw perimeter of plot/subplot
      geom_segment(x = E1(), y = N1(), xend = E2(), yend = N2(), color = "grey30") +
      geom_segment(x = E2(), y = N2(), xend = E4(), yend = N4(), color = "grey30") +
      geom_segment(x = E1(), y = N1(), xend = E3(), yend = N3(), color = "grey30") +
      geom_segment(x = E3(), y = N3(), xend = E4(), yend = N4(), color = "grey30") +
      
      # Add corner points (and centroid if present) using high-res GPS data
      geom_point(size=3, shape=21, colour="black", fill="red", stroke=0.5, show.legend = FALSE)
  })
  
  
  ##  Render the ggplot to output, and include plot modifications from user input
  output$mapPlot <- renderPlotly({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$plotChoice != "", "")
    )
    # Add layers to base map of plot
    p <- ggMap()
    if (input$taxonRadio=="color") p <- p + geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, color = taxonid,
                                                                            size = stemdiameter, label = tagid),
                                                      shape = 21, stroke = 0.5, show.legend = TRUE)
    if (input$taxonRadio=="shape") p <- p + geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, shape = taxonid,
                                                                            label = tagid),
                                                      size = 2.75, stroke = 0.5, show.legend = TRUE) +
      scale_shape_discrete(solid = FALSE)
    
    # Convert output to interactive plotly
    p <- ggplotly(p, height = 900, tooltip = c("tagid", "taxonid", "stemdiameter"))
    
    # Move legend to middle of plot rather than top
    p <- p %>% layout(legend = list(x=100, y=0.5))
    
  })
  
  
  
  
  ### Retrieve Plot Meta-Data nestedSubplot sizes for display below plot map
  ##  Obtain `nestedshrubsapling` size for selected plotsubplotid, omitting "NA" and "" values
  nestedShrubSapling <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Retrieve nestedshrubsapling value for selected plot
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      nss <- joinData() %>%
        filter(plotsubplotid==input$plotChoice, nestedshrubsapling != "NA", nestedshrubsapling != "") %>%
        distinct(nestedshrubsapling)
      nss <- nss[1,1]
    }
  })
  
  
  ##  Obtain `nestedliana` size for selected plotsubplotid, omitting "NA" and "" values
  nestedLiana <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Retrieve nestedliana value for selected plot
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      nl <- joinData() %>%
        filter(plotsubplotid==input$plotChoice, nestedliana != "NA", nestedliana != "") %>%
        distinct(nestedliana)
      nl <- nl[1,1]
    }
  })
  
  
  ##  Obtain `nestedother` size for selected plotsubplotid, omitting "NA" and "" values
  nestedOther <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Retrieve nestedliana value for selected plot
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      no <- joinData() %>%
        filter(plotsubplotid==input$plotChoice, nestedother != "NA", nestedother != "") %>%
        distinct(nestedother)
      no <- no[1,1]
    }
  })
  
  
  ##  Construct caption for use in downloaded .pdf version of Plot Map
  captionInput <- reactive({
    temp <- if (input$plotChoice==''){
      return(NULL)
    } else {
      paste("", "Nested Subplot Sizes:",
            paste0("nestedSubplotSize (smt + sap + sis + sms) = ", nestedShrubSapling()),
            paste0("nestedSubplotSize (lia) = ", nestedLiana()),
            paste0("nestedSubplotSize (other) = ", nestedOther()),
            sep = "\n")
    }
  })
  
  
  ### Construct nestedSubplotSize table for display beneath plot map
  ##  Create table
  nestedDF <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", ""),
      need(input$plotChoice != "", "")
    )
    # Create nested subplot data frame for table rendering
    temp <- data.frame(cbind(nestedShrubSapling(), nestedLiana(), nestedOther()))
    colnames(temp) <- nestedNames
    temp
  })
  
  
  ##  Create table output for display
  output$nestedTable <- renderTable(
    nestedDF(),
    align = 'c'
  )
  
  
  
  ###  Create .pdf download output from ggplot <-- Need to create caption and add it back here so that nested subplot info gets into pdf
  ##  Build the ggplot object for download and printing
  ggDownload <- reactive({
    ggplot(mapPoints(), aes(pointeasting, pointnorthing)) +
      # Set the aspect ratio using the mapPoint data
      coord_fixed() +
      
      # Add title, axis labels, and caption
      labs(title = paste0("Plot Map: ", input$plotChoice),
           x = "Easting (meters)",
           y = "Northing (meters)",
           caption = captionInput()) +
      
      # Remove axis and grid lines from panel, place legend at bottom, and size text output for printing/download
      theme(panel.grid = element_blank()) +
      theme(plot.title = element_text(size=11, face = "bold")) +
      theme(axis.text = element_text(size = 8)) +
      theme(axis.title = element_text(size = 9, face = "bold")) +
      theme(legend.text = element_text(size=7)) +
      
      # Set axis ticks to begin at minimum easting and northing values in mapPoints, and space every 5 meters
      scale_x_continuous(breaks=seq(round(min(mapPoints()$pointeasting)), max(mapPoints()$pointeasting),5)) +
      scale_y_continuous(breaks=seq(round(min(mapPoints()$pointnorthing)), max(mapPoints()$pointnorthing),5)) +
      
      # Rotate y-axis labels 90-degrees
      theme(axis.text.y  = element_text(angle=90, hjust=0.5)) +
      
      # Draw perimeter of plot/subplot
      geom_segment(x = E1(), y = N1(), xend = E2(), yend = N2(), color = "grey30") +
      geom_segment(x = E2(), y = N2(), xend = E4(), yend = N4(), color = "grey30") +
      geom_segment(x = E1(), y = N1(), xend = E3(), yend = N3(), color = "grey30") +
      geom_segment(x = E3(), y = N3(), xend = E4(), yend = N4(), color = "grey30") +
      
      # Add corner points (and centroid if present) using high-res GPS data
      geom_point(size=2, shape=21, colour="black", fill="red", stroke=1, show.legend = FALSE)
    
  })
  
  
  ##  Add layers to download plot based on user input
  ggFinalDownload <- reactive({
    p = ggDownload()
    if (input$taxonRadio=="color") p = p + 
        geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, color = taxonid, size = stemdiameter),
                   shape = 21, stroke = 0.5, show.legend = TRUE)
    
    if (input$taxonRadio=="shape") p = p +
        geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, shape = taxonid), size = 2, stroke = 0.5, show.legend = TRUE) +
        scale_shape_discrete(solid = FALSE)
    
    if ("tags" %in% input$labelChecks) p = p + 
        geom_text_repel(data=mapData(), aes(x=stemeasting, y=stemnorthing, label=tagid), size=2, nudge_x = 0.3, nudge_y = 0.3)
    
    if ("markers" %in% input$labelChecks) p = p + geom_label(aes(label = pointid), fill = "#FF6C5E", colour="white", size=2, show.legend = FALSE)
    p
  })
  
  
  ##  Pipe ggDownload output to .pdf file for local saving
  output$downloadPlotMap <- downloadHandler(
    filename = function(){
      paste0(paste(unique(mapData()$plotid), unique(mapData()$subplotid), sep = "_"), '.pdf')
    },
    content = function(file) {
      ggsave(file, plot = ggFinalDownload(), width = 6.5, units = "in", device = "pdf")
    }
  )
  
  
  
  
  ###################################################################################################
  ###################################################################################################
  ##### Gather user input and create content for Plot Data tab ######################################
  ###################################################################################################
  ###################################################################################################
  
  
  
  ###################################################################################################
  ### Construct sidebar menu content
  ##  Output domainID selected in Plot Map tab
  output$dataDomainSelect <- renderText(
    input$mapDomainChoice
  )
  
  ##  Output siteID selected in Plot Map tab
  output$dataSiteSelect <- renderText(
    input$siteChoice
  )
  
  ##  Output eventID selected in Plot Map tab
  output$dataEventSelect <- renderText(
    input$eventChoice
  )
  
  ##  Create separate plotID drop-down to populate tables without going back to Plot Map tab
  output$dataPlotSelect <- renderUI({
    # Account for null input before user selects an eventid or when user changes sites or domains
    shiny::validate(
      need(input$siteChoice != "", ""),
      need(input$eventChoice != "", "")
    )
    
    # Create drop-down
    if(input$plotChoice=='' | input$plotChoice=='undefined'){
      selectInput(inputId = "dataPlotChoice",
                  label = "Select a plot:",
                  choices = c(Choose = '', choices = thePlots()),
                  selectize = TRUE,
                  multiple = FALSE)
    } else {
      selectInput(inputId = "dataPlotChoice",
                  label = NULL,
                  choices = thePlots(),
                  selected = input$plotChoice,
                  selectize = TRUE,
                  multiple = FALSE)
    }
  })
  
  
  
  ###################################################################################################
  ### Generate table data by filtering joinData() by user-selected plotID
  plotData <- reactive({
    # Account for null input before user selects a plotid or when user changes events, sites, or domains
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "Please select a plot")
    )
    
    # Filter joinData() to get all woody records for the plot and remove cfcOnlyTag records
    temp <- joinData() %>% filter(plotsubplotid==input$dataPlotChoice) %>% 
      filter(is.na(cfconlytag) | cfconlytag=='N') %>%
      rename(mapStatus = pointstatus, aiFulcrumID = `_record_id.x`, load_status = `load_status.x`)
    
    # Replace `fulcrum_id` field values with new 'Edit Record' button
    temp$aiFulcrumID <- paste0("https://web.fulcrumapp.com/records/", temp$aiFulcrumID)
    temp$aiFulcrumID[temp$load_status=="NONE"] <- paste0('<a href="',temp$aiFulcrumID[temp$load_status=="NONE"],'"',
                                                         ' target="_blank" class="btn btn-primary">Edit Record</a>')
    temp$aiFulcrumID[temp$load_status!="NONE"] <- paste0('<a href="',temp$aiFulcrumID[temp$load_status!="NONE"],'"',
                                                         ' target="_blank" class="btn btn-info">View Loaded Record</a>')
    
    # Select columns needed for drop-down generation and display and rename some to shorten for display
    temp <- temp %>% 
      select(aiFulcrumID, basalstemdiameter, basalstemdiametermeasurementheight, ddsubplotid, eventid,
             growthform, individualid, load_status, mapStatus, measurementheight, nestedsubplotid, plantstatus, plotid, 
             recordtype, shape, stemdiameter, subplotid, tagid, tagstatus, taxonid) %>%
      rename(basalStemDiam = basalstemdiameter, basalMHeight = basalstemdiametermeasurementheight,
             eventID = eventid, gForm = growthform, mHeight = measurementheight, nSubID = nestedsubplotid,
             pStatus = plantstatus, plotID = plotid, recordType = recordtype, stemDiam = stemdiameter,
             subID = subplotid, tagID = tagid, tagStatus = tagstatus, taxonID = taxonid) %>%
      arrange(subID, nSubID, tagID)
    
    # Fix null values in gForm that otherwise result in loss of records when table is displayed
    temp <- temp %>% mutate(gForm = ifelse(is.na(gForm), "blank", gForm))
    
    # Arrange fields
    temp <- temp[c("aiFulcrumID", "load_status", "eventID", "plotID", "subID", "nSubID", "recordType", "mapStatus",
                   "tagID", "tagStatus", "taxonID", "gForm", "mHeight", "stemDiam", "basalMHeight", "basalStemDiam",
                   "pStatus", "shape", "ddsubplotid", "individualid")]
    
  })
  
  
  
  ### Generate choices for recordtype drop-down filter
  ##  Create reactive variable from recordtype
  ddRecordType <- reactive({
    # Account for null input before user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Obtain list of recordtype values for selected dataPlotChoice
    temp <- sort(unique(plotData()$recordType))
  })
  
  
  ##  Populate drop-down with recordtype values
  output$recordTypeSelect <- renderUI({
    # Account for null input beore user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Create drop-down
    pickerInput(inputId = "record_type", label = NULL,
                choices = ddRecordType(),
                selected = ddRecordType(),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  
  
  ### Generate choices for tagstatus drop-down filter
  ##  Create reactive variable from tagstatus
  ddTagStatus <- reactive({
    # Account for null input before user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Obtain list of tagstatus values for selected dataPlotChoice
    temp <- sort(unique(plotData()$tagStatus))
  })
  
  
  ##  Populate drop-down with tagstatus values
  output$tagStatusSelect <- renderUI({
    # Account for null input beore user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Create drop-down
    pickerInput(inputId = "tag_status", label = NULL,
                choices = ddTagStatus(),
                selected = ddTagStatus(),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  
  
  ### Generate choices for plantstatus drop-down filter
  ##  Create reactive variable from plantstatus
  ddPlantStatus <- reactive({
    # Account for null input before user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Obtain list of plantstatus values for selected dataPlotChoice
    temp <- sort(unique(plotData()$pStatus))
  })
  
  
  ##  Populate drop-down with plantstatus values
  output$plantStatusSelect <- renderUI({
    # Account for null input beore user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Create drop-down
    pickerInput(inputId = "plant_status", label = NULL,
                choices = ddPlantStatus(),
                selected = ddPlantStatus(),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  
  
  ### Generate choices for growthform drop-down filter
  ##  Create reactive variable from growthform
  ddGrowthForm <- reactive({
    # Account for null input before user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Obtain list of growthform values for selected dataPlotChoice
    temp <- sort(unique(plotData()$gForm))
  })
  
  
  ##  Populate drop-down with growthform values
  output$growthFormSelect <- renderUI({
    # Account for null input beore user selects a dataPlotChoice
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Create drop-down
    pickerInput(inputId = "growth_form", label = NULL,
                choices = ddGrowthForm(),
                selected = ddGrowthForm(),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  
  
  ### Render plotData() for display in Data Table tab
  output$filterDataTable <- DT::renderDataTable(
    DT::datatable(
      temp <- plotData() %>% 
        filter(recordType %in% input$record_type, tagStatus %in% input$tag_status,
               pStatus %in% input$plant_status, gForm %in% input$growth_form) %>%
        select(-ddsubplotid, -eventID, -load_status, -plotID, -individualid), 
      escape = FALSE, filter = "top" 
    )
  )
  
  
  
  ### Create download file and button for filtered table contents
  # Create download .csv
  output$downloadDataTable <- downloadHandler(
    filename = function() { 
      paste0(paste(unique(plotData()$plotID), unique(plotData()$ddsubplotid), sep = "_"), '.csv') 
    },
    content = function(file) {
      temp <- plotData() %>%
        filter(recordType %in% input$record_type, tagStatus %in% input$tag_status,
               pStatus %in% input$plant_status, gForm %in% input$growth_form) %>%
        select(-ddsubplotid, -individualid)
      write.csv(temp, file, row.names = FALSE)
    }
  )
  
  
  
  ###################################################################################################
  ##### QC Tables Tab content  ######################################################################
  ###################################################################################################
  
  ### Create summary table of strict duplicates found in plotData()
  ##  Create reactive data frame with strict duplicates
  dupPlotData <- reactive({
    temp <- plotData() %>% 
      filter(duplicated(plotData())) %>%
      select(aiFulcrumID, basalStemDiam, basalMHeight, gForm, mHeight, nSubID, pStatus, shape, stemDiam,
             subID, tagID, taxonID) %>%
      arrange(subID, nSubID, tagID)
    
    # Arrange fields
    temp <- temp[c("aiFulcrumID", "subID", "nSubID", "tagID", "taxonID", "gForm", "mHeight", "stemDiam",
                   "basalMHeight", "basalStemDiam", "pStatus", "shape")]
      
  })
  
  
  ##  Render dupPlotData for display
  output$strictDupes <- DT::renderDataTable(
    DT::datatable(
      temp <- dupPlotData(),
      escape = FALSE, filter = "top"
    )
  )
  
  
  
  ### Create summary table of duplicates in plotData() based on individualID only
  ##  Create reactive data frame with individualID duplicates
  iDupPlotData <- reactive({
    temp <- plotData() %>%
      filter(duplicated(plotData()$individualid)) %>%
      select(aiFulcrumID, basalStemDiam, basalMHeight, gForm, mHeight, nSubID, pStatus, shape, stemDiam,
             subID, tagID, taxonID) %>%
      arrange(subID, nSubID, tagID)
    
    # Arrange fields
    temp <- temp[c("aiFulcrumID", "subID", "nSubID", "tagID", "taxonID", "gForm", "mHeight", "stemDiam",
                   "basalMHeight", "basalStemDiam", "pStatus", "shape")]
  })
  
  
  ##  Render iDupPlotData for display
  output$iDupes <- DT::renderDataTable(
    DT::datatable(
      temp <- iDupPlotData(),
      escape = FALSE, filter = "top"
    )
  )
  
  
  
  ###################################################################################################
  ### Create table of individualIDs measured last year that were not measured this year
  ### Use group_by(individualid) %>% distinct(.keep_all) to deal with multi-stemmed sis, sap, etc.
  ### Want recordType, tagID, tagStatus, taxonID, gForm, pStatus fields
  
  
  ### Construct previous eventID with data for that plot based on user-selected plotID
  ##  Determine which events have data for the selected plot
  aiPlotEventQuery <- reactive({
    # Account for null input before user selects required inputs
    shiny::validate(
      need(input$dataPlotChoice != "", "")
    )
    
    # Get plotID from plotData()
    pID <- unique(plotData()$plotID)
    
    # Construct query for eventIDs in which selected plot was measured
    temp <- paste(URLencode('SELECT DISTINCT eventid FROM "VST: Apparent Individuals [PROD]"'),
          URLencode(paste0("WHERE plotid LIKE '", pID, "'")),
          sep = "%20")
    temp
  })
  
  
  ##  Get available past events for selected plot
  aiPastEvents <- reactive({
    # Account for null input before user selects required inputs
    shiny::validate(
      need(input$siteChoice != "" && input$eventChoice != "" && input$dataPlotChoice != "", "")
    )
    
    # Get distinct event years from Fulcrum
    y <- as.numeric(str_extract(sort(unlist(get_Fulcrum_data(api_token = api_token, sql = aiPlotEventQuery())), decreasing = TRUE), "20[0-9]{2}"))
    
    # Define current year based on selected eventID
    userY <- as.numeric(str_extract(input$eventChoice, "20[0-9]{2}"))
    
    # Construct eventIDs from available prior years for user drop-down
    priorY <- y[y < userY]
    priorE <- if(length(priorY) > 0){
      paste("vst", input$siteChoice, priorY, sep = "_")
    } else {
      return("")
    }
    priorE <- data.frame(priorE, stringsAsFactors = FALSE)
    colnames(priorE) <- "priorEvents"
    priorE
  })
  
  
  ##  Render output to prior events drop-down
  output$priorEventSelect <- renderUI({
    selectInput("priorEventChoice", label = NULL, c(Choose = '', choices = aiPastEvents()),
                selectize = TRUE, multiple = FALSE)
  })
  
  
  
  ### Query and download prior plot data when user selects a prior event
  priorPlotData <- reactive({
    # Account for null input before user selects required inputs
    shiny::validate(
      need(input$priorEventChoice != "" && nrow(plotData()) != 0, "")
    )
    
    # Define prior plot event query
    peQuery <- paste(URLencode('SELECT parent._record_id, parent.eventid, parent.plotid, child._child_record_id, child.individualid, child.tagstatus, child.plantstatus, child.growthform FROM "VST: Apparent Individuals [PROD]" AS parent JOIN "VST: Apparent Individuals [PROD]/vst_woody_stems" AS child'),
                     URLencode(paste0("ON (parent._record_id = child._parent_id) WHERE eventid LIKE '", input$priorEventChoice, "'",
                                      " AND plotid LIKE '", unique(plotData()$plotID), "'")),
                     sep = "%20")
    
    # Get prior plot event data
    peData <- get_Fulcrum_data(api_token = api_token, sql = peQuery)
    
    
    ######### Next want to group_by(individualid) and remove duplicates, filter by appropriate subplotid (recreate ddsubplotid), then group_by(individ) for plotData() and diff the two by individualid. Could also join with mapData() to bring in tagID and recordType.
    peData
    
  })
  
  
  
  ###################################################################################################
  ### Temporary output to see intermediate data and text during development
  # Temp text
  output$tempText <- renderText(
    nrow(priorPlotData())
  )
  
  # Temp table
  output$tempTable <- DT::renderDataTable(
    DT::datatable(
      temp <- head(priorPlotData()),
      escape = FALSE, filter = "top"
    )
  )
  
  
  
}) #  End ShinyServer function