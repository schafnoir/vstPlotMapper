### Define server logic required to create plot map
shinyServer(function(input, output, session) {
  
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
  
  
  
  ### Query data from Fulcrum based on site selected by user
  # Construct Fulcrum query
  dataQuery <- reactive({
    paste(URLencode('SELECT nestedshrubsapling, nestedliana, nestedother, bouttype, plotid, siteid, taxonid,
             subplotid, nestedsubplotid, tagid, supportingstemtagid, pointid, stemdistance, stemazimuth,
             _record_id, load_status FROM "(TOS) VST: Mapping and Tagging [PROD]"'),
          URLencode(paste0("WHERE siteid LIKE '", input$siteChoice, "'")),
          sep = "%20")
  })
  
  # Query Fulcrum to obtain site data
  fulcrumData <- reactive({
    temp <- if (input$siteChoice==''){
      return(NULL)
    } else {
    get_Fulcrum_data(api_token = api_token, sql = dataQuery())
    }
  })
  
  
  ### Filter and mutate `fulcrumData` to generate columns required for plot maps and data download
  siteData <- reactive({
    temp <- if (input$siteChoice==''){
      return(NULL)
    } else {
    # Select needed columns from `fulcrumData`
    sd <- fulcrumData() 
    
    # Join with 'plotSpatial', and only keep records with matching plotid
    sd <- inner_join(sd, plotSpatial, by = "plotid")
    
    sd %>%
      # Some subplotIDs were assigned at the 10m x 10m level: Assign new `subplotid` based on plotType
      mutate(newsubplotid = ifelse(plottype!="lgTower", 31,
                                   ifelse(subplotid %in% c(21,22,30,31), 21,
                                          ifelse(subplotid %in% c(23,24,32,33), 23,
                                                 ifelse(subplotid %in% c(39,40,48,49), 39,
                                                        ifelse(subplotid %in% c(41,42,50,51), 41,
                                                               "NA")))))) %>%
      select(-subplotid) %>%
      rename(subplotid=newsubplotid) %>%
      
      # Remove instances where `subplotid` is 'NA'
      filter(!subplotid=="NA") %>%
      
      # Create 'plotsubplotid' to pipe to plotChoice drop-down: e.g., "(T) BART_050: 21"
      mutate(plotsubplotid = ifelse(plottype=="distributed", paste0("(D) ", plotid),
                                    ifelse(plottype=="smTower", paste0("(T) ", plotid),
                                           paste0("(T) ", plotid, ": ", subplotid)))) %>%
      # Create `pointstatus` variable to detect invalid pointid values based on plottype
      mutate(pointstatus = ifelse(is.na(pointid), "notMapped",
                                  ifelse(plottype=="lgTower" & pointid %in% expLarge, "validPointID",
                                         ifelse(pointid %in% expSmall, "validPointID",
                                                "errorPointID")))) %>%
      # Create 'plotpointid' variable for valid pointids only, to enable join with 'pointSpatial' data table
      mutate(plotpointid = ifelse(is.na(pointid), "NA", 
                                  ifelse(pointstatus=="validPointID", paste(plotid, pointid, sep="_"),
                                         "NA"))) -> sd
      
    # Join to create `pointeasting` and `pointnorthing`
    sd$pointid <- as.integer(sd$pointid)
    sd <- left_join(sd, pointSpatial)
    
    # Calculate 'stemeasting' and 'stemnorthing', and sort final dataset
    sd %>%
      mutate(stemeasting = round(pointeasting + stemdistance*sin(radians(stemazimuth)), digits = 2)) %>%
      mutate(stemnorthing = round(pointnorthing + stemdistance*cos(radians(stemazimuth)), digits = 2)) %>%
      arrange(plotid, subplotid) -> sd
    
    sd
    }
  })
  
  
  
  ###  Construct menu content for additional sidebar drop-downs
  # Obtain a list of plots available for the site selected by the user
  thePlots <- reactive({
    temp <- if (input$siteChoice==''){
      return(NULL)
    } else {
    siteData() %>% select(plotsubplotid) %>% distinct() %>% arrange(plotsubplotid)
    }
  })
  
  # Populate second drop-down with plot list from above
  output$plotChoices <- renderUI({
    selectInput("plotSelect", "Select a plot to map:", c(Choose='', choices = thePlots()),
                selectize = TRUE, multiple = FALSE)
 })
  
  
  
  ### Given user input, obtain plot data, then filter to get data for the map
  # Get data for the plot
  plotData <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      siteData() %>% filter(plotsubplotid==input$plotSelect)
    }
  })
  
  # Get data for the map
  mapData <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      md <- plotData() %>% filter(pointstatus=="validPointID")
      # Remove leading zeroes from tagID field
      md$tagid <- substr(md$tagid, regexpr("[^0]", md$tagid), nchar(md$tagid))
      md
    }
  })
  
  
  
  
  
  ### Create and build content for "Plot Map" tab
  ##  Create variables required for building ggplot
  # Determine pointIDs for the map of the plot/subplot using `plotSelect` input from user
  subplotPoints <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else if (nchar(input$plotSelect)==12){
      c(31,33,41,49,51)
    } else if (str_sub(input$plotSelect, start=-2, end=-1)==21){
      c(21,23,31,39,41)
    } else if (str_sub(input$plotSelect, start=-2, end=-1)==23){
      c(23,25,33,41,43)
    } else if (str_sub(input$plotSelect, start=-2, end=-1)==39){
      c(39,41,49,57,59)
    } else {
      c(41,43,51,59,61)
    }
  })
  
  # Determine pointIDs for mapping the corners of the plot/subplot using `plotSelect` input from user
  cornerPoints <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else if (nchar(input$plotSelect)==12){
      setdiff(subplotPoints(), 41)
    } else {
      setdiff(subplotPoints(), c(31,33,49,51))
    }
  })
  
  # Extract pointID easting and northing data to add to the map
  currentPlotID <- reactive({unique(plotData()$plotid)})
  mapPoints <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      filter(pointSpatial, plotid==currentPlotID() & pointid %in% subplotPoints())
    }
  })
  
  # Isolate easting and northing coordinates required for making geom_segments that draw plot perimeter
  E1 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[1]])})
  N1 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[1]])})
  E2 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[2]])})
  N2 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[2]])})
  E3 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[3]])})
  N3 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[3]])})
  E4 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointeasting[mapPoints()$pointid==cornerPoints()[4]])})
  N4 <- reactive({ifelse(input$plotSelect=='', return(NULL), mapPoints()$pointnorthing[mapPoints()$pointid==cornerPoints()[4]])})
  
  # For caption, obtain `nestedshrubsapling` size for selected plotsubplotid, omitting "NA" and "" values
  nestedShrubSapling <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      nss <- plotData() %>%
        filter(nestedshrubsapling != "NA" & nestedshrubsapling != "") %>%
        distinct(nestedshrubsapling)
      nss <- nss[1,1]
    }
  })
  
  # For caption, obtain `nestedliana` size for selected plotsubplotid, omitting "NA" and "" values
  nestedLiana <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      nl <- plotData() %>%
        filter(nestedliana != "NA" & nestedliana != "") %>% 
        distinct(nestedliana)
      nl <- nl[1,1]
    }
  })
  
  # For caption, obtain `nestedother` size for selected plotsubplotid, omitting "NA" and "" values
  nestedOther <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      no <- plotData() %>% 
        filter(nestedother != "NA" & nestedother != "") %>% 
        distinct(nestedother)
      no <- no[1,1]
    }
  })
  
  # Build the caption
  captionInput <- reactive({
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      paste("", "Nested Subplot Sizes:",
          paste0("nestedSubplotSize (smt + sap + sis + sms) = ", nestedShrubSapling()),
          paste0("nestedSubplotSize (lia) = ", nestedLiana()),
          paste0("nestedSubplotSize (other) = ", nestedOther()),
          sep = "\n")
    }
  })
  
  ##  Build plot title
  output$plotTitle <- renderText({
    shiny::validate(
      need(input$plotSelect != "", "Please select a plot to map")
    )
    paste0("Plot Map: ", input$plotSelect)
  })
  
  ##  Build the ggplot object for display
  ggMap <- reactive({
    # Account for the fact that input is null before user selects a plot to map
    shiny::validate(
      need(input$plotSelect != "", "")
    )
    # Create the basic ggplot from the mapPoints data
    ggplot(mapPoints(), aes(pointeasting, pointnorthing)) +
      
      # Set the aspect ratio using the mapPoint data
      coord_fixed() +
      
      # Add title, axis labels, and caption to hold nestedSubplotSize data
      labs(x = "Easting (meters)",
           y = "Northing (meters)",
           caption = captionInput()) +
      
      # Remove axis and grid lines from panel
      theme(panel.grid = element_blank()) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 14, face = "bold")) +
      theme(legend.text = element_text(size=12)) +
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
      geom_point(size=3, shape=21, colour="black", fill="red", stroke=1, show.legend = FALSE) 
    
  })
  
  # Render the ggplot to output, and include plot modifications from user input
  output$plotMap <- renderPlot({
    p = ggMap()
    if (input$radio=="color") p = p + geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, color = taxonid),
                                                 size = 2.75, shape = 21, stroke = 0.8, show.legend = TRUE)
    if (input$radio=="shape") p = p + geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, shape = taxonid),
                                                 size = 2.75, stroke = 0.8, show.legend = TRUE) +
        scale_shape_discrete(solid = FALSE)
    if ("tags" %in% input$checkGroup) p = p + geom_text_repel(data=mapData(), aes(x=stemeasting, y=stemnorthing, label=tagid), size=4, 
                                                              nudge_x = 0.3, nudge_y = 0.3)
    if ("markers" %in% input$checkGroup) p = p + geom_label(aes(label = pointid), fill = "#FF6C5E", show.legend = FALSE)
    p
  }, height = 900)
  
  
  
  ###  Create .pdf download output from ggplot
  ##  Build the ggplot object for download and printing
  ggDownload <- reactive({
    ggplot(mapPoints(), aes(pointeasting, pointnorthing)) +
    # Set the aspect ratio using the mapPoint data
    coord_fixed() +
    
    # Add title, axis labels, and caption to hold nestedSubplotSize data
    labs(title = paste0("Plot Map: ", input$plotSelect),
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
    if (input$radio=="color") p = p + 
    geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, color = taxonid), size = 2, shape = 21, stroke = 0.5, show.legend = TRUE)
  
    if (input$radio=="shape") p = p +
    geom_point(data = mapData(), aes(x = stemeasting, y = stemnorthing, shape = taxonid), size = 2, stroke = 0.5, show.legend = TRUE) +
    scale_shape_discrete(solid = FALSE)
  
    if ("tags" %in% input$checkGroup) p = p + 
    geom_text_repel(data=mapData(), aes(x=stemeasting, y=stemnorthing, label=tagid), size=2, nudge_x = 0.3, nudge_y = 0.3)
  
    if ("markers" %in% input$checkGroup) p = p + geom_label(aes(label = pointid), fill = "#FF6C5E", colour="white", size=2, show.legend = FALSE)
    p
  })
  
  
  ##  Pipe ggDownload output to .pdf file for local saving
  output$downloadPlotMap <- downloadHandler(
    filename = function(){
      paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.pdf')
    },
    content = function(file) {
      ggsave(file, plot = ggFinalDownload(), width = 6.5, units = "in", device = "pdf")
    }
  )
  
  
  
  ###  Create content for Plot Data tab
  # Get data for the table based on user 'plotSelect' input
  tableData <- reactive({
    shiny::validate(
      need(input$plotSelect != "", "Please select a plot")
    )
    
    temp <- if (input$plotSelect==''){
      return(NULL)
    } else {
      # Calculate `offseteasting` and `offsetnorthing`
      td <- plotData() %>%
        mutate(offseteasting = round(stemeasting - E1(), digits = 1)) %>%
        mutate(offsetnorthing = round(stemnorthing - N1(), digits = 1)) %>%
        rename(fulcrum_id=`_record_id`)
      # Replace `fulcrum_id` field values with new 'Edit Record' button
      td$fulcrum_id <- paste0("https://web.fulcrumapp.com/records/", td$fulcrum_id)
      td$fulcrum_id[td$load_status=="NONE"] <- paste0('<a href="',td$fulcrum_id[td$load_status=="NONE"],'"',' target="_blank" class="btn btn-primary">Edit Record</a>')
      td$fulcrum_id[td$load_status!="NONE"] <- paste0('<a href="',td$fulcrum_id[td$load_status!="NONE"],'"',' target="_blank" class="btn btn-info">View Loaded Record</a>')
      # Select fields and arrange rows
      td <- td %>%
        select(fulcrum_id, bouttype, plotid, taxonid, nestedsubplotid, tagid, supportingstemtagid, subplotid, pointid,
               pointstatus, offseteasting, offsetnorthing) %>%
        arrange(nestedsubplotid, tagid)
      td <- td[c("fulcrum_id", "bouttype", "plotid", "subplotid", "nestedsubplotid", "tagid", "supportingstemtagid", 
                 "taxonid", "pointid", "pointstatus", "offseteasting", "offsetnorthing")]
    }
  })
  
  # Create output table from tableData reactive variable
  output$table <- DT::renderDataTable(
    DT::datatable(
      tableData(), escape = FALSE, filter = "top"
    )
  )
  
  # Create download .csv
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.csv') 
    },
    content = function(file) {
      td = tableData() %>% select(-fulcrum_id)
      write.csv(td, file, row.names = FALSE)
    }
  )
  
  
  
  
})
