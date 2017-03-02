library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)

# Define server logic required to create plot map
shinyServer(function(input, output, session) {
  
  ###  Construct menu content for sidebar drop-downs
  # Obtain a list of plots available for the site selected by the user
  thePlots <- reactive({
    vstInput %>% filter(siteid==input$siteChoice) %>% select(plotsubplotid) %>% distinct() %>% arrange(plotsubplotid)
  })
  
  # Populate second drop-down with plot list from above
  output$plotChoices <- renderUI({
    selectInput("plotSelect", "Select a plot to map", c(Choose='', choices = thePlots()),
                selectize = TRUE, multiple = FALSE)
  })
  
  
  ###  Given user input, obtain data for the plot, then filter further to get data for the map, and for table/download
  # Get data for the plot
  plotData <- reactive({
    vstInput %>% filter(plotsubplotid==input$plotSelect)
  })
  
  # Get data for the map
  mapData <- reactive({
    vstInput %>% filter(plotsubplotid==input$plotSelect & pointstatus=="validPointID")
  })
    
  # Get data for download
  tableData <- reactive({
    plotData() %>% select(bouttype, plotid, taxonid, nestedsubplotid, tagid, supportingstemtagid, subplotid, stemeasting, stemnorthing) %>%
      arrange(nestedsubplotid, tagid) -> temp1
    temp1 <- temp1[c("bouttype", "plotid", "subplotid", "nestedsubplotid", "tagid", "supportingstemtagid", "taxonid", "stemeasting", "stemnorthing")]
  })
  
  
  ###  Create and build content for "Plot Map" tab
  ##  Create variables required for building ggplot
  # Determine pointIDs for the map of the plot/subplot using `plotSelect` input from user
  subplotPoints <- reactive({
    temp2 <- if (input$plotSelect==''){
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
    temp3 <- if (nchar(input$plotSelect)==12){
      setdiff(subplotPoints(), 41)
    } else {
      setdiff(subplotPoints(), c(31,33,49,51))
    }
  })
  
  # Extract pointID easting and northing data to add to the map
  currentPlotID <- reactive({unique(plotData()$plotid)})
  mapPoints <- reactive({
    temp4 <- if (input$plotSelect==''){
      return(NULL)
    } else {
      temp4 <- filter(pointSpatial, plotid==currentPlotID() & pointid %in% subplotPoints())
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
    plotData() %>% filter(nestedshrubsapling != "NA" & nestedshrubsapling != "") %>%
      distinct(nestedshrubsapling) -> temp5
    temp5 <- temp5[1,1]
  })
  
  # For caption, obtain `nestedliana` size for selected plotsubplotid, omitting "NA" and "" values
  nestedLiana <- reactive({
    plotData() %>% filter(nestedliana != "NA" & nestedliana != "") %>% distinct(nestedliana) -> temp6
    temp6 <- temp6[1,1]
  })
  
  # For caption, obtain `nestedother` size for selected plotsubplotid, omitting "NA" and "" values
  nestedOther <- reactive({
    plotData() %>% filter(nestedother != "NA" & nestedother != "") %>% distinct(nestedother) -> temp7
    temp7 <- temp7[1,1]
  })
  
  # Build the caption
  captionInput <- reactive({
    paste("", "Nested Subplot Sizes:",
          paste0("nestedSubplotSize (smt + sap + sis + sms) = ", nestedShrubSapling()),
          paste0("nestedSubplotSize (lia) = ", nestedLiana()),
          paste0("nestedSubplotSize (other) = ", nestedOther()),
          sep = "\n")
  })
  
  
  
  ##  Build the ggplot object for display
  ggMap <- reactive({
    # Account for the fact that input is null before user selects a plot to map
    validate(
      need(input$plotSelect != "", "Please select a plot to map")
    )
    # Create the basic ggplot from the mapPoints data
    ggplot(mapPoints(), aes(pointeasting, pointnorthing)) +
      
      # Set the aspect ratio using the mapPoint data
      coord_fixed() +
      
      # Add title, axis labels, and caption to hold nestedSubplotSize data
      labs(title = paste0("Plot Map: ", input$plotSelect),
           x = "Easting (meters)",
           y = "Northing (meters)",
           caption = captionInput()) +
      
      # Remove axis and grid lines from panel
      theme(panel.grid = element_blank()) +
      theme(title = element_text(size=14)) +
      theme(axis.text = element_text(size = 12)) +
      theme(axis.title = element_text(size = 14, face = "bold")) +
      theme(legend.text = element_text(size=12)) +
      
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
    if (input$checkbox) p = p + geom_text(data=mapData(), aes(x=stemeasting, y=stemnorthing, label=tagid), size=4, vjust=-1)
    p
    }, height = 900)
  
  
  ##  Create .pdf download output from ggplot
  output$downloadPlotMap <- downloadHandler(
    filename = function(){
      paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.pdf')
    },
    content = function(file) {
      ggsave(file, width = 6.5, units = "in", device = "pdf")
    }
  )
  
  
  
  ###  Create content for Data Table tab
  # Create download output
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.csv') 
    },
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    }
  )
  
  # Create output table from tableData reactive variable
  output$table <- DT::renderDataTable(
    DT::datatable({
      tableData()
    })
  )
  

  
  
  
  
})
