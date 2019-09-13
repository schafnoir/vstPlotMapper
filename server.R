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
    
    # Get distinct events for site from Fulcrum
    temp <- get_Fulcrum_data(api_token = api_token, sql = aiEventQuery())
    # Fulcrum returns a list; convert to a tibble if > 1 eventID to easily show values in drop-down
    temp <- tibble(unlist(temp))
    # Create a sensible drop-down label, and sort values with latest eventID on top
    temp <- temp %>% rename(eventID = `unlist(temp)`) %>% arrange(desc(eventID))
  })
  
  ##  Render output to eventID dropdown
  output$eventSelect <- renderUI({
    selectInput("eventChoice", "Select a VST event:", c(Choose = '', choices = aiEvents()),
                  selectize = TRUE, multiple = FALSE)
  })
  
  
  
  ### Construct Fulcrum query for plotIDs based on user-selected siteID and eventID and pipe to plotID drop-down
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
  
  
  ##  Retrieve plot list from Apparent Individuals for selected siteID x eventID combination
  aiPlots <- reactive({
    # Account for null input before user selects an eventID
    shiny::validate(
      need(input$eventChoice != "", "")
    )
    # Get distinct plots for site from Fulcrum
    temp <- get_Fulcrum_data(api_token = api_token, sql = aiPlotQuery())
  })
  ####### The above works but need to know which subplots were measured as well; use M&T based approach as was previously done but do it using the AI data instead... May need to join the AI data with M&T in order to get subplotIDs for selected eventID. Many child records in AI appear to have no subplotID. Prep entire dataset necessary for use, and then make plotID:subplotID list in dropdown...
  
  
  ##  Temporary output to see intermediate data and text
  # Temp text
  output$tempText <- renderText(
    input$eventChoice
  )
  
  # Temp table
  output$tempTable <- renderTable(
    aiPlots()
  )
  
  
  
})
