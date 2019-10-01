### Define UI for Veg Structure Plot Mapper application
navbarPage("NEON VST QC v2.0", 
           tabPanel("About",
                    fluidRow(
                      # First column
                      column(width=3,
                             img(src = "treeSilhouette.png", align = "left")
                             ),
                      # Second column
                      column(width=8,
                             wellPanel(
                               h4("General"),
                               p("The NEON Vegetation Structure Plot Mapper actively queries location data for mapped and tagged woody stems from the Fulcrum
                                 '(TOS) VST: Mapping and Tagging [PROD]' table on a site by site basis once a user selects a site to map. This means
                                 that data can be mapped and downloaded as soon as records have been finalized in Fulcrum. The app also utilizes GPS data
                                 collected during Plot Establishment to inform the actual plot boundaries as they exist on the ground."),
                               br(),
                               h4("The 'Plot Map' Tab"),
                               p("The 'Plot Map' tab provides a graphical interface to VST Mapping and Tagging data, allowing one to quickly find where mapped
                                 individuals exist within the selected plot. Once the user selects a site and plot to map, the data from that plot are displayed in
                                 the main panel of the application. On the map itself, the caption at the lower right summarizes nested subplot sizes currently
                                 employed in the plot. When mapped individuals fall outside the established plot boundary, the most common cause is incorrect data 
                                 entry in the `pointid` field, and this graphical representation of the plot makes it very quick to detect this error."),
                               h5("Displaying the taxonID"),
                               p("The default behavior is to display mapped individuals color coded by taxonID. For printing purposes, or cases where color is not desired,
                                 the user can optionally elect to display individuals with a different shape for up to 6 taxonIDs. However, due to the limited number of 
                                 available shapes, the map can only display a maximum number of 6 species when 'Code taxonID by shape' is selected. Additional species will
                                 still exist in the data, but will not be shown on the map."),
                               h5("Add Data Layers"),
                               p("Clicking the 'tagIDs' checkbox allows the user to toggle display of tagIDs associated with each mapped individual in the plot. To unclutter
                                 the plot map, leading zeroes have been stripped from the tagIDs. When encoding taxonID by shape, tagIDs will still appear on the map even
                                 if there are insufficient shapes available to indicate the location of the individual with a symbol."),
                               h5("Downloading the Map"),
                               p("Clicking the 'Download (.pdf)' button will create a .pdf file named according to plotID and subplotID. Printed maps may be useful in the
                                 field to aid in locating previously mapped and tagged individuals for repeat measurements."),
                               br(),
                               h4("The 'Plot Data' Tab"),
                               p("Selected fields are displayed for each mapped and/or tagged individual in the plot in the 'Plot Data' table. By default, data are sorted
                                 hierarchically first by `nestedsubpotid` and then by `tagID`, in order to aid in finding tagged individuals within the plot."),
                               h5("Field Definitions"),
                               tags$div(
                                 tags$ul(
                                   tags$li(tags$strong("pointid:"), "The pointID within the plot that was used to map a given woody individual when 'stemDistance' and
                                           'stemAzimuth' were originally recorded."),
                                   tags$li(tags$strong("pointstatus:"), "his field shows 'notMapped' if a pointID was not entered into Fulcrum, displays 'validPointID' if the pointID chosen for mapping corresponds to a valid pointID with high-resolution GPS data, and reports 'errorPointID' in the event that a pointID was chosen for mapping that does not have high-resolution GPS data associated with it. Only records with a 'validPointID' are mapped in the 'Plot Map' tab."),
                                   tags$li(tags$strong("offseteasting:"), "The east/west distance, in meters, between the mapped woody individual and the marker at the SW corner of the plot (20m x 20m plots) or subplot (40m x 40m plots)."),
                                   tags$li(tags$strong("offsetnorthing:"), "The north/south distance, in meters, between the mapped woody individual and the marker at the SW corner of the plot (20m x 20m plots) or subplot (40m x 40m plots).")
                                 ) #  End tags$ul
                               ), #  End tags$div
                               
                               h5("Downloading the Data"),
                               p("Clicking the 'Download (.csv)' button allows plot-level data to be readily organized into a checklist when working in the field.")

                               )),
                      # Third column - empty
                      column(width=1)
                               )
                               ), # End About tabPanel
           
           
           
           tabPanel("Plot Map",
                    sidebarLayout(
                                  sidebarPanel(
                                    # Select domainID from list of domains with VST data
                                    selectInput(
                                      inputId = "mapDomainChoice",
                                      label = "Select a NEON domain:",
                                      c(Choose = '', theDomains),
                                      selectize = TRUE,
                                      multiple = FALSE
                                    ), #  End selectInput
                                    
                                    # Select siteID filtered by selected domainID
                                    uiOutput(outputId = "siteSelect"),
                                    
                                    # After user selects site, conditional panel for selecting eventID
                                    conditionalPanel(
                                      condition = "input.siteChoice !=''",
                                      uiOutput(outputId = "eventSelect")
                                    ),
                                    
                                    # After user selects eventID, conditional panel for selecting plotID
                                    conditionalPanel(
                                      condition = "input.siteChoice !='' && input.eventChoice !=''",
                                      uiOutput(outputId = "plotSelect")
                                    ),
                                    
                                    ##  After plotSelect, conditional panel for graphing options
                                    conditionalPanel(
                                      condition = "(typeof input.siteChoice !== 'undefined' && input.siteChoice !== '' &&
                                      input.eventChoice !== 'undefined' && input.eventChoice !== '' &&
                                      input.plotChoice !== 'undefined' && input.plotChoice !== '')",
                                      
                                      # Radio buttons to determine whether taxonid is coded by color or shape
                                      radioButtons("taxonRadio", label = "Code taxonID by:",
                                                 choices = list("Color" = "color", "Shape" = "shape")),
                                      
                                      # helpText to explain limitations of coding by shape
                                      helpText("A maximum of 6 species can be displayed when 'Shape' is selected."),
                                      
                                      
                                      ##  wellPanel for download options
                                      wellPanel(
                                        # Checkboxes to select whether additional components are added to downloaded ggplot
                                        checkboxGroupInput("labelChecks", label = "PDF data layers:",
                                                           choices = list("tagIDs" = "tags", "plotMarkers" = "markers")),
                                        
                                        # Download button for .pdf of Plot Map
                                        downloadButton('downloadPlotMap', 'Download (.pdf)', class="btn btn-primary")
                                      )
                                      
                                    ), #  End conditionalPanel
                                   
                                    # Set width of sidebarPanel
                                    width = 3
                                    
                                  ), #  End sidebarPanel
                                  
                                  # Main panel of the page
                                  mainPanel(
                                    # First fluidRow for Plot Title and Warnings
                                    fluidRow(
                                      wellPanel(
                                        h4(textOutput("plotTitle")),
                                        textOutput("mapPointError"),
                                        textOutput("mapDataWarning"),
                                        tags$head(tags$style("#mapPointError{color:#C40000}",
                                                           "#mapDataWarning{color:#C40000}"))
                                        ) # End wellPanel
                                    ), #  End fluidRow
                                    
                                    # Second fluidRow for plotMap content
                                    fluidRow(style = "height:900px;",
                                      plotlyOutput("mapPlot")
                                    ), #  End fluidRow
                                    
                                    # Third fluidRow for table to display nested subplot sizes
                                    fluidRow(style = "padding-top:10px;",
                                    conditionalPanel(
                                      condition = "(typeof input.siteChoice !== 'undefined' && input.siteChoice !== '' &&
                                      input.eventChoice !== 'undefined' && input.eventChoice !== '' &&
                                      input.plotChoice !== 'undefined' && input.plotChoice !== '')",
                                      wellPanel(
                                        h5("Nested Subplot Sizes for Selected Plot:"),
                                        tableOutput("nestedTable")
                                      )
                                    )
                                    ), # End fluidRow
                                  width = 8) # End mainPanel
                    ) # End sidebarLayout
            ), #  End Plot Map tabPanel
           
           
           
           tabPanel("Plot Data",
                    # Plot-select controls wellPanel
                    wellPanel(
                      fluidRow(
                        column(width = 3, h5("Plot Select:")),
                        column(width = 3, h5("Selected VST event:")),
                        column(width = 3, h5("Selected NEON site:")),
                        column(width = 3, h5("Selected NEON domain:"))
                      ),
                      fluidRow(
                        column(width = 3, uiOutput("dataPlotSelect")),
                        column(width = 3, verbatimTextOutput("dataEventSelect")),
                        column(width = 3, verbatimTextOutput("dataSiteSelect")),
                        column(width = 3, verbatimTextOutput("dataDomainSelect"))
                      )
                    ),
                    
                      
                    # tabPanel for 'tabs' content
                    tabsetPanel(
                          ### Data Table tab pane
                          tabPanel("Data Table",
                                   br(),
                                   h4("Data Table Filters"),
                                   fluidRow(
                                     column(width = 2, h5("Record Type:")),
                                     column(width = 2, h5("Tag Status:")),
                                     column(width = 2, h5("Growth Form:")),
                                     column(width = 2, h5("Plant Status:")),
                                     column(width = 2, h5("Download filtered data:"))
                                   ),
                                   fluidRow(
                                     column(width = 2, uiOutput(outputId = "recordTypeSelect")),
                                     column(width = 2, uiOutput(outputId = "tagStatusSelect")),
                                     column(width = 2, uiOutput(outputId = "growthFormSelect")),
                                     column(width = 2, uiOutput(outputId = "plantStatusSelect")),
                                     column(width = 2, downloadButton('downloadDataTable', 'Download (.csv)', class="btn btn-primary"))
                                   ), #  End second fluidRow
                                   
                                   br(),
                                   hr(),
                                   # Filtered Data Table output
                                   DT::dataTableOutput("filterDataTable")
                                   
                                   ),
                          
                                   
                          
                          ### QC Tables tab pane
                          tabPanel("QC Tables",
                                   br(),
                                   h4("QC Table: Strict Duplicates"),
                                   helpText("The strict duplicate table identifies records that are an exact match of another record
                                            in all data fields."),
                                   
                                   #h5("Temp Table"),
                                   # Temp Table output
                                   DT::dataTableOutput("tempTable"),
                                   
                                   h5("Temp Text"),
                                   verbatimTextOutput("tempText")
                                   ),
                          
                          
                          
                          ### QC Plots tab pane
                          tabPanel("QC Plots",
                                   br(),
                                   h4("QC Plots test"),
                                   
                                   #  Temp Text output
                                   h5("Temp text"),
                                   verbatimTextOutput("tempText"),
                                   
                                   h5("Temp Table"),
                                   # Temp Table output
                                   DT::dataTableOutput("tempTable")
                                   
                                   )
                        )
                        
                        
                        
                        
                      
                    
            ) # End Plot Data tabPanel
           
           
,
theme = shinytheme("cerulean")
) # End navbarPage





