### Define UI for Veg Structure Plot Mapper application
navbarPage("NEON VST Mapper", 
           tabPanel("About",
                    fluidRow(
                      column(width=2),
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
                      column(width=2)
                               )
                               ), # End About tabPanel
           
           
           
           tabPanel("Plot Map",
                    sidebarLayout(
                                  sidebarPanel(
                                    # Select domainID from list of domains with VST data
                                    selectInput(
                                      inputId = "domainSelect",
                                      label = "Select a NEON domain:",
                                      c(Choose = '', theDomains),
                                      selectize = TRUE,
                                      multiple = FALSE
                                    ), #  End selectInput
                                    
                                    # Select siteID filtered by selected domainID
                                    uiOutput(outputId = "siteSelect"),
                                    
                                    # After siteChoice selected, conditional panel for selecting plotIDs
                                    conditionalPanel(
                                      condition = "input.siteChoice !=''",
                                      uiOutput(outputId = "plotChoices")
                                    ),
                                    
                                    ##  After plotSelect, conditional panel for graphing options
                                    conditionalPanel(
                                      condition = "input.plotSelect !=''",
                                      # Radio buttons to determine whether taxonid is coded by color or shape
                                      radioButtons("radio", label = "Code taxonID by:",
                                                 choices = list("Color" = "color", "Shape" = "shape")),
                                      
                                      # helpText to explain limitations of coding by shape
                                      helpText("A maximum of 6 species can be displayed when 'Shape' is selected."),
                                      
                                      # Checkboxes to select whether additional components are added to ggplot
                                      checkboxGroupInput("checkGroup", label = "Add data layers:", 
                                                       choices = list("tagIDs" = "tags", "plotMarkers" = "markers")),
                                      
                                      # Download button for .pdf of Plot Map
                                      downloadButton('downloadPlotMap', 'Download (.pdf)', class="btn btn-primary")
                                    ), #  End conditionalPanel
                                   
                                    # Set width of sidebarPanel
                                    width = 3
                                    
                                  ), #  End sidebarPanel
                                  
                                  # Main panel of the page
                                  mainPanel(
                                    # Display plot title
                                    h4(textOutput("plotTitle")),
                                    
                                    # Display ggplot
                                    plotOutput("plotMap")
                                    
                                  ) # End mainPanel
                    ) # End sidebarLayout
            ), #  End Plot Map tabPanel
           
           
           
           tabPanel("Plot Data",
                    fluidRow(
                      column(width = 12, 
                             wellPanel(helpText("Retrieve data from tagged individuals in the plot"), 
                                       downloadButton('downloadData', 'Download (.csv)', class="btn btn-primary")
                             ) # End wellPanel
                      ) # End column
                    ), # End first fluidRow
                    
                    fluidRow(
                      column(width=12, DT::dataTableOutput("table"))
                    ) # End second fluidRow
                    
            ) # End Plot Data tabPanel
           
           
,
theme = shinytheme("cerulean")
) # End navbarPage





