library(shiny)
library(dplyr)

# Define UI for Veg Structure Plot Mapper application

navbarPage("NEON VST Mapper",
           tabPanel("Plot Map",
                    sidebarLayout(
                                  sidebarPanel(
                                    # Indicator of data currency
                                    helpText("Data pulled from Fulcrum on:", br(), "2017-03-03"),
                                    
                                    # Select siteID from a list of sites with mapped individuals
                                    selectInput(
                                      inputId = "siteChoice",
                                      label = "Select a NEON site",
                                      c(Choose = '', sort(unique(vstInput$siteid))),
                                      selectize = TRUE
                                    ), # End selectInput
                                    
                                    # Select plotIDs based on the 'siteChoice' above
                                    uiOutput("plotChoices"),
                                    
                                    # Radio buttons to determine whether taxonid is coded by color or shape
                                    radioButtons("radio", label = "Code taxonID by:",
                                                 choices = list("Color" = "color", "Shape" = "shape")),
                                    
                                    # helpText to explain limitations of coding by shape
                                    helpText("A maximum of 6 species can be displayed when 'Shape' is selected."),
                                    
                                    # Checkbox to select whether tagIDs are added to ggplot
                                    h5("Add data layers:"),
                                    checkboxInput("checkbox", label = "tagIDs", value = FALSE),
                                    
                                    # Download button for .pdf of Plot Map
                                    downloadButton('downloadPlotMap', 'Download (.pdf)'),
                                    
                                    # Set width of sidebarPanel
                                    width = 3
                                    
                                  ), #  End sidebarPanel
                                  
                                  # Main panel of the page
                                  mainPanel(
                                    # Display ggplot
                                    plotOutput("plotMap")
                                    
                                  ) # End mainPanel
                    ) # End sidebarLayout
            ), #  End Plot Map tabPanel
           
           tabPanel("Plot Data",
                    fluidRow(
                      column(width = 12, 
                             wellPanel(helpText("Retrieve data from tagged individuals in the plot"), 
                                       downloadButton('downloadData', 'Download (.csv)')
                                       ) # End wellPanel
                      ) # End column
                    ), # End first fluidRow
                    
                    fluidRow(
                      column(width=10, DT::dataTableOutput("table"))
                    ) # End second fluidRow
                    
            ) # End Plot Data tabPanel

) # End navbarPage





