library(shiny)
# library(shinyapps)

shinyUI(pageWithSidebar(
  headerPanel("Locality label generator - for use by Field Ops"),
  sidebarPanel(
    h4("Note: Session will time out after 5 minutes"),
    selectInput(inputId = 'site', label = 'Select your site', 
                                 choices = sort(unique(points$siteID)), selected = NULL),
    dateInput('dateStart',
              label = 'When is the FIRST collection date? (format: yyyy-mm-dd)',
              value = Sys.Date()
    ),
    dateInput('dateEnd',
                   label = 'What is the LAST collection date? (format: yyyy-mm-dd)',
                   value = Sys.Date() + 90
    ),
   selectInput('purpose', label = 'These labels are for:', 
               choices = c("field collection", "sorting or pinning"), selected = NULL),
   conditionalPanel(condition = "input.purpose == 'sorting or pinning'",
                    numericInput('numLabels', label= "How many copies should be made of each label",value = 3)#,
                    #checkboxGroupInput('traps', label = "Which traps should be printed?",choices = "trapIDs")
                    )
#    checkboxInput(inputId = 'preprint' , label = 'Should these labels be fully pre-printed?', value = TRUE, width = NULL)
      ),
    mainPanel(
      p("Select the site. Labels will be produced on a 14 day schedule beginning with the 1st collection date through the end date provided.
        This app will generate labels for each trap that exists at a site."),
      p("By default, if labels are for field collection, then 3 labels per trap will be produced for the site and date range specified (40 traps x 3 copies = 120 labels). Pages 
        print 8 x 30 and can fit 240 labels (2 field collection bouts). If these labels are for sorting/pinning, the user specifies the number of duplicates of each label."),
      p("Below is an example of what the first label on the sheet will look like."),
      verbatimTextOutput("label"),
      p("Note: Let HQ know if plots being sampled are missing from this application."),
      br(),
      wellPanel(
        h3("Download locality labels"),
        downloadButton("locality", label="Download Labels")
      )
      )
  )
  )
