### Development of leaflet map
library(leaflet)
library(plyr)
library(dplyr)

# Set working directory
if (file.exists("~/Documents/neonScienceDocs")){
  setwd("~/Documents/neonScienceDocs/gitRepositories/neonPlantSampling/vstPlotMapperDev")
}

# Need to figure out how to draw polygons from decimalLat and decimalLong data in pointSpatial



# Create base leaflet map
siteMap <- leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery")


siteMap




### Saving plot map to .pdf for download
## Method via ggsave() - this works on the server
output$downloadPlotMap <- downloadHandler(
  filename = function(){
    paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.pdf')
  },
  content = function(file) {
    ggsave(file, width = 6.5, units = "in", device = "pdf")
  }
)

##  Method via rmarkdown::render with .Rmd - this does not work on server yet
output$downloadPlotMap <- downloadHandler(
  filename = function() {
    paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.pdf')
  },
  
  content = function(file) {
    out <- render('plotMap.Rmd', pdf_document(latex_engine='xelatex'), clean = TRUE)
    file.rename(out, file)
  }
)

# Throws error:
"ERROR: cannot open the connection"


##  Method via rmarkdown::render using tempdir() code
output$downloadPlotMap <- downloadHandler(
  filename = function() {
    paste0(paste(unique(plotData()$plotid), unique(plotData()$subplotid), sep = "_"), '.pdf')
  },
  
  content = function(file) {
    src <- normalizePath('plotMap.Rmd')
    
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'plotMap.Rmd', overwrite = TRUE)
    
    out <- rmarkdown::render('plotMap.Rmd', pdf_document(latex_engine='xelatex'), clean = TRUE)
    file.rename(out, file)
  }
)

# Errors with above: 
"When 'latex_engine='xelatex'' not included, throws pandoc error 43"


##  Additional ideas:
# Use 'knit2pdf' function from 'knitr' library, and keep tempdir() code
# Go back to ggsave(), and still create second ggplot object for printing -> this is what I did


