library(knitr)

shinyServer(function(input, output, session) {
  
  # set options
  options(stringsAsFactors=F, strip.white=T)

  # which site is it?
  site.react <- reactive({
    site.in <- input$site
    return(site.in)
  })  
  
  # what is the first date?
  dateStart.react <- reactive({
    dateStart.in <- input$dateStart
    return(dateStart.in)
  })  
  
  # what is the range of dates?
  dateRange.react <- reactive({
    dateEnd.in <- input$dateEnd
    numBouts <- trunc(difftime(dateEnd.in, dateStart.react(), units = "days")/14, 0)
    numBouts = numBouts + 1
    
    collDates <- seq.Date(dateStart.react(), by = "14 days", length.out = numBouts)
    return(collDates)
  })  
  
  numLabels <- reactive({
    purpose <- input$purpose
    if(purpose=="field collection"){
      numLabels <- 3
    } else{
      numLabels <-  input$numLabels   
    }
    return(numLabels)
  })
   
  traps.react <- reactive({
    trapList <- input$traps
    return(trapList)
  })
  
  # Make the locality label
  locality.label <- reactive({
#    if(input$preprint==TRUE ){
    site.data <- unique.data.frame(points[grepl(site.react(), points$siteID), ])
    data.set <- data.frame()
    for (i in 1:dim(site.data)[1]){
      for(j in 1:numLabels()){
        data.set <- rbind(data.set,
                          site.data[i, ])
      }
    }
    site.data <- data.set
    rm(data.set)
    locality.label <- data.frame()
    
    for (i in 1:length(dateRange.react())){
      locality.label <- rbind(locality.label,
                              data.frame(site.data, date = dateRange.react()[i]))
    }
    
    for (i in 1:dim(locality.label)[1]){
      locality.label$collDate[i] <- paste0(strsplit(as.character(locality.label$date[i]), split = "-")[[1]][3],
                                          month.abb[as.numeric(strsplit(as.character(locality.label$date[i]), 
                                                                        split = "-")[[1]][2])],
                                          strsplit(as.character(locality.label$date[i]), split = "-")[[1]][1])
      locality.label$sampleID[i] <- paste(paste0(substr(locality.label$plotID[i],1,4), "\\_",
                                                 substr(locality.label$plotID[i],6,8)), 
                                          locality.label$pointID[i],
                                          paste0(strsplit(as.character(locality.label$date[i]), split = "-")[[1]][1],
                                                 strsplit(as.character(locality.label$date[i]), split = "-")[[1]][2],
                                                 strsplit(as.character(locality.label$date[i]), split = "-")[[1]][3]),
                                          sep=".")
    }
    locality.label$trapID <- paste(locality.label$plotID, locality.label$trapID, sep=".")
    
    locality.label$line1 <- paste0(locality.label$country, ", ", locality.label$stateProvince, " ",
                                   locality.label$county, " Co.")
    locality.label$line2 <- paste0(locality.label$siteName, ".")
    locality.label$line3 <- paste0(locality.label$elevation, "m N", locality.label$decimalLatitude, "\\hspace{1pt}$\\cdot$ W",
                                   abs(locality.label$decimalLongitude))
    locality.label$line4 <- paste("Pitfall trap", locality.label$collDate, 
                                   paste0(locality.label$collector,"."), sep=". ")
    locality.label$line5 <- paste("NEON", locality.label$sampleID, "") 
    
    # Exceptions
    locality.label$line1[substr(locality.label$plotID,1,4)=="DEJU"] <- "USA, ALASKA Southeast Fairbanks"
    locality.label$line2[substr(locality.label$plotID,1,4)=="DEJU"] <- "Census Area. Delta Junction."
    locality.label$line1[substr(locality.label$plotID,1,4)=="HEAL"] <- "USA, ALASKA Denali Borough. Healy."
    locality.label$line1[substr(locality.label$plotID,1,4)=="HARV"] <- "USA, MASSACHUSETTS"
    locality.label$line2[substr(locality.label$plotID,1,4)=="HARV"] <- "Worcester Co. Harvard Forest."
    locality.label$line1[substr(locality.label$plotID,1,4)=="LAJA"] <- "USA, PUERTO RICO Lajas"
    locality.label$line2[substr(locality.label$plotID,1,4)=="LAJA"] <- "Municipio Co. Lajas Expm Stn." 
    locality.label$line1[substr(locality.label$plotID,1,4)=="MOAB"] <- "USA, UTAH San Juan Co. Moab."
    locality.label$line1[substr(locality.label$plotID,1,4)=="WOOD"] <- "USA, NORTH DAKOTA Stutsman"
    locality.label$line2[substr(locality.label$plotID,1,4)=="WOOD"] <- "Co. Woodworth."
    locality.label$line1[substr(locality.label$plotID,1,4)=="SERC"] <- "USA, MARYLAND Anne Arundel"
    locality.label$line2[substr(locality.label$plotID,1,4)=="SERC"] <- "Co. Smithsonian Env Res Ctr." 
    locality.label$line1[substr(locality.label$plotID,1,4)=="GUAN"] <- substr(locality.label$line1[substr(locality.label$plotID,1,4)=="GUAN"], 1,
                                                                              nchar(locality.label$line1[substr(locality.label$plotID,1,4)=="GUAN"]) - 14)
    locality.label$line2[substr(locality.label$plotID,1,4)=="GUAN"] <- "Municipio Co. Guanica Forest." 
    locality.label$line2[locality.label$domainID=="D05"] <- paste(paste0(locality.label$siteName[locality.label$domainID=="D05"], "."),
                                                                  paste0(locality.label$elevation[locality.label$domainID=="D05"], "m"), sep=" ")
    locality.label$line3[locality.label$domainID=="D05"] <- paste0("N", locality.label$decimalLatitude[locality.label$domainID=="D05"], "\\hspace{1pt}$\\cdot$ W",
                                                                   abs(locality.label$decimalLongitude[locality.label$domainID=="D05"]),
                                                                   " Pitfall")
    locality.label$line4[locality.label$domainID=="D05"] <- paste("trap", locality.label$collDate[locality.label$domainID=="D05"], 
                                                                  paste0(locality.label$collector[locality.label$domainID=="D05"],"."), sep=". ")
 
    # Final formatting
    locality.label$label <- paste(locality.label$line1,"\n",
                                  locality.label$line2,"\n",
                                  locality.label$line3,"\n",
                                  locality.label$line4,"\n",
                                  locality.label$line5,"\n")
    locality.label$label2 <- paste(locality.label$line1,"\n",
                                  locality.label$line2,"\n",
                                  paste0(locality.label$elevation, "m N", locality.label$decimalLatitude, " W",
                                         abs(locality.label$decimalLongitude)),"\n",
                                  locality.label$line4,"\n",
                                  paste("NEON", paste(locality.label$plotID,substr(locality.label$sampleID, 11,20),sep="."), ""),"\n")
    
    # Exception handling
    locality.label$label[substr(locality.label$plotID,1,4)=="HEAL"] <- paste(locality.label$line1[substr(locality.label$plotID,1,4)=="HEAL"],"\n",
                                                                             locality.label$line3[substr(locality.label$plotID,1,4)=="HEAL"],"\n",
                                                                             locality.label$line4[substr(locality.label$plotID,1,4)=="HEAL"],"\n",
                                                                             locality.label$line5[substr(locality.label$plotID,1,4)=="HEAL"],"\n")
    locality.label$label2[substr(locality.label$plotID,1,4)=="HEAL"] <- paste(locality.label$line1[substr(locality.label$plotID,1,4)=="HEAL"],"\n",
                                                                              paste0(locality.label$elevation[substr(locality.label$plotID,1,4)=="HEAL"], "m N", 
                                                                                     locality.label$decimalLatitude[substr(locality.label$plotID,1,4)=="HEAL"], " W",
                                                                                     abs(locality.label$decimalLongitude[substr(locality.label$plotID,1,4)=="HEAL"])),"\n",
                                                                              locality.label$line4[substr(locality.label$plotID,1,4)=="HEAL"],"\n",
                                                                              paste("NEON", paste(locality.label$plotID[substr(locality.label$plotID,1,4)=="HEAL"],
                                                                                                  substr(locality.label$sampleID[substr(locality.label$plotID,1,4)=="HEAL"], 11,20),sep="."), ""),"\n")
    
    locality.label$label[substr(locality.label$plotID,1,4)=="MOAB"] <- paste(locality.label$line1[substr(locality.label$plotID,1,4)=="MOAB"],"\n",
                                                                             locality.label$line3[substr(locality.label$plotID,1,4)=="MOAB"],"\n",
                                                                             locality.label$line4[substr(locality.label$plotID,1,4)=="MOAB"],"\n",
                                                                             locality.label$line5[substr(locality.label$plotID,1,4)=="MOAB"],"\n")
    locality.label$label2[substr(locality.label$plotID,1,4)=="MOAB"] <- paste(locality.label$line1[substr(locality.label$plotID,1,4)=="MOAB"],"\n",
                                                                              paste0(locality.label$elevation[substr(locality.label$plotID,1,4)=="MOAB"], "m N", 
                                                                                     locality.label$decimalLatitude[substr(locality.label$plotID,1,4)=="MOAB"], " W",
                                                                                     abs(locality.label$decimalLongitude[substr(locality.label$plotID,1,4)=="MOAB"])),"\n",
                                                                              locality.label$line4[substr(locality.label$plotID,1,4)=="MOAB"],"\n",
                                                                              paste("NEON", paste(locality.label$plotID[substr(locality.label$plotID,1,4)=="MOAB"],
                                                                                                  substr(locality.label$sampleID[substr(locality.label$plotID,1,4)=="MOAB"], 11,20),sep="."), ""),"\n")
    locality.label$label2[locality.label$domainID=="D05"] <- paste(locality.label$line1[locality.label$domainID=="D05"],"\n",
                                                                   locality.label$line2[locality.label$domainID=="D05"],"\n",
                                                                   paste0("N",locality.label$decimalLatitude[locality.label$domainID=="D05"], " W",
                                                                          abs(locality.label$decimalLongitude[locality.label$domainID=="D05"]), " Pitfall","\n"),
                                                                   locality.label$line4[locality.label$domainID=="D05"],"\n",
                                                                   paste("NEON", paste(locality.label$plotID[locality.label$domainID=="D05"],
                                                                                       substr(locality.label$sampleID[locality.label$domainID=="D05"], 11,20),sep="."), ""),"\n")
    
    
    
    # Create dat file
    writeLines(locality.label$label, con = "labels.dat")
  # }
#     if(input$preprint==FALSE){
#       site.data <- unique.data.frame(points[grepl(site.react(), points$siteName), ])
#       locality.label <- data.frame()
#       
#       for (i in 1:length(dateRange.react())){
#         locality.label <- rbind(locality.label,
#                                 data.frame(site.data, date = dateRange.react()[i]))
#       }
#       
#       for (i in 1:dim(locality.label)[1]){
#         locality.label$collDate[i] <- paste0(strsplit(as.character(locality.label$date[i]), split = "-")[[1]][3],
#                                              month.abb[as.numeric(strsplit(as.character(locality.label$date[i]), 
#                                                                            split = "-")[[1]][2])],
#                                              strsplit(as.character(locality.label$date[i]), split = "-")[[1]][1])
#         locality.label$sampleID[i] <- substr(locality.label$plotID[i],1,4)
#       }
#       
#       locality.label$line1 <- paste0(locality.label$country, ", ", locality.label$stateProvince, " ",
#                                      locality.label$county, " Co.")
#       locality.label$line2 <- paste0(locality.label$siteName, ".")
#       locality.label$line3 <- ""
#       locality.label$line4 <- paste("Pitfall trap", locality.label$collDate, 
#                                     locality.label$collector, sep=". ")
#       locality.label$line5 <- paste("NEON", locality.label$sampleID, "") 
#       locality.label$label <- paste(locality.label$line1,"\n",
#                                     locality.label$line2,"\n",
#                                     locality.label$line3,"\n",
#                                     locality.label$line4,"\n",
#                                     locality.label$line5,"\n")
#       locality.label$label2 <- paste(locality.label$line1,"\n",
#                                      locality.label$line2,"\n",
#                                      locality.label$line3,"\n",
#                                      locality.label$line4,"\n",
#                                      paste("NEON", substr(locality.label$plotID,1,4), " "),"\n")
#       writeLines(locality.label$label, con = "labels.dat")
#     }
    return(locality.label)
  })

  
  output$label  <- renderText({
    locality.label()$label2[1]
  })
  output$trapIDs <- renderText({
    points$trapID[grepl(site.react(),points$siteID)]
  })

  # output$trapID <- locality.label()$trapID
  # Render outputs
  # download buttons for parsed files
  output$locality <- downloadHandler(
    filename=function() {paste(paste("localitylabels", 
                                     as.character(substr(locality.label()$plotID[1],1,4)),
                                     dateStart.react(),
                                     sep="_"), 
                               "pdf", sep=".")},
    content = function(file) {
      out = knit2pdf('output.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
  })