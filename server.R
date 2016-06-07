# title: "Stressed Lake Tool"
# author: "Nathan Johnson"
# date: "2015-06-17"
# server.R

# some change checking user name
# input = data.frame(stationName = "23452,NA,HOLDEN LAKE", stressDate = as.Date("2015-03-31"))
# input = data.frame(stationName = "17570,NA,LAKE HELENE", stressDate = as.Date("2007-03-31"))
# input = data.frame(stationName = "783344,23549,LAKE GENEVA (USED FRANCIS)", stressDate = as.Date("2007-03-31"))

library(ROracle)
library(RColorBrewer)
library(maptools)
library(maps)
library(mapproj)
library(pander)
library(xtable)
library(rmarkdown)
library(shiny)
library(leaflet)

# library(kfigr) # only for html document figure captions

# stationMetaData <- read.csv("app/listOfStressedLakes.csv", header = TRUE)
stationMetaData <- read.csv("listOfStressedLakes.csv", header = TRUE)
drv <- dbDriver("Oracle")
con <- dbConnect(drv, username = "RESDATA_VIEW", password = "prod_resdata_view", 
                 dbname = "QUERY_RPTWP") # develop connection to oracle

shinyServer(function(input, output, session) {
  
  # Basic inventory metadata
  inventory <- reactive({
    inventory = dbGetQuery(con, paste0("SELECT site_id, site_name, latitude_nbr,
                                      longitude_nbr, county_name FROM 
                                      ods_permit.site WHERE site_id IN (",
                                       unlist(strsplit(as.character(input$stationName), ","))[1], ")"))
  })
  
  ## Lat and long converted to decimal degrees
  inven = reactive({
    inven = within(inventory(), {
      dms = do.call(rbind, strsplit(as.character(LATITUDE_NBR), " "))
      Lat = as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
      rm(dms)
    })
    inven = within(inven, {
      dms = do.call(rbind, strsplit(as.character(LONGITUDE_NBR), " "))
      Long = -(as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60)
      rm(dms)
    })
    
  })
  
  ## ALL STATION DATATABLE FOR MOUSECLICK STATION ID IN LEAFLET MAP
  tableDT = dbGetQuery(con, paste0("SELECT site_id, site_name, county_name, latitude_nbr,
                                      longitude_nbr  FROM 
                                      ods_permit.site WHERE site_id IN (",
                                   paste(stationMetaData$SID, collapse = ","), ")")) 
  names(tableDT) = c("SID", "Name", "County", "Lat", "Long")
  
  tableDT = within(tableDT, {
    dms = do.call(rbind, strsplit(as.character(Lat), " "))
    Lat = as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
    rm(dms)
  })
  tableDT = within(tableDT, {
    dms = do.call(rbind, strsplit(as.character(Long), " "))
    Long = -(as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60)
    rm(dms)
  })
  
  ## dataForm - Formatted daily data
  dataForm <- reactive({
    if(strsplit(as.character(input$stationName), ",")[[1]][2] != "NA"){
      data <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val 
                                    FROM resdata.hydrologic_data_daily_agg 
                                  WHERE site_dim_id IN (",
                                    unlist(strsplit(as.character(input$stationName), ","))[1],
                                    ",",
                                    unlist(strsplit(as.character(input$stationName), ","))[2],
                                    ") AND resource_parameter_dim_id = 2", sep = ""))
    } else {data <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val 
                                          FROM resdata.hydrologic_data_daily_agg 
                                        WHERE site_dim_id IN (",
                                          unlist(strsplit(as.character(input$stationName), ","))[1],
                                          ") AND resource_parameter_dim_id = 2", sep = ""))}
    dataForm = data.frame(siteID = data$SITE_DIM_ID, 
                          date = as.Date(as.character(data$DATE_DIM_ID), format = "%Y%m%d"), 
                          level = data$RECORDED_VAL)
  })
  
  ## dataFormMon - Formatted monthly data
  dataFormMon = reactive({
    dataForm1 <- within(dataForm(), {dateMon = format(date, "%Y-%m")})
    dataFormMon <- aggregate(dataForm1$level, 
                             by = list(dataForm1$dateMon), FUN = mean, na.rm = TRUE)
    names(dataFormMon) <- c("dateMon", "level")
    dataFormMon <- dataFormMon[complete.cases(dataFormMon),]
  })
  
  #   dataFormMon <- apply.monthly(xts(dataForm$level, order.by = dataForm$date), apply, 2, median, na.rm = TRUE)
  
  ## Figure of index map
  output$map <- renderPlot({
    map.ylim = c(min(inven()$Lat)-0.5, max(inven()$Lat)+0.5)
    map.xlim = c(min(inven()$Long)-0.5, max(inven()$Long)+0.5)
    map("county", "florida", col = c("gray90"), 
        xlim = map.xlim, ylim = map.ylim, fill = FALSE)
    map("state", xlim = map.xlim, ylim = map.ylim , col ="black", fill = FALSE, add = TRUE)
    for(i in 1:nrow(inven())){
      points(x = as.vector(inven()$Long[i]), y = as.vector(inven()$Lat[i]), 
             cex = 0.75, pch = i, col = i)
    }
    text(x = inven()$Long, y = inven()$Lat, 
         labels = inven()$SITE_ID, col = "black", pos = 4, cex = 0.75)
    map.axes()
  })
  
  ## Figure of hydrograph with evaluation level
  output$plot <- renderPlot({
    dataFormMon5year <- dataFormMon()[
      as.Date(paste0(dataFormMon()$date, "-01"), 
              format = "%Y-%m-%d") > as.Date(input$stressDate)-(5*365.25),]
    dataFormMon5year <- dataFormMon5year[as.Date(paste0(dataFormMon5year$date, "-01"), 
                                                 format = "%Y-%m-%d") < as.Date(input$stressDate),]
    dataFormMon1year <- dataFormMon5year[as.Date(paste0(dataFormMon5year$date, "-01"), 
                                                 format = "%Y-%m-%d") < as.Date(input$stressDate),]
    par(mar = c(2,4,1,0.5))
    
    stationMetaDataSubset = subset(stationMetaData, 
                                   SID == strsplit(as.character(input$stationName), ",")[[1]][1])
    plot(dataForm()$date, dataForm()$level, xlim = c(input$dates[1], input$dates[2]), type = "n", 
         ylim = c(min(stationMetaDataSubset$guidanceLevel, min(dataForm()$level, na.rm = TRUE)), 
                  max(stationMetaDataSubset$highLevel, max(dataForm()$level, na.rm = TRUE))), 
         xlab = "Date", ylab = "Level (NGVD29, ft)", pch = 2)
    abline(h = stationMetaDataSubset$highLevel, col = "red", lty = 3)
    abline(h = stationMetaDataSubset$guidanceLevel, col = 20, lty = 5)
    
    for(i in 1:length(unique(dataForm()$siteID))){
      d = dataForm()[dataForm()$siteID == sort(unique(dataForm()$siteID))[i],]
      points(d$date, d$level, pch = i, col = i, cex = 0.5)
    }
    lines(x = c(input$stressDate, as.Date(input$stressDate)-(5*365.25)), 
          y = c(as.numeric(quantile(dataFormMon5year$level, 0.66667, na.rm = TRUE)),
                quantile(dataFormMon5year$level, 0.66667, na.rm = TRUE)), lty = 4, col = "magenta")
    legend("topleft", legend = c(input$stationName, "Evaluation Level 5 Year", "Guidance Level"), 
           col = c("black", "red", 20), pch = c(1,NA,NA), 
           lty = c(0,3,5), merge = FALSE, bty = "n", cex = 0.75)
    #     seq(1:length(unlist(strsplit(as.character(input$stationName), ",")))
  })
  
  ## Datatable of stressed lake evaluation statistics
  output$dt <- renderDataTable({
    dtFinal = data.frame()
    for(i in 0:15){
      #             i = 0
      dataFormMon5year <- dataFormMon()[
        as.Date(paste0(dataFormMon()$date, "-01"), 
                format = "%Y-%m-%d") > as.Date(input$stressDate)-((i+5)*365.25),]
      dataFormMon5year <- dataFormMon5year[
        as.Date(paste0(dataFormMon5year$date, "-01"), 
                format = "%Y-%m-%d") < as.Date(input$stressDate)-((i)*365.25),]
      dataFormMon1year <- dataFormMon5year[
        as.Date(paste0(dataFormMon5year$date, "-01"), 
                format = "%Y-%m-%d") > as.Date(input$stressDate)-((i+1)*365.25),]
      
      stationMetaDataSubset = subset(stationMetaData, 
                                     SID == unlist(strsplit(as.character(input$stationName), ","))[1])[1,]
      
      dataP66 <- as.numeric(quantile(dataFormMon5year$level, 0.66667, na.rm = TRUE))
      dataP40 <- as.numeric(quantile(dataFormMon5year$level, 0.40, na.rm = TRUE))
      count5year <- nrow(dataFormMon5year)
      count1year <- nrow(dataFormMon1year)
      dtStressed = data.frame("Evaluation Date" = (input$stressDate-(i*365.25)),
                              "Low Guidance Level" = stationMetaDataSubset$guidanceLevel, 
                              "Evaluation Level" = dataP66, 
                              "Recovery Level" = dataP40, 
                              "Observations 1 Year" = count1year, 
                              "Observations 5 Year" = count5year, 
                              "Stressed Criteria Met" = stationMetaDataSubset$guidanceLevel > dataP66, 
                              "Recovery Criteria Met" = stationMetaDataSubset$guidanceLevel <= dataP40,
                              check.names = F)
      dtFinal = rbind(dtFinal,dtStressed)
    }
    dtFinal}, options = list(searching = FALSE, paging = TRUE, pageLength = 5, lengthMenu = c(5,10,15)))
  
  ## All wells and lakes inventory
  output$fulldt <- renderDataTable({
    data.frame("SID_altSID" = paste(stationMetaData$SID, stationMetaData$altSID, sep = ","), 
               name = stationMetaData$name, 
               guidanceLevel = stationMetaData$guidanceLevel)
  }, options = list(pageLength = 10, paging = TRUE))
  
  ## Leaflet Map
  output$myMap = renderLeaflet({
    map = leaflet(dataMapBounds()) %>%
      # fitBounds(lng1 = min(inven()$Long) - 0.01, lng2 = max(inven()$Long) + 0.01, 
      #           lat1 = max(inven()$Lat) + 0.01 , lat2 = min(inven()$Lat) -0.01) %>% 
      # # addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
      addPopups(inven()$Long, inven()$Lat, 
                popup = ~paste(as.character(inven()$SITE_ID), 
                               as.character(inven()$SITE_NAME), sep = " - ")) %>%
      addCircleMarkers(color = "white", weight = 2, radius = 15, fillColor = "black", fillOpacity = 0.2,
                       popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       stroke = TRUE,
                       layerId = ~as.character(SID),
                       # layerId = 1, 
                       # clusterId= 2,
                       # options = markerOptions(draggable = TRUE),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
                       ) 
      map
  })
  
  # dataMapBounds <- reactive({
  #   bounds = input$myMap_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   subset(tableDT,
  #          Lat >= latRng[1] & Lat <= latRng[2]&
  #            Long >= lngRng[1] & Long <= lngRng[2])
  # })
  
  dataMapBounds <- reactive({
    # validate(
    #   need(input$stationName != "", "Please Enter Station - SID")
    # ) 
    latRng <- range(max(inven()$Lat)+5,min(inven()$Lat)-5)
    lngRng <- range(min(inven()$Long)-5,max(inven()$Long)+5 )
    subset(tableDT,
           Lat >= latRng[1] & Lat <= latRng[2]&
             Long >= lngRng[1] & Long <= lngRng[2])
  })
  
  ## Add other lakes in the areashiny
  # observe({
  #   leafletProxy("myMap", session, data = dataMapBounds()) %>%
  #     addCircles(color = "black", 
  #                popup = ~paste(as.character(SID), as.character(Name), sep = ", ")) %>%
  #     clearShapes()
  # })
  
  ## Print report as PDF, HTML, or DOCX
  output$downloadReport <- downloadHandler(
    filename = function(){ 
      paste('stressedLakeReport', sep = '.', 
            switch(input$format, PDF = 'pdf', HTML = 'html', Word = 'docx')
      )
    },
    
    content = function(file) {
      src <- normalizePath('stressedLakeReport.Rmd')
      owd <- setwd(tempdir()) # temporarily switch to the temp dir, in case you do not have write
      on.exit(setwd(owd))     # permission to the current working directory
      file.copy(src, 'stressedLakeReport.Rmd')
      
      out <- render('stressedLakeReport.Rmd', switch(
        input$format,
        PDF = pdf_document(fig_caption = TRUE, fig_width = 9, fig_height = 6), 
        HTML = html_document(), 
        Word = word_document(fig_width = 9, fig_height = 6, fig_caption = TRUE)
      ))
      file.rename(out, file)
    }
  )
  ##########################################
  # Not used because need to determine files and folder naming convention to use. This would need to be set up.
  ## Previous Evaluations Link
  # observeEvent(input$prevEvals, choose.files(file.path("L:","Hydro Eval","Staff","Keith","Lake Data Sheets (Ecologic Evaluation)","previousEvals")))
  ##############################
  
})