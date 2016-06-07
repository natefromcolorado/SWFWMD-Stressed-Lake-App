# title: "Stressed Lake Tool"
# author: "Nathan Johnson"
# date: "2015-06-17"
# ui.R 

library(shiny)
library(leaflet)
library(ROracle)
library(RColorBrewer)
library(maptools)
library(maps)
library(mapproj)
library(pander)
library(xtable)
library(rmarkdown)


# stationMetaData <- read.csv("stressedLake - app/listOfStressedLakes.csv", header = TRUE, na.strings = "")
stationMetaData <- read.csv("listOfStressedLakes.csv", header = TRUE, na.strings = "")
stationNames <- as.character(paste(stationMetaData$SID, stationMetaData$altSID, stationMetaData$name, sep = ","))

shinyUI(fluidPage(
  
  ## include SWFWMD logo using HTML syntax
  div(tags$header(
    tags$img(src="colorSealTransparent.png", height=70, width=70, style ="display:inline-block"),
    tags$h2("SWFWMD Stressed Lake Dashboard",style ="display:inline-block")
  )),
  br(),
  sidebarLayout(
    sidebarPanel(
      #       helpText("NA: No"),
      #       textInput("station", "Station", "M-0483"),
      # img(src="color-seal-transparent.png", height = 100, width = 100),
      selectInput("stationName", "Choose Station: (SID, altSID, Name)", choices = stationNames, multiple=F, width="100%"),
      # img(src = "colorSealTransparent.png", height = 100, width = 100),
      helpText("Note: 'NA' No alternate SID available"),
      #       uiOutput("stationNames"),
      #       textInput("stationName", "Choose Monitoring Well", "25370, 19463, 579770"),
      
      dateRangeInput("dates", 
                     "Hydrograph Date Range",
                     start = "1970-01-01", 
                     end = as.character(Sys.Date())),
      
      dateInput('stressDate', label = "Evaluation Date: yyyy-mm-dd", 
                value = Sys.Date()),
      
      #       submitButton(text="Submit Query"),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE), 
      downloadButton('downloadReport'),
      #       helpText(a("Click Here", href="L:/Hydro Eval/Staff/Nathan/R/scripts/SWUCAevaluationCriteria2000.xlsx", target="_blank")),
      # actionButton("prevEvals", "Previous Evaluations"), # used to open folders to previous evaluations. Need to figure out how to link to folders.
      br(),
      includeHTML("procedure.html"),
      br()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Evaluation', plotOutput("plot"),dataTableOutput("dt")),
        tabPanel('Map', leafletOutput('myMap', height = "700px")),
        tabPanel('Lookup Table',dataTableOutput('fulldt')),
        #         tabPanel('Map', plotOutput("map")),
        tabPanel('Rule', includeHTML("rule.html"))
      )
    )
  )
))
