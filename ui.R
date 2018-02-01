library(shiny)
library(data.table)
library(maptools)
library(leaflet)
library(maps)
library(rjson)
library(rgdal)
library(ggplot2)
library(dplyr)
library(tmap)
library(plotly)
library(stringr)
library(tidyr)

fluidPage(theme = shinytheme("slate"),
  titlePanel("NYC Traffic Violations"),
  navbarPage("Features",
    tabPanel("Mapping NYC Traffic Violations",
             fluidRow(
               column(2,
                      selectizeInput("ViolationType", label = h5("Violation Type"), 
                                     choices = c(top25Violations), selected = "CROSSWALK"),
                      selectizeInput("LicType", label = h5("License Type"), 
                                     choices = c(topLicTypes), selected = "PAS"),
                      selectizeInput("StatePlate", label = h5("State"), 
                                     choices = c(topStates), selected = "NY")
               ),
               column(5, offset = 0,
                      fluidPage(leafletOutput("map1"))
                      #plotOutput("testMap")
               ),
               column(5,
                      fluidPage(leafletOutput("map2"))
               )
             ),
             div(dataTableOutput('MapTable'), style = "font-size:75%")
    ),
    tabPanel("Ticket Lookup",
             sidebarLayout(
               sidebarPanel(
                 textInput("plate", label = h3("Plate #"), value = "DEG7726"),
                 #actionButton("submit","Submit")
                 width=2
               ),
               mainPanel(
                 div(dataTableOutput('PlateTable'), style = "font-size:75%")
                 
               )
             )
    ),
    tabPanel("Analysis - Day of Month",
             plotOutput('PlotByDay'),
             plotOutput('PlotByDayOverall')
    ),
    tabPanel("Analysis - Day of Week",
             plotOutput('PlotByWkday'),
             plotOutput('PlotByWkdayOverall')
    ),
    tabPanel("Analysis - Month to Month",
             plotOutput('PlotByMonth'),
             plotOutput('PlotByMonthOverall')
    ),
    
    
    tags$head(
      tags$style(
        HTML(
          ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
        )
      ) 
    )

    
  )
)
