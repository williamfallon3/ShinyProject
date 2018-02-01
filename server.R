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

function(input, output, session) {
  
  
  selected_precincts_filter = c(4,5,6,7,8)
  plot(raw_precincts_shapefile[all_man_precinct_filter, ],col='lightgrey')
  plot(raw_precincts_shapefile[selected_precincts_filter, ],col='turquoise',add=TRUE)
  
  # Compute summary data to join with shapefile
  precinctSummary = Data2017 %>% group_by(.,Precinct) %>% summarise(.,number = n()) 
  # man_precinct_shapes = raw_precincts_shapefile[all_man_precinct_filter, ] #moved to global
  
  # Join summary info to the precinct map
  man_precinct_shapes@data = left_join(man_precinct_shapes@data, precinctSummary,by = c('Precinct'))
  
  tempSummary = Data2017 %>% group_by(.,Issue.Date) %>% summarise(.,number = n())
  
  
  # reverse lookup by license plate
  
  #output$platenumber <- renderPrint({ input$platenumber })
  #plate.number = "DEG7726" #make this user input
  #byPlateNumber_dt = setkey(Data2017dt[ Plate == input$plate],Issue.Date)
  
  #filtersByPlateNumber = Data2017 %>% filter(.,Plate == input$plate) %>% arrange(.,desc(Issue.Date))

# cant get this to work for now, revert to filter in place 
  # output$PlateTable = renderDataTable({
  #  setkey(Data2017dt[ Plate == input$plate],Issue.Date)
  # })
  
  PlateFilter <- reactive({
    setkey(Data2017dt[ Plate == input$plate,1:11],Issue.Date)
  })
  
  output$PlateTable = renderDataTable({
    PlateFilter()
  },options = list(scrollX = TRUE, escape = FALSE))
  

  
  
  ### BACKUP THIS WORKS BASICALLY
  # output$PlateTable = renderDataTable({ 
  #   setkey(Data2017dt,Issue.Date)
  # },options = list(scrollX = TRUE))
  
  # PlateLookup = reactive({
  #   if (input$submit > 0) {
  #     byPlateNumber_dt = setkey(Data2017dt[ Plate == input$plate],Issue.Date)
  #   }
  # })
  
  # output$PlateTable <- renderTable({
  #   if (is.null(PlateLookup())) {return()}
  #   print(PlateLookup())
  # }, 'include.rownames' = FALSE
  # , 'include.colnames' = TRUE
  # )
  
  # leafletProxy("mymap", session) %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(data = man_precinct_shapes[selected_precincts_filter,],
  #               fillColor = "white",
  #               fillOpacity = 0,
  #               color = "green",
  #               stroke = T,
  #               weight = 1,
  #               layerId = as.character(man_precinct_shapes[selected_precincts_filter,]@data$Precinct),
  #               group = "regions",
  #               label = as.character(man_precinct_shapes[selected_precincts_filter,]@data$Precinct)) %>%
  
  bins <- c(0, 50000, 100000, 150000, 200000, 250000, 300000, 400000, 500000, Inf)
  pal <- colorBin("YlOrRd", domain = man_precinct_shapes@data$number, bins = bins)
  pal2 <- colorBin("Blues", domain = man_precinct_shapes@data$number, bins = bins*.15)  
  
  
  output$map1 = renderLeaflet({
    leaflet(man_precinct_shapes) %>%
      setView(lng = -73.92, lat = 40.78, zoom = 11) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(man_precinct_shapes@data$number),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = as.character(man_precinct_shapes@data$Precinct),
        group = "regions",
        label = paste(" Precinct #",as.character(man_precinct_shapes@data$Precinct)," / ",as.character(man_precinct_shapes@data$number)," Violations / $",as.character(man_precinct_shapes@data$revenue)),
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 3px"),
          textsize = "10px",
          direction = "top") 
        ) %>% 
      addLegend(pal = pal, values = ~man_precinct_shapes@data$number, opacity = 0.7, title = "Violations by Precinct",
                position = "bottomright")
    
  })
  
  output$map2 = renderLeaflet({
    leaflet(man_precinct_shapes) %>%
      setView(lng = -73.92, lat = 40.78, zoom = 11) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal2(man_precinct_shapes@data$number),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        layerId = as.character(man_precinct_shapes@data$Precinct),
        group = "regions",
        label = paste(" Precinct #",as.character(man_precinct_shapes@data$Precinct)," / ",as.character(man_precinct_shapes@data$number)," Violations / $",as.character(man_precinct_shapes@data$revenue)),
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 3px"),
          textsize = "10px",
          direction = "top") 
      ) %>% 
      addLegend(pal = pal2, values = ~man_precinct_shapes@data$number, opacity = 0.7, title = "Filtered Data",
                position = "bottomright")
    
  })
  
  output$mymap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = man_precinct_shapes,
                  fillColor = "white",
                  fillOpacity = 1,
                  color = "black",
                  stroke = T,
                  weight = 1,
                  layerId = as.character(man_precinct_shapes@data$Precinct),
                  group = "regions",
                  label = as.character(man_precinct_shapes@data$Precinct))
      # addPolygons(data = man_precinct_shapes,
      #             highlightOptions = highlightOptions(color = "white", weight = 2,
      #             bringToFront = TRUE),
      #             stroke = TRUE, smoothFactor = 1, fillOpacity = .5,
      #             label = as.character(man_precinct_shapes@data$Precinct),
      #             layerId = as.character(man_precinct_shapes@data$Precinct))
  })
  
  #----------- #cite source https://stackoverflow.com/questions/41104576/changing-styles-when-selecting-and-deselecting-multiple-polygons-with-leaflet-sh
  clickedIds = manhattan_precincts_list
  observeEvent(input$mymap_shape_click, {
    
    #create object for clicked polygon
    click = input$mymap_shape_click



    #append all click ids in empty vector
    #ifelse(click %in% clickedIds,clickedIds = clickedIds[clickedIds!=click],clickedIds=c(clickedIds,click))


  }) #END OBSERVE EVENT

  #-----------
  
  # PlotByDay = ggplot(violations_by_dayofmonth[violations_by_dayofmonth$day<32,], aes(day, perday)) +
  #   geom_line() +
  #   geom_smooth(method = "lm", color = "blue") +
  #   facet_wrap(~Precinct, scales = "free_y") +
  #   scale_x_discrete(limits=seq(1,31,5))
  # 
  # PlotByDayOverall = ggplot(violations_by_dayofmonth_summary[violations_by_dayofmonth_summary$day<32,], aes(day, perday)) +
  #   geom_line() +
  #   geom_smooth(method = "lm", color = "blue") +
  #   scale_x_discrete(limits=seq(1,31,5))
  
  dataByDay = violations_by_dayofmonth[violations_by_dayofmonth$day<32,]
  dataByDayOverall = violations_by_dayofmonth_summary[violations_by_dayofmonth_summary$day<32,]
  dataByWkday = violations_by_wkday[violations_by_wkday$wkday<7,]
  dataByWkdayOverall = violations_by_wkday_summary[violations_by_wkday_summary$wkday<7,]
  dataByMonth = violations_by_month[violations_by_month$month<13,]
  dataByMonthOverall = violations_by_month_summary[violations_by_month_summary$month<13,]
  
  # plot to show in analysis page
  # if (input$PlotChoice == "PlotByDay") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByDay, aes(day, perday)) +
  #                                   geom_point() +
  #                                   geom_smooth(method = "lm", color = "blue") +
  #                                   facet_wrap(~Precinct, scales = "free_y") +
  #                                   scale_x_discrete(limits=seq(1,31,5))
  #   )
  # } else if (input$PlotChoice == "PlotByDayOverall") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByDayOverall, aes(day, perday)) +
  #                                          geom_point() +
  #                                          geom_smooth(method = "lm", color = "blue") +
  #                                          scale_x_discrete(limits=seq(1,31,5))
  #   )
  # } else if (input$PlotChoice == "PlotByWkday") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByWkday, aes(wkday, tot)) +
  #                                     geom_point() +
  #                                     facet_wrap(~Precinct, scales = "free_y") +
  #                                     scale_x_discrete(limits=seq(0,6,1))
  #   )
  # } else if (input$PlotChoice == "PlotByWkdayOverall") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByWkdayOverall, aes(wkday, tot)) +
  #                                            geom_point() +
  #                                            scale_x_discrete(limits=seq(0,6,1))
  #   )
  # } else if (input$PlotChoice == "PlotByMonthl") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByMonth, aes(month, tot)) +
  #                                     geom_point() +
  #                                     facet_wrap(~Precinct, scales = "free_y") +
  #                                     scale_x_discrete(limits=seq(0,6,1))
  #   )
  # } else if (input$PlotChoice == "PlotByMonthOverall") {
  #   output$AnalysisPlot = renderPlot(ggplot(dataByMonthOverall, aes(month, tot)) +
  #                                            geom_point() +
  #                                            scale_x_discrete(limits=seq(0,6,1))
  #   )
  # } else {
  #   hr()
  # }
  
  # MapDataFiltered <- reactive({
  #   MapDataFiltered = Data2017 %>% filter(.,Violation==input$ViolationType) %>%
  #                         filter(.,License.Type==input$LicType) %>%
  #                         filter(.,State==input$StatePlate)
  #   
  # })
  
  # MapDataSummary <- reactive({
  #   Data2017 %>% filter(.,Violation==input$ViolationType) %>%
  #     # filter(.,License.Type==input$LicType) %>%
  #     # filter(.,State==input$StatePlate) %>%
  #     group_by(.,Precinct) %>% summarise(.,n = n(),revenue = sum(Fine.Amount))
  # 
  # })
  
  # output$content <- renderTable({
  #   MapDataFiltered()
  # })
  

  # output$plotmap <- renderPlot({
  #   qtm(man_precinct_shapes, MapDataSummary$number)
  # })
  
  # observe({
  #   x <- input$ViolationType
  #   if (x == "ALL")
  #     x <- ''
  #   y <- input$LicType
  #   if (y == "ALL")
  #     y <- ''
  #   z <- input$StatePlate
  #   if (z == "ALL")
  #     z <- ''
  # })
  
  MapFilter <- reactive({
    # 
    # if (input$ViolationType == "ALL") {
    #   input$ViolationType = ""
    # }
    # if (input$LicType == "ALL") {
    #   input$LicType = ""
    # }
    # if (input$ViolationType == "ALL") {
    #   input$StatePlate = ""
    # }
    
    setkey(Data2017dt[ Violation == input$ViolationType,][ License.Type == input$LicType,][ State == input$StatePlate,],Issue.Date)
    
  })
  
  MapSummary <- reactive({
    # 
    # if (input$ViolationType == "ALL") {
    #   input$ViolationType = ""
    # }
    # if (input$LicType == "ALL") {
    #   input$LicType = ""
    # }
    # if (input$ViolationType == "ALL") {
    #   input$StatePlate = ""
    # }
    
    setkey(Data2017dt[ Violation == input$ViolationType,][ License.Type == input$LicType,][ State == input$StatePlate,],Issue.Date)
    
  })
  
  output$MapTable = renderDataTable({
    MapFilter()
  },options = list(scrollX = TRUE))
  
  
  precinctSummary = Data2017 %>% group_by(.,Precinct) %>% summarise(.,number = n(), revenue = sum(Fine.Amount))
  man_precinct_shapes@data = left_join(man_precinct_shapes@data, precinctSummary,by = c('Precinct'))
  
  output$testMap = renderPlot({
    qtm(man_precinct_shapes, fill = c("number"), fill.palette = "Blues", ncol =2, labels="Precinct")
  })
  
  
  
  dataByDay = violations_by_dayofmonth[violations_by_dayofmonth$day<32,]
  output$PlotByDay = renderPlot(ggplot(dataByDay, aes(day, perday)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    facet_wrap(~Precinct, scales = "free_y") +
    scale_x_discrete(limits=seq(1,31,5))
  )
  # plot to show in analysis page
  dataByDayOverall = violations_by_dayofmonth_summary[violations_by_dayofmonth_summary$day<32,]
  output$PlotByDayOverall = renderPlot(ggplot(dataByDayOverall, aes(day, perday)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    scale_x_discrete(limits=seq(1,31,5))
  )

## THESE WORK AS PLOTS BUT NEED TO FIX BUG
  
  dataByWkday = violations_by_wkday[violations_by_wkday$wkday<7,]
  output$PlotByWkday = renderPlot(ggplot(dataByWkday, aes(wkday, tot)) +
                                  geom_point() +
                                  facet_wrap(~Precinct, scales = "free_y") +
                                  scale_x_discrete(limits=seq(0,6,1))
  )
  # plot to show in analysis page
  dataByWkdayOverall = violations_by_wkday_summary[violations_by_wkday_summary$wkday<7,]
  output$PlotByWkdayOverall = renderPlot(ggplot(dataByWkdayOverall, aes(wkday, tot)) +
                                         geom_point() +
                                         scale_x_discrete(limits=seq(0,6,1))
  )

  ###############
  
  dataByMonth = violations_by_month[violations_by_month$month<13,]
  output$PlotByMonth = renderPlot(ggplot(dataByMonth, aes(month, tot)) +
                                    geom_point() +
                                    facet_wrap(~Precinct, scales = "free_y") +
                                    scale_x_discrete(limits=seq(1,12,1))
  )
  # plot to show in analysis page
  dataByMonthOverall = violations_by_month_summary[violations_by_month_summary$month<13,]
  output$PlotByMonthOverall = renderPlot(ggplot(dataByMonthOverall, aes(month, tot)) +
                                           geom_point() +
                                           scale_x_discrete(limits=seq(1,12,1))
  )

}