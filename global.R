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
library(DT)
library(shinythemes)
setwd("~/NYCDSA/Projects/ShinyProject")

#Data2017 = read.csv("Cleaned_DF_2017.csv", header = TRUE, sep=",")
#Data2017dt = fread("Cleaned_DF_2017.csv", header = TRUE)
Data2017dt = as.data.table(Data2017)

raw_precincts_shapefile = readOGR("nyprecincts.json")
raw_precincts_shapefile = spTransform(raw_precincts_shapefile, CRS("+proj=longlat +datum=WGS84"))
# "+proj=longlat +datum=WGS84" # swaped this out
manhattan_precincts_list = c(19,14,18,1,13,17,20,10,6,5,9,24,34,23,0,33,28,7,25,26,30,32)
all_man_precinct_filter <- raw_precincts_shapefile$Precinct %in% manhattan_precincts_list

man_precinct_shapes = raw_precincts_shapefile[all_man_precinct_filter, ]

precinctSummary = Data2017 %>% group_by(.,Precinct) %>% summarise(.,number = n(), revenue = sum(Fine.Amount))
man_precinct_shapes@data = left_join(man_precinct_shapes@data, precinctSummary,by = c('Precinct'))
#precinctSummary2 = Data2017 %>% filter(.,State="NJ") %>% group_by(.,Precinct) %>% summarise(.,numberNJ = n(), revenueNJ = sum(Fine.Amount))
#man_precinct_shapes@data = left_join(man_precinct_shapes@data, precinctSummary2,by = c('Precinct'))


bins <- c(0, 50000, 100000, 150000, 200000, 250000, 300000, 400000, 500000, Inf)
pal <- colorBin("YlOrRd", domain = man_precinct_shapes@data$number, bins = bins)

monthdays = rep(12,31)
monthdays[29:30]=11
monthdays[31]=7
daysinmonth = c(31,28,31,30,31,30,31,31,30,31,30,31)

totalsByViolation = Data2017 %>% group_by(.,Violation) %>% summarise(.,number = n(),revenue = sum(Fine.Amount)) %>% arrange(.,desc(number))
top25Violations = as.character(totalsByViolation$Violation[1:25])

totalsByState = Data2017 %>% group_by(.,State) %>% summarise(.,number = n(),revenue = sum(Fine.Amount)) %>% arrange(.,desc(number))
topStates = as.character(totalsByState$State)

totalsByType = Data2017 %>% group_by(.,License.Type) %>% summarise(.,number = n(),revenue = sum(Fine.Amount)) %>% arrange(.,desc(number))
topLicTypes = as.character(totalsByType$License.Type)

violations_by_month = Data2017 %>% group_by(.,month,Precinct) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot)) %>% mutate(.,permth=tot/daysinmonth[as.numeric(month)]) %>% filter(.,Precinct %in% manhattan_precincts_list)
violations_by_month_summary = Data2017 %>% group_by(.,month) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot)) %>% mutate(.,permth=tot/daysinmonth[as.numeric(month)])

violations_by_dayofmonth = Data2017 %>% group_by(.,day,Precinct) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot)) %>% mutate(.,perday=tot/monthdays[as.numeric(day)]) %>% filter(.,Precinct %in% manhattan_precincts_list)
violations_by_dayofmonth_summary = Data2017 %>% group_by(.,day) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot)) %>% mutate(.,perday=tot/monthdays[as.numeric(day)])

violations_by_wkday = Data2017 %>% group_by(.,wkday,Precinct) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot)) %>% filter(.,Precinct %in% manhattan_precincts_list)
violations_by_wkday_summary = Data2017 %>% group_by(.,wkday) %>% summarise(.,tot=n()) %>% arrange(.,desc(tot))