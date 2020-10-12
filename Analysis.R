#Look at claimant count chages by deprivation decile pre/post COVID19

library(nomisr)
library(tidyverse)
library(readxl)

################# Import and process old CC ##########################

old_raw <- nomis_get_data("NM_162_1",
                          date = "2019-08",
                          #LSOAs and Scottish datazones
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

old <- old_raw %>% rename(area = "GEOGRAPHY_CODE",
                          claimant_count_old = "OBS_VALUE")

old <- old %>% select(area,
                      claimant_count_old)

old$temp <- as.numeric(gsub("E","",old$area))
old <- old %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
old <- old %>% select(-temp)



########################## Import and process new CC ###################################

new_raw <- nomis_get_data("NM_162_1",
                          date = "2020-08",
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

new <- new_raw %>% rename(area = "GEOGRAPHY_CODE",
                          claimant_count_new = "OBS_VALUE")

new <- new %>% select(area,
                      claimant_count_new)

new$temp <- as.numeric(gsub("E","",new$area))
new <- new %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
new <- new %>% select(-temp)

##############################################

# y1 <- nomis_data_info("NM_2010_1")
# b1 <- nomis_get_metadata("NM_2010_1", concept = "GEOGRAPHY", type = "type")
# e1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURES")
# f1 <- nomis_get_metadata("NM_2010_1", concept = "C_AGE")
# c1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURE")
# h1 <- nomis_get_metadata("NM_2010_1", concept = "GENDER")

############################ Import and process England and Wales populations ########################



population <- nomis_get_data("NM_2010_1",
                                time =  "latest",
                                geography = c("TYPE298"),
                                measures = 20100,
                                c_age = 203,
                                gender = 0)

population <- population %>% rename(area = "GEOGRAPHY_CODE",
                                          population = "OBS_VALUE")

population <- population %>% select(area, population)
#Also filter for London only
population$temp <- as.numeric(gsub("E","",population$area))
population <- population %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
population <- population %>% select(-temp)


population17 <- nomis_get_data("NM_2010_1",
                             time =  "2017",
                             geography = c("TYPE298"),
                             measures = 20100,
                             c_age = 203,
                             gender = 0)

population17 <- population17 %>% rename(area = "GEOGRAPHY_CODE",
                                    population17 = "OBS_VALUE")

population17 <- population17 %>% select(area, population17)
#Also filter for London only
population17$temp <- as.numeric(gsub("E","",population17$area))
population17 <- population17 %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
population17 <- population17 %>% select(-temp)

######################## Create one dataset ###############################################

cc <- merge(old, new, by = "area")

cc <- merge(cc, population, by = "area", all.x = T)

cc <- merge(cc, population17, by = "area", all.x = T)

########################### Calculate rates ########################################

cc$old_rate <- (cc$claimant_count_old/cc$population)*100

cc$new_rate <- (cc$claimant_count_new/cc$population)*100


cc$`Claimant count rate ppt change (August 2019 - August 2020)` <- cc$new_rate - cc$old_rate

cc$old_rate <- round(cc$old_rate,1)
cc$new_rate <- round(cc$new_rate,1)
cc$`Claimant count rate ppt change (August 2019 - August 2020)` <- round(cc$`Claimant count rate ppt change (August 2019 - August 2020)`,1)

cc$"Change decile (1 = low)" <- ntile(cc$`Claimant count rate ppt change (August 2019 - August 2020)`,10)
cc$"Change decile (1 = low)" <- as.factor(cc$"Change decile (1 = low)")

#make nice names for the columns
cc <- cc %>% rename("lsoa11cd" = area, 
                    "Claimant count (August 2019)" = claimant_count_old, 
                    "Claimant count (August 2020)" = claimant_count_new, 
                    "Population 2018" = population, 
                    "Population 2017" = population17, 
                    "Claimant count rate % (August 2019)" = old_rate, 
                    "Claimant count rate % (August 2020)" = new_rate)

#format the data
cc$`Claimant count (August 2019)` <- format(cc$`Claimant count (August 2019)`, big.mark = ",")
cc$`Claimant count (August 2020)` <- format(cc$`Claimant count (August 2020)`, big.mark = ",")
cc$`Population 2018` <- format(cc$`Population 2018`, big.mark = ",")
cc$`Population 2017` <- format(cc$`Population 2017`, big.mark = ",")

EWlook <- read_csv("os lsoa msoa lookup.csv")
EWScotlook <- EWlook %>% select(LSOA11CD, LSOA11NM,LAD17NM, RGN11NM)
EWScotlook <- EWScotlook[!duplicated(EWScotlook),]
# EWScotlook <- bind_rows(EWlook, scotlook)
EWScotlook <- rename(EWScotlook,"Local Authority" = LAD17NM, "Region/Country" = RGN11NM, "Neighbourhood name" = LSOA11NM)

cc <- merge(cc, EWScotlook, by.x ="lsoa11cd", by.y = "LSOA11CD" )
#cc <- select(cc,-`Population 2018`)

###### ADD IMD

#' England
IMD19Eng <- read_excel("File_2_-_IoD2019_Domains_of_Deprivation.xlsx", 
                       sheet = "IoD2019 Domains")
IMD19Eng <- IMD19Eng %>% select(`LSOA code (2011)`,
                                `Local Authority District name (2019)`,
                                `Local Authority District code (2019)`, 
                                `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
                                `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  rename(`IMD Rank (1 is most deprived)` = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
         `IMD decile (1 is most deprived)` = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`
  )


cc <- merge(cc, IMD19Eng[c(1,4,5)], by.x = "lsoa11cd", by.y = "LSOA code (2011)", all.x = T)
cc$`IMD rank London (1 is most deprived)` <- round(rank(cc$`IMD Rank (1 is most deprived)`),0)
#rebased deciles
cc <- cc %>% mutate(`IMD decile London (1 is most deprived)` = ntile(`IMD rank London (1 is most deprived)`,10))

#remove England ranks
cc <- select(cc,-`IMD Rank (1 is most deprived)`,-`IMD decile (1 is most deprived)`,-`Region/Country`)


cc <- cc[,c(1,10,11,4,5,12,13,6:9)]
saveRDS(cc, "cc.rds")
###########################################
################### TABLE ################
###########################################
library(reactable)
library(crosstalk)


table <- cc %>% select(-`Population 2018`, -`Population 2017`)


#table1 <- table[!is.na(table$`Change decile (1 = low)`),]


table2 <- SharedData$new(table, group = "1")


##########the reactable table

tbl <- reactable(table2, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("Claimant count rate ppt change (August 2019 - August 2020)" = "desc"),
                 columns = list(`Change decile (1 = low)` = colDef(filterable = T),
                                `Local Authority` = colDef(filterable = T),
                                `IMD decile London (1 is most deprived)` = colDef(filterable = T)),
                 #`COVID-19 deaths per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0)),
                 #`COVID-19 deaths age adjusted per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0))),
                 theme = reactableTheme(
                   stripedColor = "#faf8f1",
                   highlightColor = "#e5dec4",
                   cellPadding = "6px 10px",
                   style = list(fontFamily = "Arial", fontSize = "12px"),
                   #searchInputStyle = list(width = "100%", fontWeight = "400"),
                   headerStyle = list(color = "white",background = "#2A2A2A",
                                      "&:hover[aria-sort]" = list(background = "#8c8c8c "),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#8c8c8c"),
                                      borderColor = "#555"
                   )
                 )) 
tbl

######################################
############## MAP ###################
#####################################

library(rgdal)
library(rgeos)
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

lsoa.centroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")

#merge in the cc data
#EWS.centroids <- st_as_sf(EWS.centroids)
EWS.centroids.df <- merge(cc,lsoa.centroids , by = "lsoa11cd", all.x = T)
EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.df$ccradius <- (EWS.centroids.df$`Claimant count rate ppt change (August 2019 - August 2020)`)* 0.5 + 2
EWS.centroids.df$"Change decile (1 = low)" <- as.factor(EWS.centroids.df$"Change decile (1 = low)")
#EWS.centroids.df <- EWS.centroids.df[c(1:14,16,15)]


EWS.centroids.df$`IMD decile London (1 is most deprived)` <- as.factor(EWS.centroids.df$`IMD decile London (1 is most deprived)`)

factpal <- colorFactor("RdBu",levels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), 
                       ordered = TRUE, reverse = F )

labels <- sprintf("<strong>%s</strong><br/>%g ppt change<sup></sup>",
                  EWS.centroids.df$`Neighbourhood name`, round(EWS.centroids.df$`Claimant count rate ppt change (August 2019 - August 2020)`,1)) %>% 
  lapply(htmltools::HTML)





#function for adding circle sizes to the legend
addLegendCustom <- function(title,map, colors, labels, sizes, opacity = 0.5, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-12,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, title = title,
                   labels = labelAdditions, opacity = opacity, position = position))
}

#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("Claimant count change and deprivation,<br> August 2019 to August 2020, London</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Claimant Count, ONS; Indices of Multiple Deprivation 2019, MHCLG<br> Analysis: WPI Economics on behalf of Trust for London"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)

#remove NA
#EWS.centroids.df <- EWS.centroids.df[!is.na(EWS.centroids.df$`Change decile (1 = low)`),]
#EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.dfXT <- SharedData$new(EWS.centroids.df, group = "1") 


#get region layer to make clip layer
NUTS1 <- geojson_sf("https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_3.geojson")
NUTS1 <- NUTS1 %>% filter(nuts118nm != "London")



#map element
m2 <- leaflet(EWS.centroids.dfXT, height = "580px", options = leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>% setView(-0.035,51.489, 10.2) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  
  
  addCircleMarkers( group = "circlegw",
                    radius = ~`ccradius`,
                    stroke = F,
                    color = ~factpal(`IMD decile London (1 is most deprived)`), opacity = 0.85, fillOpacity = 0.85,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   %>% 
  
  addPolygons(data = NUTS1, color = "white",opacity = 1, fillColor = "white", fillOpacity = 1)  %>%        #add london clip
  
  addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("+1ppts","+5ppts","+10ppts"),
                  
                  sizes = c(2.5,4.5,7)*2, position = "bottomright" , title = "CC change <br> 2019-2020<br>&nbsp") %>% 
  
  addLegend(pal = factpal, values = EWS.centroids.df$`IMD decile London (1 is most deprived)`, 
            labels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), position = "bottomright", title = "IMD deciles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2





combo <- htmltools::tagList(title, m2, tbl,sources) #I think this makes a combined html object
#browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.


############################################
################## stats and analysis ######
############################################

cc <- readRDS("cc.rds")
table$`Population 2018` <- gsub(",","",table$`Population 2018`) 

cc <- merge(cc,new, by.x = "lsoa11cd", by.y = "area", all.x = T)
cc <- merge(cc,old, by.x = "lsoa11cd", by.y = "area", all.x = T)
cc <- rename(cc,`cc 2020` = claimant_count_new)
cc <- rename(cc,`cc 2019` = claimant_count_old)

cc[,4:5] <- sapply(cc[,4:5], function(x){as.numeric(sub(",","",x))})


t2 <- cc %>% group_by(`IMD decile London (1 is most deprived)`) %>% 
  summarise("total CC 2020" = sum(`cc 2020`), "total CC 2019" = sum(`cc 2019`),
            "totalpop18" = sum(`Population 2018`))


t2$`Claimant Count 2020 rate` <- round((t2$`total CC 2020`/t2$totalpop18)*100,1)
t2$`Claimant Count 2017 rate` <- round((t2$`total CC 2017`/t2$totalpop18)*100,1)
t2$`CC change` <- t2$`Claimant Count 2020 rate` - t2$`Claimant Count 2017 rate`


write.csv(t2, "Average CC change by deprivation London.csv", row.names = F)


