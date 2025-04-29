#Look at claimant count chages by deprivation decile pre/post COVID19

library(nomisr)
library(tidyverse)
library(readxl)
library(stringr)

#note the claimant count data on NOMIS use 2011 LSOAs not 2021 yet (ugh), however population data stops for 2011 LSOAs after 2021, so here I just use the 2020 pop denominators throughout

################# Import and process old CC ##########################
nomisgeogs <- nomisr::nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY", type = "TYPE")

old_raw <- nomis_get_data("NM_162_1",
                          date = "prevyear",
                          #LSOAs and Scottish datazones
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

old <- old_raw %>% rename(lsoa11cd = "GEOGRAPHY_CODE",
                          claimant_count_old = "OBS_VALUE")

old <- old %>% select(lsoa11cd,
                      claimant_count_old, DATE_NAME)

old$temp <- as.numeric(gsub("E","",old$lsoa11cd))
old <- old %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
old <- old %>% select(-temp)


################# Import and process 2019 pre-covid CC ##########################

precovid_raw <- nomis_get_data("NM_162_1",
                          date = "2019-03",
                          #LSOAs and Scottish datazones
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

precovid <- precovid_raw %>% rename(lsoa11cd = "GEOGRAPHY_CODE",
                          claimant_count_precovid = "OBS_VALUE")

precovid <- precovid %>% select(lsoa11cd,
                                claimant_count_precovid, DATE_NAME)

precovid$temp <- as.numeric(gsub("E","",precovid$lsoa11cd))
precovid <- precovid %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
precovid <- precovid %>% select(-temp)

########################## Import and process new CC ###################################

new_raw <- nomis_get_data("NM_162_1",
                          date = "latest",
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

new <- new_raw %>% rename(lsoa11cd = "GEOGRAPHY_CODE",
                          claimant_count_new = "OBS_VALUE")

new <- new %>% select(lsoa11cd,
                      claimant_count_new, DATE_NAME)

new$temp <- as.numeric(gsub("E","",new$lsoa11cd))
new <- new %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
new <- new %>% select(-temp)

#stores the month for use later
month <- pull(new_raw[1,2]) %>% word(.,1)
year <- pull(new_raw[1,2]) %>% word(.,2)
oldyear <- pull(old_raw[1,2]) %>% word(.,2)

##############################################

# y1 <- nomis_data_info("NM_2010_1")
# b1 <- nomis_get_metadata("NM_2010_1", concept = "GEOGRAPHY", type = "type")
# e1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURES")
# f1 <- nomis_get_metadata("NM_2010_1", concept = "C_AGE")
# c1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURE")
# h1 <- nomis_get_metadata("NM_2010_1", concept = "GENDER")

############################ Import and process England and Wales populations ########################



populationraw <- nomis_get_data("NM_2010_1",
                                time =  "latest",
                                geography = c("TYPE298"),
                                measures = 20100,
                                c_age = 203,
                                gender = 0)

population <- populationraw %>% rename(lsoa11cd = "GEOGRAPHY_CODE",
                                          population = "OBS_VALUE")

population <- population %>% select(lsoa11cd, population, popyear = "DATE")
#Also filter for London only
population$temp <- as.numeric(gsub("E","",population$lsoa11cd))
population <- population %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
population <- population %>% select(-temp)

# ###### 2019 pop #####
# populationraw2019 <- nomis_get_data("NM_2010_1",
#                                 time =  "2019",
#                                 geography = c("TYPE298"),
#                                 measures = 20100,
#                                 c_age = 203,
#                                 gender = 0)
# 
# population2019 <- populationraw2019 %>% rename(lsoa11cd = "GEOGRAPHY_CODE",
#                                        population2019 = "OBS_VALUE")
# 
# population2019 <- population2019 %>% select(lsoa11cd, population2019, popyear = "DATE")
# #Also filter for London only
# population2019$temp <- as.numeric(gsub("E","",population2019$lsoa11cd))
# population2019 <- population2019 %>% filter(temp < 1004766) #the last LSOA of London is Wandsworth E01004765
# population2019 <- population2019 %>% select(-temp)


######################## Create one dataset ###############################################

cc <- merge(old, new, by = "lsoa11cd")

cc <- reduce(list(old, new, precovid), full_join, by = "lsoa11cd")

cc <- merge(cc, population, by = "lsoa11cd", all.x = T)



########################### Calculate rates ########################################

cc$old_rate <- (cc$claimant_count_old/cc$population)*100

cc$new_rate <- (cc$claimant_count_new/cc$population)*100

cc$precovid_rate <- (cc$claimant_count_precovid/cc$population)*100

cc$`Claimant count rate ppt 1yr change (March 2024 - March 2025)` <- round(cc$new_rate - cc$old_rate,1)
cc$`Claimant count rate ppt post covid change (March 2019 - March 2025)` <- round(cc$new_rate - cc$precovid_rate,1)

cc$old_rate <- round(cc$old_rate,1)
cc$new_rate <- round(cc$new_rate,1)
cc$precovid_rate <- round(cc$precovid_rate,1)

#change decile year on year
cc$"Change decile (1 = low)" <- ntile(cc$`Claimant count rate ppt 1yr change (March 2024 - March 2025)`, 10)
cc$"Change decile (1 = low)" <- as.factor(cc$"Change decile (1 = low)")

#change deciles post covid
cc$"Post covid change decile (1 = low)" <- ntile(cc$`Claimant count rate ppt post covid change (March 2019 - March 2025)`,10)
cc$"Post covid change decile (1 = low)" <- as.factor(cc$"Post covid change decile (1 = low)")


#make nice names for the columns
oldnames <- tibble("Old names" = names(cc))
newnames <- c("lsoa11cd", 
              
              paste0("Claimant count (",month," 2024)") , 
              
              "Date old",
              
              paste0("Claimant count (",month," 2025)") , 
              
              "Date new",
              
              paste0("Claimant count (",month," 2019)") , 
              
              "Date precovid",
              
              paste0("Population ",cc$`Pop year`[1]),
              
              "Pop year" , 
              
              "Claimant count rate % (March 2024)", 
              
              "Claimant count rate % (March 2025)",
              
              "Claimant count rate % (March 2019)",
              
              "Claimant count rate ppt change (March 2024 - March 2025)",
              
              "Claimant count rate ppt change (March 2019 - March 2025)",
              
              "Change decile (1 = low)", 
              "Post covid change decile (1 = low)")

names(cc) <- newnames



#format the data
#select columns that contain "claimant count" in the title
percentcols <-  cc %>% colnames() %>% str_detect("%")
countcols <-  cc %>% colnames() %>% str_detect("Claimant count \\(|Population")

cc[,countcols] <- format(cc[,countcols],big.mark = ",")


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


#cc <- cc[,c(1,10,11,4,5,12,13,6:9)]


saveRDS(cc, paste0("cc2_",Sys.Date(), ".rds"))

########################## HTML OUTPUTS, 1 year change #########################


###########################################
################### TABLE 1yr ################
###########################################
library(reactable)
library(crosstalk)


table <- cc %>% select(-`Date old`, -`Date new`, -`Pop year`)
table <- cc %>% select(`lsoa11cd`, `Neighbourhood name`, `Local Authority`,`IMD rank London (1 is most deprived)`,
                        `IMD decile London (1 is most deprived)`,
                        `Claimant count rate % (March 2024)`,
                        `Claimant count rate % (March 2025)`,
                       `Claimant count rate ppt change (March 2024 - March 2025)`,
                       `Change decile (1 = low)` )


#table1 <- table[!is.na(table$`Change decile (1 = low)`),]


table2 <- SharedData$new(table, group = "1")


##########the reactable table

tbl <- reactable(table2, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("Claimant count rate ppt change (March 2024 - March 2025)" = "desc"),
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

# library(rgdal)
# library(rgeos)
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
#suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

lsoa.centroids <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_PWC_in_England_and_Wales_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

#merge in the cc data
#EWS.centroids <- st_as_sf(EWS.centroids)
EWS.centroids.df <- merge(cc,lsoa.centroids , by = "lsoa11cd", all.x = T)
EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.df$ccradius <- (EWS.centroids.df$`Claimant count rate ppt change (March 2024 - March 2025`)* 0.7 + 2
EWS.centroids.df$"Change decile (1 = low)" <- as.factor(EWS.centroids.df$"Change decile (1 = low)")
#EWS.centroids.df <- EWS.centroids.df[c(1:14,16,15)]


EWS.centroids.df$`IMD decile London (1 is most deprived)` <- as.factor(EWS.centroids.df$`IMD decile London (1 is most deprived)`)

factpal <- colorFactor("RdBu",levels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), 
                       ordered = TRUE, reverse = F )

labels <- sprintf("<strong>%s</strong><br/>%g ppt change<sup></sup>",
                  EWS.centroids.df$`Neighbourhood name`, round(EWS.centroids.df$`Claimant count rate ppt change (March 2024 - March 2025`,1)) %>% 
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
title <- tags$div(HTML("Claimant count change and deprivation,<br> March 2024 to March 2025, London</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Sources: Claimant Count, ONS; Indices of Multiple Deprivation 2019, MHCLG<br> 
                         Analysis: WPI Economics on behalf of Trust for London<br>
                         Note: Indices of Multiple Deprivation have been re-based for London"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)

#remove NA
#EWS.centroids.df <- EWS.centroids.df[!is.na(EWS.centroids.df$`Change decile (1 = low)`),]
#EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.dfXT <- SharedData$new(EWS.centroids.df, group = "1") 


#get region layer to make clip layer
NUTS1 <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/NUTS_Level_1_January_2018_FCB_in_the_United_Kingdom_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
NUTS1 <- rmapshaper::ms_simplify(NUTS1, keep = 0.05)
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
  
  #addPolygons(data = NUTS1, color = "white",opacity = 1, fillColor = "white", fillOpacity = 1)  %>%        #add london clip
  
  addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("+1ppts","+5ppts","+7ppts"),
                  
                  sizes = c(2.5,4.5,7)*2, position = "bottomright" , title = "CC change <br> 2024-2025<br>&nbsp") %>% 
  
  addLegend(pal = factpal, values = EWS.centroids.df$`IMD decile London (1 is most deprived)`, 
            labels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), position = "bottomright", title = "IMD deciles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2





combo <- htmltools::tagList(title, m2, tbl,sources) #I think this makes a combined html object
#browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index.html") #this saves it as an HTML page in the default folder.
#htmlwidgets::saveWidget(combo,file =  "Updatedmapwidget.html", selfcontained = T)



########################## 
########################## HTML OUTPUTS, post-covid change #########################
########################## 

library(reactable)
library(crosstalk)


table <- cc %>% select(-`Date old`, -`Date new`, -`Pop year`)
table <- cc %>% select(`lsoa11cd`, `Neighbourhood name`, `Local Authority`,`IMD rank London (1 is most deprived)`,
                       `IMD decile London (1 is most deprived)`,
                        `Claimant count rate % (March 2019)`,
                       `Claimant count rate % (March 2025)`,
                       `Claimant count rate ppt change (March 2019 - March 2025)`,
                       `Post covid change decile (1 = low)` )


#table1 <- table[!is.na(table$`Change decile (1 = low)`),]


table2 <- SharedData$new(table, group = "1")


##########the reactable table

tbl <- reactable(table2, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("Claimant count rate ppt change (March 2019 - March 2025)" = "desc"),
                 columns = list(`Post covid change decile (1 = low)` = colDef(filterable = T),
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


#merge in the cc data
#EWS.centroids <- st_as_sf(EWS.centroids)
EWS.centroids.df <- merge(cc,lsoa.centroids , by = "lsoa11cd", all.x = T)
EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.df$ccradius <- (EWS.centroids.df$`Claimant count rate ppt change (March 2019 - March 2025`)* 0.7 + 2
EWS.centroids.df$"Change decile (1 = low)" <- as.factor(EWS.centroids.df$"Change decile (1 = low)")
#EWS.centroids.df <- EWS.centroids.df[c(1:14,16,15)]


EWS.centroids.df$`IMD decile London (1 is most deprived)` <- as.factor(EWS.centroids.df$`IMD decile London (1 is most deprived)`)

factpal <- colorFactor("RdBu",levels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), 
                       ordered = TRUE, reverse = F )

labels <- sprintf("<strong>%s</strong><br/>%g ppt change<sup></sup>",
                  EWS.centroids.df$`Neighbourhood name`, round(EWS.centroids.df$`Claimant count rate ppt change (March 2019 - March 2025`,1)) %>% 
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
title <- tags$div(HTML("Claimant count change and deprivation,<br> March 2019 to March 2025, London</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Sources: Claimant Count, ONS; Indices of Multiple Deprivation 2019, MHCLG<br> 
                         Analysis: WPI Economics on behalf of Trust for London<br>
                         Note: Indices of Multiple Deprivation have been re-based for London"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)

#remove NA
#EWS.centroids.df <- EWS.centroids.df[!is.na(EWS.centroids.df$`Change decile (1 = low)`),]
#EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.dfXT <- SharedData$new(EWS.centroids.df, group = "1") 






#map element
m2covid <- leaflet(EWS.centroids.dfXT, height = "580px", options = leafletOptions(padding = 100, zoomSnap = 0.25, zoomDelta = 0.3)) %>% setView(-0.035,51.489, 10.2) %>% 
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
  
  #addPolygons(data = NUTS1, color = "white",opacity = 1, fillColor = "white", fillOpacity = 1)  %>%        #add london clip
  
  addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("+1ppts","+5ppts","+7ppts"),
                  
                  sizes = c(2.5,4.5,7)*2, position = "bottomright" , title = "CC change <br> 2019-2025<br>&nbsp") %>% 
  
  addLegend(pal = factpal, values = EWS.centroids.df$`IMD decile London (1 is most deprived)`, 
            labels = levels(EWS.centroids.df$`IMD decile London (1 is most deprived)`), position = "bottomright", title = "IMD deciles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2covid





combo <- htmltools::tagList(title, m2covid, tbl,sources) #I think this makes a combined html object
#browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index_covid.html") #this saves it as an HTML page in the default folder.
#htmlwidgets::saveWidget(combo,file =  "Updatedmapwidget.html", selfcontained = T)



############################################
################## stats and analysis ######
############################################

cc <- readRDS("cc2_2025-04-28.rds")


cc[,c(2,4,6,8)] <- sapply(cc[,c(2,4,6,8)], function(x){as.numeric(sub(",","",x))})


cc <- cc %>% rename("Population" = `Population `)

t2 <- cc %>%  group_by(`IMD decile London (1 is most deprived)`) %>%
  summarise("total CC 2025" = sum(`Claimant count (March 2025)`), "total CC 2019" = sum(`Claimant count (March 2019)`),
            "totalpop19" = sum(`Population`))



t2$`Claimant Count 2025 rate` <- round((t2$`total CC 2025`/t2$totalpop19)*100,1)
t2$`Claimant Count 2019 rate` <- round((t2$`total CC 2019`/t2$totalpop19)*100,1)
t2$`CC change` <- t2$`Claimant Count 2025 rate` - t2$`Claimant Count 2019 rate`


write.csv(t2, "Average CC change by deprivation London Apr25.csv", row.names = F)

library(highcharter)
source("../LPP-Updates/plots/LPP colours.r")
colourlist <-  comparison.pair

t2$`IMD decile London (1 is most deprived)`[t2$`IMD decile London (1 is most deprived)` == 1] <- "Most deprived"
t2$`IMD decile London (1 is most deprived)`[t2$`IMD decile London (1 is most deprived)` == 10] <- "Least deprived"

plot <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(categories = t2$`IMD decile London (1 is most deprived)`,title = list(text = "")) %>%
  #hc_add_series(data = df$median, name = "Change in the median (average)")%>%
  hc_add_series(data = t2$`CC change`, name = "Percentage point change", dataLabels = list(enabled = T), showInLegend = F)%>%
  hc_colors(colourlist)%>%
   hc_yAxis(title = list(text = "Percentage point change"), gridLineColor = "#ffffff",
            labels = list(enabled = F))%>%
  hc_title(text = "Change in unemployment benefit claim rate, March 2019 - March 2025, by neighbourhood deprivation deciles", align = "left",
           style = list(fontSize ="18px",color = "#0d2e5b",
                        fontFamily = "Arial", fontWeight = "400" ))%>%
  hc_exporting(enabled = T)

plot

title <- "Change in unemployment benefit claim rate, by neighbourhood deprivation deciles (March 2019 to March 2025)"
Note <- "Indices of Multiple Deprivation have been re-based for London"
Data_source <- "Claimant count, ONS; Indices of Multiple Deprivation 2019, MHCLG"
Analysis <- "WPI Economics on behalf of Trust for London"

export_hc(plot, "BLG1.js", as = "is")

