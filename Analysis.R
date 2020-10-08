#Look at claimant count chages by deprivation decile pre/post COVID19

library(nomisr)
library(tidyverse)

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


######################## Create one dataset ###############################################

cc <- merge(old, new, by = "area")

cc <- merge(cc, population, by = "area", all.x = T)

########################### Calculate rates ########################################

cc$old_rate <- (cc$claimant_count_old/cc$population)*100

cc$new_rate <- (cc$claimant_count_new/cc$population)*100


cc$`Claimant count rate ppt change (August 2017 - August 2020)` <- cc$new_rate - cc$old_rate

cc$old_rate <- round(cc$old_rate,1)
cc$new_rate <- round(cc$new_rate,1)
cc$`Claimant count rate ppt change (August 2017 - August 2020)` <- round(cc$`Claimant count rate ppt change (August 2017 - August 2020)`,1)

cc$"Change decile (1 = low)" <- ntile(cc$`Claimant count rate ppt change (August 2017 - August 2020)`,10)
cc$"Change decile (1 = low)" <- as.factor(cc$"Change decile (1 = low)")

#make nice names for the columns
cc <- cc %>% rename("lsoa11cd" = area, 
                    "Claimant count (August 2017)" = claimant_count_old, 
                    "Claimant count (August 2020)" = claimant_count_new, 
                    "Population 2018" = population, 
                    "Claimant count rate % (August 2017)" = old_rate, 
                    "Claimant count rate % (August 2020)" = new_rate
)

#format the data
cc$`Claimant count (August 2017)` <- format(cc$`Claimant count (August 2017)`, big.mark = ",")
cc$`Claimant count (August 2020)` <- format(cc$`Claimant count (August 2020)`, big.mark = ",")
cc$`Population 2018` <- format(cc$`Population 2018`, big.mark = ",")
