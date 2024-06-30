install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)

# Step 1, 2 and 3
StormEventsDetails1997 <- read.csv("StormEvents_details-ftp_v1.0_d1997_c20220425.csv", header = TRUE, sep = ",")
subsetStormDetails1997 <- StormEventsDetails1997 %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE) %>%
  arrange(STATE) 

#Step 4
subsetStormDetails1997$STATE <- str_to_title(subsetStormDetails1997$STATE)
subsetStormDetails1997$CZ_NAME <- str_to_title(subsetStormDetails1997$CZ_NAME)

#Step 5
filteredSubsetStormDetails1997 <- subsetStormDetails1997 %>%
  filter(CZ_TYPE == "C") %>%
  select(- CZ_TYPE)

#Step 6
filteredSubsetStormDetails1997$STATE_FIPS <- str_pad(filteredSubsetStormDetails1997$STATE_FIPS, width = 3, side = "left", pad = "0")
filteredSubsetStormDetails1997$CZ_FIPS <- str_pad(filteredSubsetStormDetails1997$CZ_FIPS, width = 3, side = "left", pad = "0")
combinedFIPSStormDetails <- filteredSubsetStormDetails1997 %>%
  unite(fips, STATE_FIPS, CZ_FIPS, sep = "_")

#Step 7
renamedStormDetails <- rename_all(combinedFIPSStormDetails,tolower)

#Step 8
state_info <- data.frame(state = state.name, region = state.region, area = state.area)

#Step 9
NumberEventsStormDetails <- data.frame(table(renamedStormDetails$state))
renamedNumberEventsStormsDetails <- NumberEventsStormDetails %>%
  rename("state" = "Var1", "freq" = "Freq") 
merged <- merge(renamedNumberEventsStormsDetails, state_info, by = "state")

#Step 10

ggplot(merged, aes(x = area, y = freq)) + geom_point(aes(color=region)) + labs(x = "Land area (square miles)", 
                                                                               y = "# of storm events in 1997")

