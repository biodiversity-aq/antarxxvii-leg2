library(tidyverse)
library(here)

#############
# Read files
#############
event_raw <- read_delim("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaLeNVUFFmxko3fTNevRVQ6b9_4InQLLDiZBdhsiI_m04uK9azEg7WKSo6bd-ZOA/pub?gid=746365751&single=true&output=tsv", delim = "\t")
occ_raw <- read_delim("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaLeNVUFFmxko3fTNevRVQ6b9_4InQLLDiZBdhsiI_m04uK9azEg7WKSo6bd-ZOA/pub?gid=2141931064&single=true&output=tsv", delim = "\t")
icpms_conc_raw <- read_delim("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaLeNVUFFmxko3fTNevRVQ6b9_4InQLLDiZBdhsiI_m04uK9azEg7WKSo6bd-ZOA/pub?gid=2020900842&single=true&output=tsv", delim = "\t")
icpms_prec_raw <- read_delim("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaLeNVUFFmxko3fTNevRVQ6b9_4InQLLDiZBdhsiI_m04uK9azEg7WKSo6bd-ZOA/pub?gid=553900765&single=true&output=tsv", delim = "\t")
fpxrf_raw <- read_delim("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaLeNVUFFmxko3fTNevRVQ6b9_4InQLLDiZBdhsiI_m04uK9azEg7WKSo6bd-ZOA/pub?gid=1476475874&single=true&output=tsv", delim = "\t")

###########
# Event
###########
# drop columns that are not useful in events
event <- event_raw %>%
  select(-matches("Nb of|DNA bottles|Pictures")) %>%
  # cast to character because different types cannot be pivoted into the same column
  mutate(
    `Number of replicates` = as.character(`Number of replicates`),
    `Time start` = as.character(`Time start`),
    `Time start dredging` = as.character(`Time start dredging`),
    `Time stop dredging` = as.character(`Time stop dredging`),
    `Time out of the water` = as.character(`Time out of the water`),
    `Total dredging time` = as.character(`Total dredging time`),
    `Speed of the ship` = as.character(`Speed of the ship`),
    # round values to 4 decimal places
    decimalLatitude = sprintf("%.3f", decimalLatitude),
    decimalLongitude = sprintf("%.3f", decimalLongitude)
    )

##############
# Occurrence 
##############
occ <- occ_raw %>%
  select(-c(`id`, 
            `Type`, 
            `Person`, 
            `Expedition and leg`,
            `#Organism`,  # copied to organismID
            `Name series`,  # in eventID
            `Number station`,  # in eventID
            `Transport`, 
            `Volume of ethanol (ml)`,
            `Volume/weight`,  # not measurement of the organisms
            `Content`,  # copied to occurrenceRemarks
            `#Bottle`))

##########
# eMoF
##########

# events emof
event_emof <- event %>%
  pivot_longer(
    cols = c("Number of replicates",
             "Surface sediments",
             "sediment description",
             "potential pollution",
             "Time start",
             "Time start dredging",
             "Time stop dredging",
             "Time out of the water",
             "Total dredging time",
             "Speed of the ship"
    ),
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>%
  mutate(
    measurementUnit = case_when(
      measurementType == "Speed of the ship" ~ paste("nautical miles"),
      measurementType == "Total dredging time" ~ paste("minutes"),
    ),
    measurementAccuracy = NA,
    measurementMethod = NA
  ) %>%
  # only select relevant columns for emof table
  select(
    eventID,
    measurementType,
    measurementValue,
    measurementUnit,
    measurementAccuracy,
    measurementMethod
  )

# ICP-MS concentration table
icpms_conc <- icpms_conc_raw %>%
  pivot_longer(
    cols = -eventID,
    names_to = "measurementType",
    values_to = "measurementValue"
  ) %>%
  mutate(
    # because event_emof contains measurementValues that are string, need to cast it to string in order to bind_row
    measurementValue = as.character(measurementValue),  
    measurementUnit = "µg/g",
    measurementMethod = "https://doi.org/10.1016/j.marpolbul.2022.114501"
    )

# ICP-MS precision table, field should goes to measurementAccuracy
icpms_prec <- icpms_prec_raw %>%
  pivot_longer(
    cols = -eventID,
    names_to = "element",
    values_to = "measurementAccuracy"
  ) 

# ICP-MS emof table
# Merge the long datasets based on eventID and matching element types
icpms <- icpms_conc %>%
  mutate(element = case_when(
    grepl("Al ", measurementType) ~ "Al",
    grepl("As ", measurementType) ~ "As",
    grepl("Cd ", measurementType) ~ "Cd",
    grepl("Co ", measurementType) ~ "Co",
    grepl("Cr ", measurementType) ~ "Cr",
    grepl("Cu ", measurementType) ~ "Cu",
    grepl("Fe ", measurementType) ~ "Fe",
    grepl("Hg ", measurementType) ~ "Hg",
    grepl("Mn ", measurementType) ~ "Mn",
    grepl("Ni ", measurementType) ~ "Ni",
    grepl("Pb ", measurementType) ~ "Pb",
    grepl("V ", measurementType) ~ "V",
    grepl("Zn ", measurementType) ~ "Zn",
    TRUE ~ NA_character_
  )) %>%
  left_join(icpms_prec, by = c("eventID", "element")) %>%
  mutate(measurementAccuracy = paste("combined precision (±1sd)", measurementAccuracy)) %>%
  select(eventID, measurementType, measurementValue, measurementUnit, measurementAccuracy, measurementMethod)

# FP-XRF emof table
fpxrf <- fpxrf_raw %>%
  pivot_longer(
    cols = -eventID,
    names_to = "measurementType",
    values_to = "measurementValue",
    values_drop_na = TRUE
  ) %>%
  mutate(
    # because event_emof contains measurementValues that are string, need to cast it to string in order to bind_row
    measurementValue = as.character(measurementValue),
    measurementUnit = "µg/g",
    measurementMethod = "https://doi.org/10.1016/j.marpolbul.2022.114501",
    measurementAccuracy = NA  # add this field so that we can bind to the ICPMS eMoF table (same number of columns)
  )

# combine ICPMS and FPXRF tables into 1 emof table 
emof <- bind_rows(event_emof, icpms, fpxrf)

# write files
write_delim(event, here("data", "output", "event.txt"), delim = "\t", na = "", escape = "double")
write_delim(occ, here("data", "output", "occurrence.txt"), delim = "\t", na = "", escape = "double")
write_delim(emof, here("data", "output", "emof.txt"), delim = "\t", na = "", escape = "double")
