# Code to look at the BFISH catch from research fishing

#When running this code, use renv::restore() to install any needed packages
#If you have added any packages, run renv::snapshot() and make sure 
#to push the changes to the repo
library(tidyverse)

main.dir <- this.path::here(.. = 1)

# Read in data
CRF_CATCH <- read.csv(file.path(main.dir, "01_Data", "CRF_CATCH.csv"))
CRF_DRIFT <- read.csv(file.path(main.dir, "01_Data", "CRF_DRIFT.csv"))
sps_lookup <- read.csv(file.path(main.dir, "01_Data", "sps_lookup.csv"))
CRF_SAMPLE <- read.csv(file.path(main.dir, "01_Data", "CRF_SAMPLE.csv"))

# Get species ID info for Deep7 
deep7 <- c("Gindai", "Kalekale", "Ehu", "Onaga", "Hahapuupuu", "Opakapaka", "Lehi")

deep7_id <- sps_lookup %>%
filter(COMMON_NAME %in% deep7) %>% 
select(X, SPECIES_CD, SCIENTIFIC_NAME, COMMON_NAME)

# Filter catch data for Deep7 species only
deep7_catch <- CRF_CATCH %>% 
filter(SPECIES_CD %in% deep7_id$SPECIES_CD)
