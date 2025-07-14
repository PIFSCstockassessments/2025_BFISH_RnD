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
psu_lookup <- read.csv(file.path(main.dir, "01_Data", "BFISH PSU lookup table.csv"))
index_dt <- read.csv(file.path(main.dir, "01_Data", "index_dt.csv"))


# # Get species ID info for Deep7 
# deep7 <- c("Gindai", "Kalekale", "Ehu", "Onaga", "Hahapuupuu", "Opakapaka", "Lehi")

# deep7_id <- sps_lookup %>%
# filter(COMMON_NAME %in% deep7) %>% 
# select(X, SPECIES_CD, SCIENTIFIC_NAME, COMMON_NAME)

# # Filter catch data for Deep7 species only
# deep7_catch <- CRF_CATCH %>% 
# filter(SPECIES_CD %in% deep7_id$SPECIES_CD)

# deep7_samples <- CRF_SAMPLE %>% 
# filter(SAMPLE_ID %in% deep7_catch$SAMPLE_ID) %>% 
# select(BFISH, SAMPLE_ID, SAMPLE_DATE, TARGET_GRID_ID, PSU, SAMPLE_MEAN_DEPTH_M)

# # Combine sample and PSU info with catch info
# deep7_catch <- deep7_catch %>% 
# left_join(deep7_samples, by = "SAMPLE_ID") %>%
# left_join(psu_lookup, by = "PSU")

# Opaka grids
opaka_rf_cvs <- index_dt %>% 
filter(gears_used == "fishing" & model_number == 74) %>% 
mutate(time = time + 1) %>% 
pull(cv) %>% 
round(., digits = 2)

grids_deep7 <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(YEAR) %>% 
summarise(Num_grids = n_distinct(PSU), 
Grids_per_BI = n_distinct(PSU[Island == "Big Island"]),
Grids_per_Maui = n_distinct(PSU[Island == "Maui Nui"]),
Grids_per_Oahu = n_distinct(PSU[Island == "Oahu"]),
Grids_per_Niihau = n_distinct(PSU[Island == "Niihau"]),
Grids_per_Kauai = n_distinct(PSU[Island == "Kauai"]),
Grids_per_HB_H_D = n_distinct(PSU[STRATA == "HB_H_D"]),
Grids_per_HB_H_M = n_distinct(PSU[STRATA == "HB_H_M"]),
Grids_per_HB_H_S = n_distinct(PSU[STRATA == "HB_H_S"]),
Grids_per_HB_L_S = n_distinct(PSU[STRATA == "HB_L_S"]),
Grids_per_SB_A_S = n_distinct(PSU[STRATA == "SB_A_S"]),
Grids_per_HB_L_D = n_distinct(PSU[STRATA == "HB_L_D"]),
Grids_per_SB_A_D = n_distinct(PSU[STRATA == "SB_A_D"]),
Grids_per_HB_L_M = n_distinct(PSU[STRATA == "HB_L_M"]),
Grids_per_SB_A_M = n_distinct(PSU[STRATA == "SB_A_M"]),
Grids_per_SB_H_S = n_distinct(PSU[STRATA == "SB_H_S"])
) %>%
mutate(CV_deep7 = c(0.17, 0.15, 0.17, 0.16, 0.14, 0.13, 0.12, NA, NA), 
CV_opaka_rf = c(opaka_rf_cvs, NA, NA)) #from Table 11 in BFISH stock assessment report

grids_deep7 %>%
ggplot(aes(x = Num_grids, y = CV_deep7)) + 
geom_point() +
ylim(0,.4) +
xlab("Number of grids") +
ylab("CV for Deep7 estimates") +
theme_bw()
ggsave(file.path(main.dir, "03_Outputs", "deep7_gridsvsCV.png"))


grids_deep7 %>%
ggplot(aes(x = Num_grids, y = CV_opaka_rf)) + 
geom_point() +
ylim(0,.4) +
xlab("Number of grids") +
ylab("CV for Opaka estimates") +
theme_bw()
ggsave(file.path(main.dir, "03_Outputs", "opaka_gridsvsCV.png"))

write.csv(grids_deep7, file.path("03_Outputs", "grid_counts_deep7.csv"), row.names = F)



