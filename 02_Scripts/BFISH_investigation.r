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
CAM_LEN <- read.csv(file.path(main.dir, "01_Data", "CAM_LENGTHS.csv"))
CAM_SAMPLE <- read.csv(file.path(main.dir, "01_Data", "CAM_SAMPLE.csv"))

# Get species ID info for Deep7 
deep7 <- c("Gindai", "Kalekale", "Ehu", "Onaga", "Hahapuupuu", "Opakapaka", "Lehi")

deep7_id <- sps_lookup %>%
filter(COMMON_NAME %in% deep7) %>% 
select(X, SPECIES_CD, SCIENTIFIC_NAME, COMMON_NAME)

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
geom_smooth(method = lm, se = FALSE) +
ylim(0,.4) +
xlab("Number of grids") +
ylab("CV for Opaka estimates") +
theme_bw()
ggsave(file.path(main.dir, "03_Outputs", "opaka_gridsvsCV.png"))

write.csv(grids_deep7, file.path("03_Outputs", "grid_counts_deep7.csv"), row.names = F)

CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(YEAR) %>% 
summarise(n_samples = n_distinct(PSU),
n_positives_deep7 = n_distinct(PSU[SPECIES_CD %in% deep7_id$SPECIES_CD]),
prop_zeros = (n_samples - n_positives_deep7)/n_samples)


prop_zero_yr <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(YEAR) %>% 
summarise(n_samples = n_distinct(PSU),
n_positives_deep7 = n_distinct(PSU[SPECIES_CD %in% deep7_id$SPECIES_CD]),
prop_zeros = (n_samples - n_positives_deep7)/n_samples)

# Calculate the proportion of zero deep7 catch for each PSU
prop_zero_psu <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(PSU, SAMPLE_ID) %>% 
mutate(
    # Create a logical flag: TRUE if the SPECIES_CD is a Deep 7 species, FALSE otherwise
    is_deep7_species = SPECIES_CD %in% deep7_id$SPECIES_CD
  ) %>%
  # Summarize at the SAMPLE_ID level to get one row per sample, indicating if it had a deep7 catch
  summarise(
    # 'any(is_deep7_species)' returns TRUE if at least one deep7 species was caught in this sample, FALSE otherwise
    sample_had_deep7_catch = any(is_deep7_species),
    .groups = 'drop' # This drops the grouping by SAMPLE_ID, so the next group_by can work correctly
  ) %>% 
  # Now group by PSU to calculate the proportion of zero-catch samples within each PSU
  group_by(PSU) %>%
  summarise(
    # Count the total number of distinct samples within this PSU
    total_samples_in_psu = n(),
    # Count how many of these samples had at least one deep7 catch
    samples_with_deep7_catch = sum(sample_had_deep7_catch),
    # Calculate the proportion of samples that had zero deep7 catch
    prop_zeros_deep7 = ((total_samples_in_psu - samples_with_deep7_catch) / total_samples_in_psu) * 100
  ) %>%
  # Arrange the results to show PSUs with the lowest proportion of zeros on top
  arrange(desc(total_samples_in_psu), prop_zeros_deep7)

write.csv(prop_zero_psu, file.path("03_Outputs", "prop_zero_by_psu_deep7.csv"), row.names = F)

# Calculate the proportion of zero Opaka catch for each PSU
prop_zero_psu_opaka <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(PSU, SAMPLE_ID) %>% 
mutate(
    # Create a logical flag: TRUE if the SPECIES_CD is a Deep 7 species, FALSE otherwise
    is_opaka_species = SPECIES_CD == "PRFI"
  ) %>%
  # Summarize at the SAMPLE_ID level to get one row per sample, indicating if it had a opaka catch
  summarise(
    # 'any(is_opaka_species)' returns TRUE if at least one opaka species was caught in this sample, FALSE otherwise
    sample_had_opaka_catch = any(is_opaka_species),
    .groups = 'drop' # This drops the grouping by SAMPLE_ID, so the next group_by can work correctly
  ) %>% 
  # Now group by PSU to calculate the proportion of zero-catch samples within each PSU
  group_by(PSU) %>%
  summarise(
    # Count the total number of distinct samples within this PSU
    total_samples_in_psu = n(),
    # Count how many of these samples had at least one opaka catch
    samples_with_opaka_catch = sum(sample_had_opaka_catch),
    # Calculate the proportion of samples that had zero opaka catch
    prop_zeros_opaka = ((total_samples_in_psu - samples_with_opaka_catch) / total_samples_in_psu) * 100
  ) %>%
  # Arrange the results to show PSUs with the lowest proportion of zeros on top
  arrange(desc(total_samples_in_psu), prop_zeros_opaka)

write.csv(prop_zero_psu_opaka, file.path("03_Outputs", "prop_zero_by_psu_opaka.csv"), row.names = F)

# Calculate the proportion of zero Deep7 catch for each strata
prop_zero_strata <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(STRATA, SAMPLE_ID) %>% 
mutate(
    # Create a logical flag: TRUE if the SPECIES_CD is a Deep 7 species, FALSE otherwise
    is_deep7_species = SPECIES_CD %in% deep7_id$SPECIES_CD
  ) %>%
  # Summarize at the SAMPLE_ID level to get one row per sample, indicating if it had a deep7 catch
  summarise(
    # 'any(is_deep7_species)' returns TRUE if at least one deep7 species was caught in this sample, FALSE otherwise
    sample_had_deep7_catch = any(is_deep7_species),
    .groups = 'drop' # This drops the grouping by SAMPLE_ID, so the next group_by can work correctly
  ) %>% 
  # Now group by PSU to calculate the proportion of zero-catch samples within each PSU
  group_by(STRATA) %>%
  summarise(
    # Count the total number of distinct samples within this PSU
    total_samples_in_strata = n(),
    # Count how many of these samples had at least one deep7 catch
    samples_with_deep7_catch = sum(sample_had_deep7_catch),
    # Calculate the proportion of samples that had zero deep7 catch
    prop_zeros_deep7 = ((total_samples_in_strata - samples_with_deep7_catch) / total_samples_in_strata) * 100
  ) %>%
  # Arrange the results to show PSUs with the lowest proportion of zeros on top
  arrange(prop_zeros_deep7)

write.csv(prop_zero_strata, file.path("03_Outputs", "prop_zero_by_strata_deep7.csv"), row.names = F)

# Calculate the proportion of zero Opaka catch for each strata
prop_zero_strata_paka <- CRF_SAMPLE %>% 
left_join(psu_lookup, by = "PSU") %>%
left_join(CRF_CATCH, by = "SAMPLE_ID") %>% 
select(BFISH.x, SAMPLE_ID, SAMPLE_DATE, PSU, Island, STRATA, SPECIES_CD, LENGTH_CM) %>% 
mutate(YEAR = lubridate::year(SAMPLE_DATE), 
MONTH = lubridate::month(SAMPLE_DATE), 
DAY = lubridate::day(SAMPLE_DATE)) %>% 
group_by(STRATA, SAMPLE_ID) %>% 
mutate(
    # Create a logical flag: TRUE if the SPECIES_CD is a Deep 7 species, FALSE otherwise
    is_opaka = SPECIES_CD == "PRFI"
  ) %>%
  # Summarize at the SAMPLE_ID level to get one row per sample, indicating if it had a deep7 catch
  summarise(
    # 'any(is_deep7_species)' returns TRUE if at least one deep7 species was caught in this sample, FALSE otherwise
    sample_had_opaka_catch = any(is_opaka),
    .groups = 'drop' # This drops the grouping by SAMPLE_ID, so the next group_by can work correctly
  ) %>% 
  # Now group by PSU to calculate the proportion of zero-catch samples within each PSU
  group_by(STRATA) %>%
  summarise(
    # Count the total number of distinct samples within this PSU
    total_samples_in_strata = n(),
    # Count how many of these samples had at least one deep7 catch
    samples_with_opaka_catch = sum(sample_had_opaka_catch),
    # Calculate the proportion of samples that had zero deep7 catch
    prop_zeros_opaka = ((total_samples_in_strata - samples_with_opaka_catch) / total_samples_in_strata) * 100
  ) %>%
  # Arrange the results to show PSUs with the lowest proportion of zeros on top
  arrange(prop_zeros_opaka)

write.csv(prop_zero_strata_paka, file.path("03_Outputs", "prop_zero_by_strata_opaka.csv"), row.names = F)

cv_mod <- lm(CV_opaka_rf ~ Num_grids, data = grids_deep7)
new <- data.frame(Num_grids = seq(300,600,by=25))
new$predicted_cv <- round(predict(cv_mod, new), 2)

## Size data investigation 

# Filter catch data for Deep7 species only
deep7_catch <- CRF_CATCH %>% 
filter(SPECIES_CD %in% deep7_id$SPECIES_CD)

deep7_samples <- CRF_SAMPLE %>% 
filter(SAMPLE_ID %in% deep7_catch$SAMPLE_ID) %>% 
select(BFISH, SAMPLE_ID, SAMPLE_DATE, TARGET_GRID_ID, PSU, SAMPLE_MEAN_DEPTH_M)

# Combine sample and PSU info with catch info
deep7_catch <- deep7_catch %>% 
left_join(deep7_samples, by = "SAMPLE_ID") %>%
left_join(psu_lookup, by = "PSU") %>%
mutate(Gear_type = "fishing")

deep7_cam_lengths <- CAM_LEN %>%
filter(SPECIES_CD %in% deep7_id$SPECIES_CD)

deep7_cam_samples <- CAM_SAMPLE %>%
filter(DROP_CD %in% deep7_cam_lengths$DROP_CD) %>% 
select(DROP_CD, PSU)

deep7_cam_lengths <- deep7_cam_lengths %>% 
left_join(deep7_cam_samples, by = "DROP_CD") %>%
left_join(psu_lookup, by = "PSU") %>%
mutate(LENGTH_CM = MEAN_MM/10,
Gear_type = "camera")

deep7_rf <- deep7_catch %>%
select(SAMPLE_ID, SPECIES_CD, COMMON_NAME, LENGTH_CM, PSU, Island, STRATA, Gear_type) 


strata_mean_len <- deep7_cam_lengths %>% 
select(DROP_CD, SPECIES_CD, COMMON_NAME, LENGTH_CM, PSU, Island, STRATA, Gear_type) %>%
bind_rows(deep7_rf) %>% 
group_by(SPECIES_CD, STRATA) %>%
summarise(mean_length = mean(LENGTH_CM, na.rm =T),
SD = sd(LENGTH_CM, na.rm = T),
N = n()) 

strata_mean_len %>%
ggplot(aes(x = SPECIES_CD, y = mean_length)) + 
geom_col(aes(fill = SPECIES_CD)) + 
facet_wrap(~STRATA) +
labs(x = "Species", y = "Mean Length (cm)") +
theme_bw()
ggsave(file.path(main.dir, "03_Outputs", "mean_length_strata_sps.png"))
