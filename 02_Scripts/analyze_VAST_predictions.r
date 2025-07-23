library(tidyverse)

main.dir <- this.path::here(.. = 1)

# Read in data
VAST <- read.csv(file.path(main.dir, "01_Data", "predict_psu.csv"))
names(VAST)

#complex-level comparisons among strata
b_mean<-tapply(VAST$encounter_prob,VAST$STRATA,mean)
b_sd<-tapply(VAST$encounter_prob,VAST$STRATA,sd)
b_cv<-b_sd/b_mean

# Transform to logit scale, calculate mean, then back-transform
VAST$logit_probs <- qlogis(VAST$encounter_prob)  # or log(p/(1-p))
mean_logit <- tapply(VAST$logit_probs,VAST$STRATA,mean, na.rm = TRUE)
mean_prob <- plogis(mean_logit)  # or exp(x)/(1+exp(x))


p_mean<-tapply(VAST$positive_catch,VAST$STRATA,mean)
p_sd<-tapply(VAST$positive_catch,VAST$STRATA,sd)
p_cv<-p_sd/p_mean

d_mean<-tapply(VAST$density,VAST$STRATA,mean)
d_sd<-tapply(VAST$density,VAST$STRATA,sd)
d_cv<-d_sd/d_mean

#most of the positive catch means and CVs are similar among strata. Most of the variaiton is coming from encounter probability. large varitions, with HB_H_M and  HB_H_S having teh highest probabilities at 0.046725807 and 0.048477647. Being the average among all psus, we might want to focus on sampling psu with higher encounter probabilities rather than random to overcome the low encounter probabilities.  

##Look at species-specific comparisons among strata
b_mean<-tapply(VAST$encounter_prob,list(VAST$species, VAST$STRATA),mean)
b_sd<-tapply(VAST$encounter_prob,list(VAST$species, VAST$STRATA),sd)
b_cv<-b_sd/b_mean

#apply logit transform
mean_logit <- tapply(VAST$logit_probs,list(VAST$species, VAST$STRATA),mean, na.rm = TRUE)
mean_prob <- plogis(mean_logit)  # or exp(x)/(1+exp(x))

write.csv(mean_prob,file = "strata_encounter_probs.csv")

#probability increase to 0.19 for paka in HB_H_S

p_mean<-tapply(VAST$positive_catch,list(VAST$species, VAST$STRATA),mean)
p_sd<-tapply(VAST$positive_catch,list(VAST$species, VAST$STRATA),sd)
p_cv<-p_sd/p_mean
#again , less differences among strata in positive process by species


d_mean<-tapply(VAST$density,list(VAST$species, VAST$STRATA),mean)
d_sd<-tapply(VAST$density,list(VAST$species, VAST$STRATA),sd)
d_cv<-d_sd/d_mean

#Encounter probabilities are driving the outcome, so look for areas with high probabilities of encounter

# look at encounter probability by psu
library(sf)           # For working with spatial data
library(ggplot2)      # For plotting
library(dplyr)        # For data manipulation
library(viridis)      # For color scales (optional)
library(RColorBrewer)


HI <- st_read("C:\\Users\\John.Syslo\\Documents\\BFISH RnD\\Shapefiles\\BFISH_PSU.shp")
  
#merge with VAST data - remember VAST is replicated by species and year, so select one at  a time. Paka first
VAST_paka<-VAST[VAST$species=="prfi" & VAST$year==2022,]

merged_data <- HI %>%
  left_join(VAST_paka, by = "PSU")


ggplot(merged_data) +
  geom_sf(aes(fill = encounter_prob), color = NA) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(title = "Opakapaka - Spectral Palette",
       fill = "Encounter probability")

ggsave("opakapaka_mhi.png", width = 10, height = 8, dpi = 300)

#Zoom in on Maui Nui for greater detail
VAST_paka_Maui<-VAST_paka[VAST_paka$Island=="Maui Nui",]
HI_maui<-HI[HI$Island=="Maui Nui",]
merged_data_Maui <- HI_maui %>%
  left_join(VAST_paka_Maui, by = "PSU")


ggplot(merged_data_Maui) +
  geom_sf(aes(fill = encounter_prob), color = NA) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(title = "Opakapaka - Maui Nui",
       fill = "Encounter probability")

ggsave("opakapaka_maui.png", width = 10, height = 8, dpi = 300)


#Zoom in on Oahu for greater detail
VAST_paka_Oahu<-VAST_paka[VAST_paka$Island=="Oahu",]
HI_oahu<-HI[HI$Island=="Oahu",]
merged_data_Oahu <- HI_oahu %>%
  left_join(VAST_paka_Oahu, by = "PSU")


ggplot(merged_data_Oahu) +
  geom_sf(aes(fill = encounter_prob), color = NA) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(title = "Opakapaka - Oahu",
       fill = "Encounter probability")

ggsave("opakapaka_oahu.png", width = 10, height = 8, dpi = 300)

#Zoom in on Big Island for greater detail
VAST_paka_Big_Island<-VAST_paka[VAST_paka$Island=="Big Island",]
HI_Big_Island<-HI[HI$Island=="Big Island",]
merged_data_Big_Island <- HI_Big_Island %>%
  left_join(VAST_paka_Big_Island, by = "PSU")

ggplot(merged_data_Big_Island) +
  geom_sf(aes(fill = encounter_prob), color = NA) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(title = "Opakapaka - Big_Island",
       fill = "Encounter probability")

ggsave("opakapaka_Big_Island.png", width = 10, height = 8, dpi = 300)

#Zoom in on Kauai for greater detail
VAST_paka_Kauai<-VAST_paka[VAST_paka$Island=="Kauai",]
HI_Kauai<-HI[HI$Island=="Kauai",]
merged_data_Kauai <- HI_Kauai %>%
  left_join(VAST_paka_Kauai, by = "PSU")

ggplot(merged_data_Kauai) +
  geom_sf(aes(fill = encounter_prob), color = NA) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(title = "Opakapaka - Kauai",
       fill = "Encounter probability")

ggsave("opakapaka_Kauai.png", width = 10, height = 8, dpi = 300)


#################################################################
#HB_H_S  has the best encounter probability for paka at 0.19
#How is it distributed?

ggplot(HI) +
  geom_sf(aes(fill = factor(STRATA)), color = NA) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(title = "MHI BFISH Domain",
       fill = "Strata")

p_HI<-ggplot(HI) +
  geom_sf(aes(fill = factor(STRATA)), color = NA, size = 0.1) +
  scale_fill_manual(values = c(
    "HB_H_D" = "#8B4513",
    "HB_H_M" = "#CD853F", 
    "HB_H_S" = "#FF0000",  # Bright red to stand out
    "HB_L_D" = "#DEB887",
    "HB_L_M" = "#F5DEB3",
    "HB_L_S" = "#F0E68C",
    "SB_A_D" = "#98FB98",
    "SB_A_M" = "#20B2AA",
    "SB_A_S" = "#4682B4",
    "SB_H_S" = "#483D8B"
  )) +
  theme_minimal() +
  labs(title = "MHI BFISH Domain",
       fill = "Strata")

ggsave("strata_map_MHI.png", plot = p_HI, width = 10, height = 6, dpi = 300)

#what is the proportion of overall domain comprised by each stratum?
strat<-tapply(HI$OBJECTID,HI$STRATA,length)
prop_strat<-strat/nrow(HI)
plot(prop_strat)

#Convert prop_strat to data frame
prop_strat_df <- data.frame(
  Strata = names(prop_strat),
  Proportion = as.numeric(prop_strat)
)

# Create barplot with matching colors from Option 1
p<-ggplot(prop_strat_df, aes(x = Strata, y = Proportion, fill = Strata)) +
  geom_col(color = "white", size = 0.2) +
  scale_fill_manual(values = c(
    "HB_H_D" = "#8B4513",
    "HB_H_M" = "#CD853F", 
    "HB_H_S" = "#FF0000",  # Bright red to stand out
    "HB_L_D" = "#DEB887",
    "HB_L_M" = "#F5DEB3",
    "HB_L_S" = "#F0E68C",
    "SB_A_D" = "#98FB98",
    "SB_A_M" = "#20B2AA",
    "SB_A_S" = "#4682B4",
    "SB_H_S" = "#483D8B"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Proportion by Strata",
    x = "Strata",
    y = "Proportion",
    fill = "Strata"
  ) +
  scale_y_continuous(labels = scales::percent_format())

# Save the plot as PNG
ggsave("strata_proportions.png", plot = p, width = 10, height = 6, dpi = 300)
