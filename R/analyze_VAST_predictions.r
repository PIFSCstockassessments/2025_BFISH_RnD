library(tidyverse)

main.dir <- this.path::here(.. = 1)

# Read in data
VAST <- read.csv(file.path(main.dir, "01_Data", "predict_psu.csv"))
names(VAST)

#complex-level comparisons among strata
b_mean<-tapply(VAST$encounter_prob,VAST$STRATA,mean)
b_sd<-tapply(VAST$encounter_prob,VAST$STRATA,sd)
b_cv<-b_sd/b_mean

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

#probability increase to 0.24 for paka in HB_H_S

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

