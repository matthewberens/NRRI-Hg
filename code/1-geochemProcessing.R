#############################################
# NRRI Hg Incubation Geochem Processing
# Name: Matthew Berens
# Updated: 7/18/2025
#############################################

#Load libraries and set directory
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

source("code/0_.R")

#Import Data-----------------------------------------------------
geochem_data = read.csv("data/geochemRaw.csv")


#Methylation Kinetics--------------------------------------------
methylation_kinetics = geochem_data %>%
  filter(parameter %in% c("Hg201")) %>%
  select(constituent, parameter, carbon, voltage, day, replicate, result_value) %>%
  unite(ID, c("constituent", "parameter", "day")) %>%
  pivot_wider(names_from = "ID", values_from = "result_value") %>%
  select(!c("STHG_Hg201_1", "STHG_Hg201_14")) %>%
  mutate(kmeth = -log((SMHG_Hg201_14 - SMHG_Hg201_0)/STHG_Hg201_0)/14) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

meth %>%
  ggplot(aes(x = carbon, y = kmeth, fill = voltage)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3) 
  scale_fill_manual(values = pnw_palette("Bay", 2))



#Demethylation Kinetics--------------------------------------------
demethylation_kinetics = geochem_data %>%
  filter(parameter %in% c("Hg204")) %>%
  select(constituent, parameter, carbon, voltage, day, replicate, result_value) %>%
  unite(ID, c("constituent", "parameter", "day")) %>%
  pivot_wider(names_from = "ID", values_from = "result_value") %>%
  select(!c("STHG_Hg204_1", "SMHG_Hg204_1")) %>%
  mutate(kdem = -log((SMHG_Hg204_14/STHG_Hg204_14)/(SMHG_Hg204_0/STHG_Hg204_0))/14) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

demethylation_kinetics %>%
  ggplot(aes(x = carbon, y = kdem, fill = voltage)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3)


#Sulfate Kinetics--------------------------------------------
sulfur = raw %>%
  subset(constituent == "STHG") %>%
  subset(parameter %in% c("sulfate", "elementalS", "total_S")) %>%
  select(parameter, carbon, voltage, day, replicate, result_value) %>%
  unite(ID, c("parameter", "day")) %>%
  pivot_wider(names_from = "ID", values_from = "result_value") %>%
  mutate(ksulfate = -log(sulfate_14/sulfate_0)/14,
         ktotal = log(total_S_14/total_S_0)/14,
         kelemental = log(elementalS_14/elementalS_0)/14) %>%
  mutate_all(~ifelse(is.nan(.), NA, .)) 


sulfur %>%
  ggplot(aes(x = carbon, y = ksulfate, fill = voltage)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3) +
  theme_mb1() +
  scale_fill_manual(values = pnw_palette("Bay", 2))

############################################################
## SUMMARY OF RATES ########################################
############################################################

summary_rates =
  merge(meth, dem, by = c("carbon", "voltage", "replicate")) %>%
  merge(sulfur, by = c("carbon", "voltage", "replicate")) %>%
  mutate(carbon = recode(carbon, A0 = 'nocarbon', A1 = 'carbon')) %>%
  mutate(voltage = recode(voltage, V0 = 'novoltage', V1 = 'voltage')) %>%
  select(voltage, carbon, replicate, kmeth, kdem, ksulfate, ktotal, kelemental)


summary_rates %>%
  ggplot(aes(x=kdem, y = kmeth, fill = voltage)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3) +
  facet_grid(~carbon) +
  theme_mb1() +
  scale_fill_manual(values = pnw_palette("Bay", 2))
