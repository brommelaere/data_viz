rm(list=ls()) # Removes all objects from the current workspace (R memory)
options(scipen=999) # Do not print with Scientific Notation

# Program Details: Overview -----------------------------------------------
# *************************************************************************

script.meta <- list(
  
  ##
  Programmer   = "Ben Rommelaere",
  Project				 = "CIMT Energy Visualization",
  Program      = "",
  Version      = 1,
  Date_Created = "12/26/2019",
  Last_Updated = "02/16/2020",
  
  Description  = 
    
    "Load csvs & combine into dataset for analysis",
  
  Notes        = 
    
    "Saves completed data to base folder"
  
)

# *************************************************************************

# packages ----------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(forcats)
library(stringr)
library(magrittr)
library(scales)
library(dplyr)

# Paths -------------------------------------------------------------------
main <- "/Users/benrommelaere/Desktop/Data Science/data_viz/cimt_imports" #change this to local repo location
untouched <- file.path(main, "01 Untouched")
raw <- file.path(main, "02 Raw/Trade-Imports-Yr2017-2019/")
base <- file.path(main, "03 Base")
temp <- file.path(main, "04 Intermediate")
output <- file.path(main, "05 Output")

# ----------------------------------------------------------
# 1: Putting All the Data Together
# ----------------------------------------------------------
ch27.df <- read.csv(file = file.path(raw, str_c("Trade-Imports-Chp-27", ".csv", sep="")),
                       header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") 

codes.df <- read.csv(file = file.path(raw, str_c("Trade-Metadata-Imports", ".csv", sep="")),
                    header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  dplyr::rename(hs_code = code) %>% 
  select(-c('start_date', 'end_date'))

country.df <- read.csv(file = file.path(raw, str_c("country_id", ".csv", sep="")),
                     header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  dplyr::rename(country_id = country_code) %>% 
  distinct()

prov.df <- read.csv(file = file.path(raw, str_c("prov_id", ".csv", sep="")),
                     header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  dplyr::rename(geo_id = province_code)

state.df <- read.csv(file = file.path(raw, str_c("state_id", ".csv", sep="")),
                     header = T, sep = ",", stringsAsFactors=FALSE, na.strings = "NA") %>% 
  dplyr::rename(state_id = state_code)

base.df <- ch27.df %>% 
  left_join(codes.df, by = "hs_code") %>% 
  left_join(country.df, by = "country_id") %>% 
  left_join(prov.df, by = "geo_id") %>% 
  left_join(state.df, by = "state_id") %>% 
  filter(country_id!=999,
         !(country_id==9 & state_id==1000)) %>% 
  mutate(trading_partner = ifelse(country_label=="United States", state, country_label),
         price = value/quantity)
 
save(base.df,
     file = file.path(base, str_c("ch27-energy-data", ".Rda", sep="")))
