rm(list=ls()) # Removes all objects from the current workspace (R memory)
options(scipen=999) # Do not print with Scientific Notation

# Program Details: Overview -----------------------------------------------
# *************************************************************************

script.meta <- list(
  
  ##
  Programmer   = "Ben Rommelaere",
  Project				 = "CIMT Energy Chapter",
  Program      = "",
  Version      = 1,
  Date_Created = "12/26/2019",
  Last_Updated = "02/16/2020",
  
  Description  = 
    
    "Create Supply Cost Curves",
  
  Notes        = 
    
    "Saves Charts to Output"
  
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
# 2: Supply Graphs
# ---------------------------------------------------------- 
load(file = file.path(base, str_c("ch27-energy-data", ".Rda", sep="")))

## Natural Gas
nat.gas.df <- base.df %>% 
  filter(hs_code == 271121,
         province == "Canada",
         year==2019 & month==12,
         quantity>0) %>% 
  mutate(trading_partner = ifelse(quantity < 1000, "Other", trading_partner)) %>% 
  dplyr::group_by(trading_partner) %>% 
  dplyr::summarise(quantity = sum(quantity),
                   value = sum(value)) %>% 
  mutate(price = value / quantity) %>% 
  arrange(price) %>% 
  mutate(quantity = quantity/1000,
         right_dfm = cumsum(quantity),
         left_dfm = right_dfm - quantity,
         name1 = fct_reorder(as.factor(trading_partner), right_dfm))

max_x = round_any(max(nat.gas.df$right_dfm), 100, f = ceiling)
max_y = round_any(max(nat.gas.df$price), 1, f = ceiling)

ggplot(nat.gas.df, aes(ymin = 0)) +
  geom_rect(aes(xmin = left_dfm, xmax = right_dfm, ymax = `price`, fill = name1)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, max_x)) +
  labs(y = "Import Price ($/Cubic Meter)",
       x = "Quantity (1000s of Cubic Meters)",
       title = "Natural Gas Import Supply Curve",
       subtitle = "Dec 2019 \n Import Price vs. Quantity Imported",
       fill = "Import Origin: Country/State") +
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        plot.caption = element_text(hjust = 0, size = 6))  +
  scale_y_continuous(limits = c(0, max_y), labels = dollar) +
  scale_fill_brewer(palette = "Set3")

ggsave(file.path(output, str_c("Natural-Gas", ".jpeg", sep="")))
       
## Oil
oil.df <- base.df %>% 
  filter(hs_code == 270900,
         province == "Canada",
         year==2019 & month==12,
         quantity>0) %>% 
  mutate(trading_partner = ifelse(quantity < 100000, "Other", trading_partner)) %>% 
  dplyr::group_by(trading_partner) %>% 
  dplyr::summarise(quantity = sum(as.numeric(quantity)),
                   value = sum(as.numeric(value))) %>% 
  mutate(price = value / (8.38641436*quantity)) %>% 
  arrange(price) %>% 
  mutate(quantity = quantity*8.38641436/31,
         right_dfm = cumsum(quantity),
         left_dfm = right_dfm - quantity,
         name1 = fct_reorder(as.factor(trading_partner), right_dfm))

max_x = round_any(max(oil.df$right_dfm), 100, f = ceiling)
max_y = round_any(max(oil.df$price), 1, f = ceiling)

ggplot(oil.df, aes(ymin = 0)) +
  geom_rect(aes(xmin = left_dfm, xmax = right_dfm, ymax = `price`, fill = name1)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, max_x)) +
  labs(y = "Import Price ($/barrel)",
       x = "Quantity (bpd)",
       title = "Crude Oil Import Supply Curve",
       subtitle = "Dec 2019 Import Price vs. Quantity Imported",
       fill = "Import Origin: Country/State") +
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        plot.caption = element_text(hjust = 0, size = 6))  +
  scale_y_continuous(limits = c(0, max_y), labels = dollar) +
  scale_fill_brewer(palette = "Set3")

ggsave(file.path(output, str_c("Oil", ".jpeg", sep="")))

## Propane
propane.df <- base.df %>% 
  filter(hs_code == 271112,
         province == "Canada",
         year==2019 & month==12,
         quantity>0) %>% 
  mutate(trading_partner = ifelse(quantity < 135000, "Other", trading_partner)) %>% 
  dplyr::group_by(trading_partner) %>% 
  dplyr::summarise(quantity = sum(quantity),
                   value = sum(value)) %>% 
  mutate(price = value / quantity) %>% 
  arrange(price) %>% 
  mutate(quantity = quantity/1000,
         right_dfm = cumsum(quantity),
         left_dfm = right_dfm - quantity,
         name1 = fct_reorder(as.factor(trading_partner), right_dfm))

max_x = round_any(max(propane.df$right_dfm), 100, f = ceiling)
max_y = round_any(max(propane.df$price), 1, f = ceiling)

ggplot(propane.df, aes(ymin = 0)) +
  geom_rect(aes(xmin = left_dfm, xmax = right_dfm, ymax = `price`, fill = name1)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, max_x)) +
  labs(y = "Import Price ($/Litre)",
       x = "Quantity (1000s of Litres)",
       title = "Propane Import Supply Curve",
       subtitle = "Dec 2019 \n Import Price vs. Quantity Imported",
       fill = "Import Origin: Country/State") +
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        plot.caption = element_text(hjust = 0, size = 6))  +
  scale_y_continuous(limits = c(0, max_y), labels = dollar) +
  scale_fill_brewer(palette = "Set3")

ggsave(file.path(output, str_c("Propane", ".jpeg", sep="")))

## Gasoline
base.df %>% 
  filter(hs_code == 271012,
         province == "Canada",
         year==2019 & month==12,
         quantity>0) %>% 
  mutate(trading_partner = ifelse(quantity < 220000, "Other", trading_partner)) %>% 
  dplyr::group_by(trading_partner) %>% 
  dplyr::summarise(quantity = sum(quantity),
                   value = sum(value)) %>% 
  mutate(price = value / (quantity)) %>% 
  arrange(price) %>% 
  mutate(quantity = quantity/1000,
         right_dfm = cumsum(quantity),
         left_dfm = right_dfm - quantity,
         name1 = fct_reorder(as.factor(trading_partner), right_dfm))

max_x = round_any(max(gas.df$right_dfm), 100, f = ceiling)
max_y = round(max(gas.df$price), 2) + 0.25

ggplot(gas.df, aes(ymin = 0)) +
  geom_rect(aes(xmin = left_dfm, xmax = right_dfm, ymax = `price`, fill = name1)) +
  scale_x_continuous(labels = scales::comma, limits = c(0, max_x)) +
  labs(y = "Import Price ($/Litre)",
       x = "Quantity (1000s of Litres)",
       title = "Gasoline (including Aviation) Import Supply Curve",
       subtitle = "Dec 2019 \n Import Price vs. Quantity Imported",
       fill = "Import Origin: Country/State") +
  theme(legend.text = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"),
        plot.margin=unit(c(0.5,0.5,2,0.5),"cm"),
        plot.caption = element_text(hjust = 0, size = 6))  +
  scale_y_continuous(limits = c(0, max_y), labels = dollar) +
  scale_fill_brewer(palette = "Set3")

ggsave(file.path(output, str_c("Gasoline", ".jpeg", sep="")))
