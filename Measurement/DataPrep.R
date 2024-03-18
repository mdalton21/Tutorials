###############################################
# Data Preparation for SoDA 501 Tutorial
# Maya A. Dalton
# February 2024
###############################################
rm(list=ls())

# Load libraries
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(did)

# Set working directory
setwd("~/OneDrive - The Pennsylvania State University/MA Thesis/Datasets")

# Load datasets
ecav <- read_excel("ECAV datatset_Version 1.2.xls")
vdem <- read_csv("V-Dem-CY-Full+Others-v12.csv")
qog.ts <- read_csv("qog_std_ts_jan23.csv")

###############################################
# Cleaning and subsetting
###############################################
ecav.c <- ecav %>%
  mutate(year = format(as.Date(Date, format="%Y/%m/%d"),"%Y"), # Extract year from date variable
         year = as.numeric(year),
         election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
         election_year = as.numeric(election_year))  %>%
  dplyr::select(country, year, Actor1Type, EventViolence)

# Selecting necessary variables from vdem
vdem.c <- vdem %>%
  dplyr::select(year, country_name, v2lgqugen, v2x_polyarchy)

# Selecting necessary variables from qog: gdppc
qog.c2 <- qog.ts %>%
  dplyr::select(cname, year, wdi_gdpcapcur)

# Join datasets
df <- left_join(vdem.c, qog.c2, by=c('country_name'='cname', "year"))
df <- left_join(df, ecav.c, by=c('country_name'='country', "year"))

df.c <- df %>% 
  mutate(actor_ord = case_when(
    Actor1Type == 1 ~ "State",
    Actor1Type == 2 ~ "Citizens",
    Actor1Type == 3 ~ "Party",
    Actor1Type == 4 ~ "Armed Group",
    Actor1Type == 5 ~ "Other",
    Actor1Type == -99 ~ NA
  ),
  quota_ord = case_when(
    v2lgqugen == 0 ~ "No",
    v2lgqugen == 1 ~ "Yes, no sanct",
    v2lgqugen == 2 ~ "Yes, weak sanct",
    v2lgqugen == 3 ~ "Yes, strong sanct",
    v2lgqugen == 4 ~ "Yes, reserved"
  ))

df.c <- df.c %>% drop_na()

write.csv(df.c, "~/OneDrive - The Pennsylvania State University/SoDA 501/Tutorials/Dalton-Tutorial/measurement-df.csv", row.names=FALSE)

