# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(janitor)

# Data Import and Cleaning
import1<-read_sav(file="../data/GSS2016.sav") %>%
  drop_na(MOSTHRS)%>%
  rename(work_hours=MOSTHRS) %>% #renaming it for my convenience and to minimize confusion
  select(-HRS1,
         -HRS2,
         -where(~mean(is.na(.))>0.75))
  

# Visualization
import1%>%
  ggplot(aes(x=work_hours))+
  geom_histogram()

