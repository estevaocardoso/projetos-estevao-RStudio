#Preliminaries
library(tidyverse)
library(dplyr)
rm(list=ls())
library("utils")
library("tidyverse")

setwd("-----------") #fill in with your working directory!

#Getting the data
gender_data <- as_tibble(read.csv("Gender_StatsData.csv"))

head(gender_data)

teenager_fr <- subset(gender_data, Indicator.Code == "SP.ADO.TFRT")
teenager_fr
rm(gender_data)

mean_value = mean(teenager_fr$X2000, na.rm = TRUE)
std_value = sd(teenager_fr$X2000, na.rm=TRUE)

byincomelevel <- filter(teenager_fr,Country.Code%in%c("LIC","MIC","HIC", "WLD"))

histdata_twoyears <- select(teenager_fr, Country.Name, Country.Code, Indicator.Name,         Indicator.Code, X1960, X2000)

histdata_twoyears <- gather(teenager_fr, Year, FertilityRate, X1960, X2000) %>%
  select(Year, Country.Name, Country.Code, FertilityRate)

histdata_twoyears <- filter(histdata_twoyears, !is.na(FertilityRate))
ggplot(histdata_twoyears, aes(x=FertilityRate)) +
  geom_histogram(data=subset(histdata_twoyears, Year=="X1960"),
                 color="darkred", fill="red", alpha=0.4) +
  geom_histogram(data=subset(histdata_twoyears, Year=="X2000"),
                 color="darkblue", fill="blue", alpha=0.4)
ggsave("hist.png")