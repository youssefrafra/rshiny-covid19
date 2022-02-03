library(tidyverse)
library(plotly)
library(lubridate)
library(flexdashboard)
library(shinydashboard)
library(ggplot2)
library(shiny)
library(leaflet)
library(ggmap)
library(plyr)
library(leaflet)
library(tidygeocoder)
library(viridis)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgdal)

world_spdf <- readOGR( "DATA/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp")
world <- ne_countries(scale = "medium")
WHO_COVID <- read_csv("DATA/WHO-COVID.csv")
vaccination_data <- read_csv("DATA/vaccination-data.csv")
country <- read_delim("DATA/country.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)
data_with_coords <- merge(x = WHO_COVID, y = country, by = "Name", all.y  = TRUE)
vaccines_with_coords <- merge(x = vaccination_data, y = country, by = "Name", all.y  = TRUE)

#Ebola
ebola <- read_csv("DATA/ebola.csv")
ebola_deaths <- ebola %>% filter( Indicator =="Cumulative number of confirmed Ebola deaths", Date == max(Date)) %>%
  group_by(Indicator) %>%
  summarize(somme = sum(value))
ebola_cases <- ebola %>% filter(Indicator == "Cumulative number of confirmed Ebola cases" , Date == max(Date)) %>%
  group_by(Indicator) %>%
  summarize(somme = sum(value))

#Sars 
sars <- read_csv("C:/Users/rafra/OneDrive/Bureau/ProjetRshiny/rshiny-covid19/DATA/sars.csv")
sars_cases <- sars %>% filter(Date == max(Date)) %>% summarise(somme= sum(`Cumulative number of case(s)`))
sars_deaths <- sars %>% filter(Date == max(Date)) %>% summarise(somme= sum(`Number of deaths`))