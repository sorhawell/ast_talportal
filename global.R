rm(list=ls())

#### OBS: ingen danske bogstaver i denne fil. Se: https://github.com/rstudio/shinydashboard/issues/190#issuecomment-334078366 ###


# Load packages -----------------------------------------------------------

# Shiny-pakker
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyFeedback)
library(shinycssloaders)
library(shinyBS)
library(shinyhelper)
#library(shinyjs)

# Databearbejdning
library(tidyr)
library(dplyr)
library(glue)
library(readr)
library(purrr)

# Grafik og tabeller
library(ggplot2)
library(ggthemes)
library(gt)

# Eksport af grafik og tabeller
library(webshot)
library(openxlsx)

# Diverse
library(htmltools)


# Soruce R functions ----------
source("./R/server_functions.R")
source("./R/ui_functions.R")


# Load data ---------------------------------------------------------------
data_ksb_spread <- read.csv("app_data/data_ksb_2013_2022Q4_spread.csv", encoding = "UTF-8", sep = ";")

data_udk_spread <- read.csv("app_data/data_udk_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_ask_spread <- read.csv("app_data/data_ask_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_tilsyn_spread <- read.csv("app_data/data_tilsyn_spread_2022Q4.csv", encoding = "UTF-8", sep = ";")

data_mellemkommunal_spread <- read.csv("app_data/data_mellemkommunal_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_underretning_spread <- read.csv("app_data/data_underretning_spread_2022.csv", encoding =  "UTF-8", sep = ";")

data_tvangsadoption_spread <- read.csv("app_data/data_tvangsadoption_spread_2022Q4.csv", encoding =  "UTF-8", sep = ";")

data_anbringelse_spread <- read.csv("app_data/data_anbringelse_spread_2022.csv", encoding = "UTF-8", sep = ";")

kommuner <- read.csv("app_data/kommuner.csv", encoding = "UTF-8", sep = ";")

kommuner <- kommuner %>%
  arrange(Region, Kommune) %>%
  mutate(fuldt_navn = paste(Kommune, 'Kommune') )

Sagsemner <- read.csv("app_data/Sagsemner_uden_nummer_2022Q4.csv", encoding = "UTF-8", sep = ";")


Sagsemner_udk <- read.csv("app_data/Sagsemner_udk_opdatering_10.csv", encoding = "UTF-8", sep = ";", stringsAsFactors = FALSE)

Sagsemner_ask <- read.csv("app_data/Sagsemner_ask_3.csv", encoding="UTF-8", sep=";", na.strings="NULL", stringsAsFactors=FALSE)

Sagsemner_tilsyn <- unique(as.character(data_tilsyn_spread$Sagsemne))

Sagsemner_tilsyn_tb <- tibble(Sagsemner_tilsyn) %>%
  mutate(Lovgrundlag = "Kommunestyrelsesloven")
#Sagsemner_tilsyn_tb$Sagsemner_tilsyn <- as.character(Sagsemner_tilsyn_tb$Sagsemner_tilsyn)

Sagsemner_forside <- read.csv("app_data/Sagsemner_samlet_nye_omraeder_2.csv", encoding = "UTF-8", sep = ";", stringsAsFactors = FALSE)

Sagsemner <- Sagsemner %>%
  arrange(Lovgivning, Sorteringskode)

Sagsemner_forside <- Sagsemner_forside %>%
  arrange(Omraade, Lovgrundlag, Sagsemne)

datohierarki <- read.csv("app_data/datohierarki.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE)

instanser <- read.csv("app_data/instanser.csv", encoding = "UTF-8", sep=";", stringsAsFactors = FALSE)







