rm(list=ls())

#### OBS: ingen danske bogstaver i denne fil. Se: https://github.com/rstudio/shinydashboard/issues/190#issuecomment-334078366 ###


# Load packages -----------------------------------------------------------

# Shiny-pakker
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinyFeedback)
require(shinycssloaders)
require(shinyBS)
require(shinyhelper)
#require(shinyjs)

# Databearbejdning
require(tidyr)
require(dplyr)
require(glue)
require(readr)
require(purrr)

# Grafik og tabeller
require(ggplot2)
require(ggthemes)
require(gt)

# Eksport af grafik og tabeller
require(webshot)
require(openxlsx)

# Diverse
require(htmltools)


# Load data ---------------------------------------------------------------


data_ksb_spread <- read.csv("data_ksb_2013_2022Q4_spread.csv", encoding = "UTF-8", sep = ";")

data_udk_spread <- read.csv("data_udk_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_ask_spread <- read.csv("data_ask_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_tilsyn_spread <- read.csv("data_tilsyn_spread_2022Q4.csv", encoding = "UTF-8", sep = ";")

data_mellemkommunal_spread <- read.csv("data_mellemkommunal_spread_2022_Q4.csv", encoding = "UTF-8", sep = ";")

data_underretning_spread <- read.csv("data_underretning_spread_2022.csv", encoding =  "UTF-8", sep = ";")

data_tvangsadoption_spread <- read.csv("data_tvangsadoption_spread_2022Q4.csv", encoding =  "UTF-8", sep = ";")

data_anbringelse_spread <- read.csv("data_anbringelse_spread_2022.csv", encoding = "UTF-8", sep = ";")

kommuner <- read.csv("kommuner.csv", encoding = "UTF-8", sep = ";")

kommuner <- kommuner %>%
  arrange(Region, Kommune) %>%
  mutate(fuldt_navn = paste(Kommune, 'Kommune') )

Sagsemner <- read.csv("Sagsemner_uden_nummer_2022Q4.csv", encoding = "UTF-8", sep = ";")


Sagsemner_udk <- read.csv("Sagsemner_udk_opdatering_11.csv", encoding = "UTF-8", sep = ";", stringsAsFactors = FALSE)

Sagsemner_ask <- read.csv("Sagsemner_ask_3.csv", encoding="UTF-8", sep=";", na.strings="NULL", stringsAsFactors=FALSE)

Sagsemner_tilsyn <- unique(as.character(data_tilsyn_spread$Sagsemne))

Sagsemner_tilsyn_tb <- tibble(Sagsemner_tilsyn) %>%
  mutate(Lovgrundlag = "Kommunestyrelsesloven")
#Sagsemner_tilsyn_tb$Sagsemner_tilsyn <- as.character(Sagsemner_tilsyn_tb$Sagsemner_tilsyn)

Sagsemner_forside <- read.csv("Sagsemner_samlet_nye_omraeder_2.csv", encoding = "UTF-8", sep = ";", stringsAsFactors = FALSE)

Sagsemner <- Sagsemner %>%
  arrange(Lovgivning, Sorteringskode)

Sagsemner_forside <- Sagsemner_forside %>%
  arrange(Omraade, Lovgrundlag, Sagsemne)

datohierarki <- read.csv("datohierarki.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE)

instanser <- read.csv("instanser.csv", encoding = "UTF-8", sep=";", stringsAsFactors = FALSE)







