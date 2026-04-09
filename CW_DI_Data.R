library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)

CWDI_General <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "General Data") |> 
  dplyr::select(!c("Bag ID", "Time of Capture", "Date of Release", "Time of Release"))
CEWL <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "CEWL data")
Osmo <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "Osm data")
#combine
CWDI <- CWDI_General |> 
  dplyr::full_join(CEWL, by = "Lizard ID") |> 
  dplyr::full_join(Osmo, by = c("Lizard ID", "Location"))
#filter
CWDI <- CWDI |> 
  mutate(across(c(CEWL1, CEWL2),
                ~ ifelse(`Lizard ID` == "DI-04", NA, .)),
         across(CEWL4,
                ~ ifelse(`Lizard ID` == "CW-09", NA_real_, .)),
         across(`Replicate 1`,
                ~ ifelse(`Lizard ID` == "DI-13", NA_real_, .)),
         across(`Replicate 2`,
                ~ ifelse(`Lizard ID` %in% c("CW-04","CW-16"), NA_real_, .)),
         across(`Replicate 3`,
                ~ ifelse(`Lizard ID` %in% c("CW-06","CW-13","CW-14","CW-CP-03"), NA_real_, .))) |> 
#remove second osmos
  filter(!(`Lizard ID` %in% c("CW-13-02", "CW-14-02", "CW-15-02", "CW-16-02")))

CWDI <- CWDI |> 
  mutate(CEWL = rowMeans(across(CEWL1:CEWL5), na.rm = TRUE)) |> 
  mutate(Osmo = rowMeans(across(`Replicate 1`:`Replicate 4`), na.rm = TRUE))

ggplot(filter(CWDI, !`Lizard ID` %in% c("DI-09", "CW-01")), #CWDI, 
       aes(x = Species, y = CEWL)) + geom_boxplot()
  
ggplot(CWDI, aes(x = Species, y = Osmo)) + geom_boxplot()



