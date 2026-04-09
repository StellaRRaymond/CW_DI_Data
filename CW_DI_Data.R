library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(lmerTest)
library(emmeans)

CWDI_General <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "General Data") |> 
  dplyr::select(!c("Bag ID", "Time of Capture", "Date of Release", "Time of Release"))
CEWL <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "CEWL data")
Osmo <- read_xlsx("../CWDI Spreadsheet.xlsx", sheet = "Osm data")
#combine + tidy
CWDI <- CWDI_General |> 
  dplyr::full_join(CEWL, by = "Lizard ID") |> 
  dplyr::full_join(Osmo, by = c("Lizard ID", "Location")) |> 
  dplyr::rename(SVL = `SVL (mm)`,
                Mass = `Mass (g)`,
                Lizard_ID = `Lizard ID`,
                Delay_Days = `Delay Days`,
                Gravidity = `Gravidity Status`) |>
  mutate(Amb_VPD = (Amb_RH/100)*(0.611*exp((17.502*`Amb_Temp`)/(`Amb_Temp` + 240.97))))

#filter
CWDI <- CWDI |> 
  mutate(across(c(CEWL1, CEWL2),
                ~ ifelse(Lizard_ID == "DI-04", NA, .)),
         across(CEWL4,
                ~ ifelse(Lizard_ID == "CW-09", NA_real_, .)),
         across(`Replicate 1`,
                ~ ifelse(Lizard_ID == "DI-13", NA_real_, .)),
         across(`Replicate 2`,
                ~ ifelse(Lizard_ID %in% c("CW-04","CW-16"), NA_real_, .)),
         across(`Replicate 3`,
                ~ ifelse(Lizard_ID %in% c("CW-06","CW-13","CW-14","CW-CP-03"), NA_real_, .))) |> 
#remove second osmos
  filter(!(Lizard_ID %in% c("CW-13-02", "CW-14-02", "CW-15-02", "CW-16-02")))

CWDI <- CWDI |> 
  mutate(CEWL = rowMeans(across(CEWL1:CEWL5), na.rm = TRUE)) |> 
  mutate(pOsm = rowMeans(across(`Replicate 1`:`Replicate 4`), na.rm = TRUE))


CEWL_model <- lm(CEWL ~ Species + Sex + SVL + Mass + Amb_VPD, data = CWDI)
anova(CEWL_model)

pOsm_model <- lm(pOsm ~ Species + SVL + Mass + Sex + Delay_Days + (Gravidity %in% Sex), data = CWDI)
anova(pOsm_model)


ggplot(filter(CWDI, !Lizard_ID %in% c("DI-09", "CW-01")), #CWDI, 
       aes(x = Species, y = CEWL)) + geom_boxplot()

ggplot(CWDI, aes(x = Species, y = Osmo)) + geom_boxplot()

ggplot()

