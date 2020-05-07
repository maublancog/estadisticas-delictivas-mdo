
# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)
library(readxl)


# Importación y tratamiento de data ----------------------------------------

oij_2015_2020 <- read_excel("oij_2015_2020.xlsx")
oij_2015_2020 <- clean_names(oij_2015_2020)
skim(oij_2015_2020)

view(oij_2015_2020)

