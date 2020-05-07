
# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(ggthemes)

# Importación y tratamiento de data ----------------------------------------

oij_2015_2020 <- read_excel ("oij_2015_2020.xlsx")
oij_2015_2020 <- clean_names (oij_2015_2020)
oij_2015_2020 <- oij_2015_2020 %>% mutate (año = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%Y"))
oij_2015_2020 <- oij_2015_2020 %>% mutate (mes = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%m"))
oij_2015_2020 <- oij_2015_2020 %>% mutate (año_mes = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%Y, %m"))

ids_2017 <- read_excel ("ids_2017.xlsx")
ids_2017 <- clean_names (ids_2017)
ids_2017 <- na.omit (ids_2017)
ids_2017_mdo <- ids_2017 %>% filter (indice_de_desarrollo_social_distrital_2017_segun_division_territorial_administrativa %in% c(11501, 11502, 11503, 11504))


# Delitos según tiempo ----------------------------------------------------

## Crear los grupos según el mes de comisión de los delitos

oij_2015_2020_temp <- oij_2015_2020 %>% group_by (año_mes) %>% filter (n()>10) %>%
  summarize (delitos = n()) %>% arrange (año_mes)

## Crear un segundo df para encontrar el delito más recurrente cada mes

oij_2015_2020_temp_2 <- oij_2015_2020 %>% group_by (año_mes, delito) %>% filter (n() >10) %>%
  summarize (delitos = n())

oij_2015_2020_temp_2 <- as.data.frame (oij_2015_2020_temp_2) %>% 
  group_by (año_mes) %>%
  filter (delitos == max(delitos)) 
  
mayor_incidencia <- oij_2015_2020_temp_2$delito

## Generación del gráfico por delito priorizado

oij_2015_2020_temp %>% ggplot (aes(x = año_mes, y = delitos, colour = mayor_incidencia)) +
  geom_point(size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020") +
  scale_colour_brewer("Delito de mayor incidencia", palette="Dark2") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 
 


# Crear un tercer data frame para segregar por género
                                                
oij_2015_2020_temp_3 <- oij_2015_2020 %>% group_by (año_mes, genero) %>% filter (n() >10) %>%
  summarize (delitos = n())

oij_2015_2020_temp_3 <- as.data.frame (oij_2015_2020_temp_3) %>% 
  group_by (año_mes) %>%
  filter (delitos == max(delitos)) 

mayor_incidencia_genero <- oij_2015_2020_temp_3$genero

# Generaciond el gráfico por género

oij_2015_2020_temp %>% ggplot (aes(x = año_mes, y = delitos, colour = mayor_incidencia_genero)) +
  geom_point(size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  scale_colour_manual("Principales víctimas por género", values=c("steelblue", "brown4")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 
      