
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

oij_2015_2020 <- read_excel ("Data/oij_2015_2020.xlsx")
oij_2015_2020 <- clean_names (oij_2015_2020)
oij_2015_2020 <- oij_2015_2020 %>% mutate (ano = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%Y"))
oij_2015_2020 <- oij_2015_2020 %>% mutate (mes = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%m"))
oij_2015_2020 <- oij_2015_2020 %>% mutate (ano_mes = format(as.Date (oij_2015_2020$fecha, format = "%Y / %m / %d"), "%Y, %m"))

ids_2017 <- read_excel ("Data/ids_2017.xlsx")
ids_2017 <- clean_names (ids_2017)
ids_2017 <- na.omit (ids_2017)
ids_2017_mdo <- ids_2017 %>% filter (indice_de_desarrollo_social_distrital_2017_segun_division_territorial_administrativa %in% c(11501, 11502, 11503, 11504))


# Delitos según tiempo ----------------------------------------------------

## Crear los grupos según el mes de comisión de los delitos

oij_2015_2020_temp <- oij_2015_2020 %>% group_by (ano_mes) %>% filter (n()>10) %>%
  summarize (delitos = n()) %>% arrange (ano_mes)

## Crear un segundo df para encontrar el delito más recurrente cada mes

oij_2015_2020_temp_2 <- oij_2015_2020 %>% group_by (ano_mes, delito) %>% filter (n() >10) %>%
  summarize (delitos = n())

oij_2015_2020_temp_2 <- as.data.frame (oij_2015_2020_temp_2) %>% 
  group_by (ano_mes) %>%
  filter (delitos == max(delitos)) 
  
mayor_incidencia <- oij_2015_2020_temp_2$delito

## Generación del gráfico por delito priorizado

oij_2015_2020_temp %>% ggplot (aes(x = ano_mes, y = delitos, colour = mayor_incidencia)) +
  geom_point(size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020") +
  scale_colour_brewer("Delito de mayor incidencia", palette="Dark2") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 
 


# Crear un tercer data frame para segregar por género
                                                
oij_2015_2020_temp_3 <- oij_2015_2020 %>% group_by (ano_mes, genero) %>% filter (n() >10) %>%
  summarize (delitos = n())

oij_2015_2020_temp_3 <- as.data.frame (oij_2015_2020_temp_3) %>% 
  group_by (ano_mes) %>%
  filter (delitos == max(delitos)) 

mayor_incidencia_genero <- oij_2015_2020_temp_3$genero

# Generaciond el gráfico por genero

oij_2015_2020_temp %>% ggplot (aes(x = ano_mes, y = delitos, colour = mayor_incidencia_genero)) +
  geom_point(size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  scale_colour_manual("Principales victimas por genero", values=c("steelblue", "brown4")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 


# Incidencia por distrito vs Poblacion ------------------------------------


# Datos del INEC a 2016 Proyecciones nacionales 2011-2016 por distrito
## al 30 de junio de cada año

distritos <- data.frame (distrito = c("San Pedro", "Sabanilla", "Mercedes", "San Rafael"), población = c(29229, 13560, 5893, 12979))

oij_2015_2020_temp_4 <- oij_2015_2020 %>%
  group_by (ano_mes, distrito) %>%
  summarize (delitos = n())

oij_2015_2020_temp_4 <- as.data.frame (oij_2015_2020_temp_4) %>% 
  group_by (ano_mes) %>% filter (distrito != "DESCONOCIDO") 

oij_2015_2020_temp_4 %>% ggplot (aes(x = ano_mes, y = delitos, colour = distrito)) +
  geom_point (size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020 por distrito") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 

## Se repite el experimento sin San Pedro

oij_2015_2020_temp_5 <- oij_2015_2020_temp_4 %>% filter (distrito != "SAN PEDRO")
      
oij_2015_2020_temp_5 %>% ggplot (aes(x = ano_mes, y = delitos, colour = distrito)) +
  geom_point (size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020 por distrito") +
  xlab ("Mes") +
  ylab ("Cantidad de delitos") 

## Ahora se analizará en proporción a la población


oij_2015_2020_temp_6 <- oij_2015_2020_temp_4 %>%
  mutate (delito_1000_habit = (delitos * ifelse(distrito == "SAN PEDRO", 1/29229, 1) *
                                ifelse (distrito == "SABANILLA", 1/ 13560, 1) *
                                ifelse (distrito == "MERCEDES", 1/5893, 1) *
                                ifelse (distrito == "SAN RAFAEL", 1/12979, 1) * 1000))

oij_2015_2020_temp_6 %>% ggplot (aes(x = ano_mes, y = delito_1000_habit, colour = distrito)) +
  geom_point (size = 2) +
  scale_x_discrete () +
  theme (axis.text.x = element_text(size = "6", angle = 90, face = "bold")) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020 por distrito") +
  xlab ("Mes") +
  ylab ("Delitos por cada mil habitantes")

## Estadistica canton comparando por hora

oij_2015_2020_temp_7 <- oij_2015_2020 %>% filter (distrito != "DESCONOCIDO") %>% group_by (ano_mes, delito, distrito, hora) %>%
  summarize (delitos = n())

oij_2015_2020_temp_7 %>% 
  ggplot(aes (x = ano_mes, y = delitos, colour = distrito)) +
  geom_point () +
  facet_grid(. ~ hora) +
  theme(strip.text.x = element_text(size = 5), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle ("Incidencia delictiva en Montes de Oca de enero 2015 a abril 2020 por rango horario") +
  xlab ("Mes según cada rango horario") +
  ylab ("Cantidad de delitos por mes") 
  



