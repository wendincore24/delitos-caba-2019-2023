getwd()
setwd("C:/Users/54356/Documents/Facultad/Ciencia de Datos/TP_FINAL/1_RAW/NUEVO")
##Instalar los paquetes necesarios
#install.packages("tidyverse")
#install.packages("janitor")
# Cargar librerías necesarias
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)  # Para manejo de fechas
library(scales)     # Para formateo de valores
library(sf)
library(janitor)    #Para clean names
library(readxl)
###-----Crear las carpetas necesarias-----###
dir.create("input", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)

#Rutas de los archivos
instub <- 'raw'
outstub<-'input'
##Cargar los datos


Delitos2019 <- read.csv(file.path(instub,"delitos_2019.csv"))
Delitos2020 <- read.csv(file.path(instub,"delitos_2020.csv"))
Delitos2021 <- read.csv(file.path(instub,"delitos_2021.csv"))
Delitos2022 <- read.csv(file.path(instub,"delitos_2022.csv"))
#Delitos2023 <- read.csv(file.path(instub,"delitos_2023.csv"))
Delitos2023 <- read_excel(file.path(instub, "delitos_2023.xlsx"))
##Ver los datos en tabla para verificar que han sidos cargados
View(Delitos2019)
View(Delitos2020)
View(Delitos2021)
View(Delitos2022)
View(Delitos2023)

##Convertir a tibble para una mejor visualizacion pero tambien puedo usar el original data frame
delitos_tbl19<-as_tibble(Delitos2019)
delitos_tbl20<-as_tibble(Delitos2020)
delitos_tbl21<-as_tibble(Delitos2021)
delitos_tbl22<-as_tibble(Delitos2022)
delitos_tbl23<-as_tibble(Delitos2023)

##---Examinar las primeras filas del conjunto de datos----###
print("Primeras filas del conjunto de datos: ")
head(delitos_tbl19)
print("Primeras filas del conjunto de datos: ")
head(delitos_tbl20)
print("Primeras filas del conjunto de datos: ")
head(delitos_tbl21)
print("Primeras filas del conjunto de datos: ")
head(delitos_tbl22)
print("Primeras filas del conjunto de datos: ")
head(delitos_tbl23)
##Documentacion sobre las variables
cat("
Variables:
-id-mapa:Identificador unico
-anio:Año de ocurrencia del hecho (todos los registros son del 2016)
-mes: Mes de ocurrencia del hecho
-dia:Día de la semana en que ocurrió el hecho (de “lunes” a “domingo”)
-fecha:Fecha de ocurrencia del hecho denunciado.
-franja: Hora donde ocurrió los hechos
-tipo: Tipo general del hecho denunciado (por ejemplo, “Robo”, “Hurto”, “Amenazas”)
-subtipo:Descripción más específica del delito (por ejemplo, “Robo con armas”, “Lesiones”)
-uso_arma:Indica si en el delito se usó algún tipo de arma (valores posibles: “Sí”, “No”)
-uso_moto:Indica si en el hecho delictivo se utilizó una motocicleta (“Sí”, “No”)
-barrio:Barrio de la ciudad donde ocurrió el delito
-comuna:Número de comuna
-barrio:Barrio de la ciudad donde ocurrió el delito
-lat:Latitud geografica donde ocurrio el evento
-long:Longitud geogr
    ")
##----Limpieza de nombre de columnas de las bases de datos con clean name ---##
Delitos2019 <- Delitos2019 %>% ## Convierte los nombres de columnas a un formato más limpio y programático
  clean_names()
Delitos2020 <- Delitos2020 %>%   ##Muy útil para evitar errores con nombres largos o con espacios.
  clean_names()
Delitos2021 <- Delitos2021 %>%
  clean_names()
Delitos2022 <- Delitos2022 %>%
  clean_names()
Delitos2023 <- Delitos2023 %>%
  clean_names()

##---Creo variables útiles y corregir valores inconsistentes--##
##Año 2019
delitos_tbl19 <- delitos_tbl19 %>%
  mutate(
    fecha = as.Date(fecha), ##Convertir la variable fecha a tipo Date para analisis por tiempo
    mes = str_to_title(mes),##Util para visualizaciones limpias
    dia_semana = factor(dia, ###Sirve para los gráficos
                        levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"),
                        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    #franja = as.numeric(franja),###Convertir a numeric si quiero hacer análisis por hora
    uso_arma = factor(uso_arma, levels = c("NO", "SI")),
    uso_moto = factor(uso_moto, levels = c("NO", "SI")),
    tipo = as.factor(tipo), ##Convertir a factor , es útil para gráficos con ggplot
    subtipo = as.factor(subtipo), ##Convertir a factor
    barrio = str_to_title(barrio), ##Str_to_title es util para visualizaciones limpias
    
  )
##Año 2020
delitos_tbl20 <- delitos_tbl20 %>%
  mutate(
    fecha = as.Date(fecha),
    mes = str_to_title(mes),##Util para visualizaciones limpias
    dia_semana = factor(dia, 
                        levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"),
                        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    #franja_horaria = as.numeric(franja),###convertir a numeric si quiero hacer análisis por hora
    uso_arma = factor(uso_arma, levels = c("NO", "SI")),
    uso_moto = factor(uso_moto, levels = c("NO", "SI")),
    tipo = as.factor(tipo), ##Convertir a factor , util para gráficos con ggplot
    subtipo = as.factor(subtipo), ##Convertir a factor
    barrio = str_to_title(barrio)
  )
##Año 2021
delitos_tbl21 <- delitos_tbl21 %>%
  mutate(
    fecha = as.Date(fecha),
    mes = str_to_title(mes),
    dia_semana = factor(dia, 
                        levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"),
                        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    #franja_horaria = as.numeric(franja),
    uso_arma = factor(uso_arma, levels = c("NO", "SI")),
    uso_moto = factor(uso_moto, levels = c("NO", "SI")),
    tipo = as.factor(tipo),
    subtipo = as.factor(subtipo),
    barrio = str_to_title(barrio)
    
  )

##Año 2022
delitos_tbl22 <- delitos_tbl22 %>%
  mutate(
    fecha = as.Date(fecha),
    mes = str_to_title(mes),
    dia_semana = factor(dia, 
                        levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"),
                        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    #franja_horaria = as.numeric(franja),
    uso_arma = factor(uso_arma, levels = c("NO", "SI")),
    uso_moto = factor(uso_moto, levels = c("NO", "SI")),
    tipo = as.factor(tipo),
    subtipo = as.factor(subtipo),
    barrio = str_to_title(barrio)
  )

##Año 2023
delitos_tbl23 <- delitos_tbl23 %>%
  mutate(
    fecha = as.Date(fecha),
    mes = str_to_title(mes),
    dia_semana = factor(dia, 
                        levels = c("LUN", "MAR", "MIE", "JUE", "VIE", "SAB", "DOM"),
                        labels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")),
    #franja_horaria = as.numeric(franja),
    uso_arma = factor(uso_arma, levels = c("NO", "SI")),
    uso_moto = factor(uso_moto, levels = c("NO", "SI")),
    tipo = as.factor(tipo),
    subtipo = as.factor(subtipo),
    barrio = str_to_title(barrio)
  )

# Asegura que las columnas 'longitud' y 'latitud' sean numéricas.
# Esto es crucial para el procesamiento posterior.

##Año 2019
delitos_tbl19$longitud <- as.numeric(as.character(delitos_tbl19$longitud))
delitos_tbl19$latitud <- as.numeric(as.character(delitos_tbl19$latitud))
##Año 2020
delitos_tbl20$longitud <- as.numeric(as.character(delitos_tbl20$longitud))
delitos_tbl20$latitud <- as.numeric(as.character(delitos_tbl20$latitud))
##Año 2021
delitos_tbl21$longitud <- as.numeric(as.character(delitos_tbl21$longitud))
delitos_tbl21$latitud <- as.numeric(as.character(delitos_tbl21$latitud))
##Año 2022
delitos_tbl22$longitud <- as.numeric(as.character(delitos_tbl22$longitud))
delitos_tbl22$latitud <- as.numeric(as.character(delitos_tbl22$latitud))
##Año 2023
delitos_tbl23$longitud <- as.numeric(as.character(delitos_tbl23$longitud))
delitos_tbl23$latitud <- as.numeric(as.character(delitos_tbl23$latitud))

##------Verificar valores faltantes de las columnas de la base de datos----####


missingvals<-colSums(is.na(delitos_tbl19))
print("Valores faltantes por columna:")
print(missingvals[missingvals > 0])
missingvals<-colSums(is.na(delitos_tbl20))
print("Valores faltantes por columna:")
print(missingvals[missingvals > 0])
missingvals<-colSums(is.na(delitos_tbl21))
print("Valores faltantes por columna:")
print(missingvals[missingvals > 0])
missingvals<-colSums(is.na(delitos_tbl22))
print("Valores faltantes por columna:")
print(missingvals[missingvals > 0])
missingvals<-colSums(is.na(delitos_tbl23))
print("Valores faltantes por columna:")  ####Hay valores faltantes por columna en las bases de datos
print(missingvals[missingvals > 0])


####----Evaluacion de la calidad de los datos----#####
##----Verificar valores duplicados de cada base de datos-----####

duplicados<-sum(duplicated(delitos_tbl19))
print(paste("Numero de filas duplicadas:",duplicados))
duplicados<-sum(duplicated(delitos_tbl20))
print(paste("Numero de filas duplicadas:",duplicados))
duplicados<-sum(duplicated(delitos_tbl21))
print(paste("Numero de filas duplicadas:",duplicados))
duplicados<-sum(duplicated(delitos_tbl22))
print(paste("Numero de filas duplicadas:",duplicados)) ##No hay filas duplicadas en ningun caso
duplicados<-sum(duplicated(delitos_tbl23))
print(paste("Numero de filas duplicadas:",duplicados))



##----Eliminar NA de columnas especificas--##
##Año 2019
delitos_tbl19 <- delitos_tbl19 %>% drop_na(longitud)##Existe NA en las variables longitud ,latitud y franja horaria 
delitos_tbl19 <- delitos_tbl19 %>% drop_na(latitud)
delitos_tbl19 <- delitos_tbl19 %>% drop_na(franja)
##Año 2020
delitos_tbl20 <- delitos_tbl20 %>% drop_na(longitud)
delitos_tbl20<- delitos_tbl20 %>% drop_na(latitud)
delitos_tbl20<- delitos_tbl20 %>% drop_na(franja)
##Año 2021
delitos_tbl21 <- delitos_tbl21 %>% drop_na(longitud)
delitos_tbl21 <- delitos_tbl21 %>% drop_na(latitud)
delitos_tbl21<- delitos_tbl21 %>% drop_na(franja)
##Año 2022
delitos_tbl22 <- delitos_tbl22 %>% drop_na(longitud)
delitos_tbl22 <- delitos_tbl22 %>% drop_na(latitud)
delitos_tbl22<- delitos_tbl22 %>% drop_na(franja)
##Año 2023
delitos_tbl23 <- delitos_tbl23 %>% drop_na(longitud)
delitos_tbl23 <- delitos_tbl23 %>% drop_na(latitud)
delitos_tbl23<- delitos_tbl23 %>% drop_na(franja)

##Estadisticas resumidas basicas

summary(delitos_tbl19)
summary(delitos_tbl20)
summary(delitos_tbl21)
summary(delitos_tbl22)
summary(delitos_tbl23)


##Guardo la version limpia en input
write.csv(delitos_tbl19,file.path(outstub,"limpieza_de_datos2019.csv"),row.names=FALSE)
write.csv(delitos_tbl20,file.path(outstub,"limpieza_de_datos2020.csv"),row.names=FALSE)
write.csv(delitos_tbl21,file.path(outstub,"limpieza_de_datos2021.csv"),row.names=FALSE)
write.csv(delitos_tbl22,file.path(outstub,"limpieza_de_datos2022.csv"),row.names=FALSE)
write.csv(delitos_tbl23,file.path(outstub,"limpieza_de_datos2023.csv"),row.names=FALSE)
