library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate) 
library(data.table)
library(sp)
library(lattice)
library(maps)
library(rgdal)
library(tidyr)
library(tibble)
library(sf)
library(foreign)
library(cartography)
library(date)


#LIMPIEZA DE TABLA Y DATOS PARA GENERAR DATALIMPIA.RDA

dataset <- read.csv2("https://raw.githubusercontent.com/Juanmick/ejercicio/master/2019_Accidentalidad.csv")


colnames(dataset)[7] <- "TIPOACCIDENTE"
colnames(dataset)[8] <- "ESTADOMETEREOLOGICO"
colnames(dataset)[9] <- "TIPOVEHICULO"
colnames(dataset)[10] <- "TIPOPERSONA"
colnames(dataset)[11] <- "RANGOEDAD"



dataset$LESIVIDAD <- as.character(as.numeric(dataset$LESIVIDAD))
ac <- select(dataset, -NÚMERO,-Nº..EXPEDIENTE, -CALLE)


ac$DISTRITO <- as.factor(ac$DISTRITO)

ac$FECHA <- dmy(ac$FECHA)

ac$HORA <- as.character(ac$HORA)
ac$HORA <- strptime(ac$HORA, format="%R")

#Reclasificamos en menos niveles la gravedad según la estructura proporcionadad por el ayto
ac$LESIVIDAD[ac$LESIVIDAD == "1"] <- "LEVE"
ac$LESIVIDAD[ac$LESIVIDAD == "2"] <- "LEVE"
ac$LESIVIDAD[ac$LESIVIDAD == "3"] <- "GRAVE"
ac$LESIVIDAD[ac$LESIVIDAD == "4"] <- "FALLECIDO"
ac$LESIVIDAD[ac$LESIVIDAD == "5"] <- "LEVE"
ac$LESIVIDAD[ac$LESIVIDAD == "6"] <- "LEVE"
ac$LESIVIDAD[ac$LESIVIDAD == "7"] <- "LEVE"
ac$LESIVIDAD[ac$LESIVIDAD == "14"] <- "SIN ASISTENCIA"
ac$LESIVIDAD[ac$LESIVIDAD == "77"] <- "DESCONOCIDO"
ac$LESIVIDAD[is.na(ac$LESIVIDAD)] <- "SIN ASISTENCIA"
ac$LESIVIDAD <- as.factor(ac$LESIVIDAD)

ac$LESIVIDAD <- factor( ac$LESIVIDAD, levels = levels( ac$LESIVIDAD )[ c( 1,5,4,3,2 ) ] )

ac$RANGOEDAD <- as.character(ac$RANGOEDAD)
#Reclasificamos los Rangos para tener menos niveles
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 0 A 5 AÑOS'] <- 'DE 0 A 17 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 6 A 9 AÑOS'] <- 'DE 0 A 17 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 10 A 14 AÑOS'] <- 'DE 0 A 17 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 15 A 17 AÑOS'] <- 'DE 0 A 17 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 18 A 20 AÑOS'] <- 'DE 18 A 39 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 21 A 24 AÑOS'] <- 'DE 18 A 39 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 25 A 29 AÑOS'] <- 'DE 18 A 39 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 30 A 34 AÑOS'] <- 'DE 18 A 39 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 35 A 39 AÑOS'] <- 'DE 18 A 39 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 40 A 44 AÑOS'] <- 'DE 40 A 64 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 45 A 49 AÑOS'] <- 'DE 40 A 64 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 50 A 54 AÑOS'] <- 'DE 40 A 64 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 55 A 59 AÑOS'] <- 'DE 40 A 64 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 60 A 64 AÑOS'] <- 'DE 40 A 64 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 65 A 69 AÑOS'] <- 'MAYOR DE 65 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'DE 70 A 74 AÑOS'] <- 'MAYOR DE 65 AÑOS'
ac$RANGOEDAD[ac$RANGOEDAD == 'MAYOR DE 74 AÑOS'] <- 'MAYOR DE 65 AÑOS'
ac$RANGOEDAD <- as.factor(ac$RANGOEDAD)

#REORDENAMOS LOS RANGOS DE EDAD
ac$RANGOEDAD <- factor( ac$RANGOEDAD, levels = levels( ac$RANGOEDAD )[ c( 4,1,2,3,5 ) ] )


#RECLASIFICAMOS Y ORDENAMOS ALGUNOS VALORES DEL TIEMPO
ac$ESTADOMETEREOLOGICO <- as.character(ac$ESTADOMETEREOLOGICO)
ac$ESTADOMETEREOLOGICO[ac$ESTADOMETEREOLOGICO == ""] <- 'Se desconoce'
ac$ESTADOMETEREOLOGICO[is.na(ac$ESTADOMETEREOLOGICO)] <- "Se desconoce"
ac$ESTADOMETEREOLOGICO <- as.factor(ac$ESTADOMETEREOLOGICO)
ac$ESTADOMETEREOLOGICO <- factor( ac$ESTADOMETEREOLOGICO, levels = levels( ac$ESTADOMETEREOLOGICO )[ c( 7,1,6,3,4,2,5 ) ] )

#RECLASIFICAMOS LOS TIPOS DE PERSONA
ac$TIPOPERSONA <- as.character(ac$TIPOPERSONA)
ac$TIPOPERSONA[ac$TIPOPERSONA == ""] <- 'Se desconoce'
ac$TIPOPERSONA[is.na(ac$TIPOPERSONA)] <- "Se desconoce"
ac$TIPOPERSONA <- as.factor(ac$TIPOPERSONA)
ac$TIPOPERSONA <- factor( ac$TIPOPERSONA, levels = levels( ac$TIPOPERSONA )[ c( 4,1,2,3 ) ] )

#RECLASIFICAMOS LOS TIPOS DE ACCIDENTE
ac$TIPOACCIDENTE <- as.character(ac$TIPOACCIDENTE)
ac$TIPOACCIDENTE[ac$TIPOACCIDENTE == ""] <- 'Se desconoce'
ac$TIPOACCIDENTE[is.na(ac$TIPOACCIDENTE)] <- "Se desconoce"
ac$TIPOACCIDENTE <- as.factor(ac$TIPOACCIDENTE)

#RECLASIFICAMOS LOS TIPOS DE VEHICULO
ac$TIPOVEHICULO <- as.character(ac$TIPOVEHICULO)
ac$TIPOVEHICULO[ac$TIPOVEHICULO == ""] <- 'Sin especificar'
ac$TIPOVEHICULO[is.na(ac$TIPOVEHICULO)] <- "Sin especificar"
ac$TIPOVEHICULO <- as.factor(ac$TIPOVEHICULO)

#Reclasificamos y creamos columna GEOCODIGO
ac$GEOCODIGO[ac$DISTRITO == "ARGANZUELA"] <- "07902"
ac$GEOCODIGO[ac$DISTRITO == "BARAJAS"] <- "07921"
ac$GEOCODIGO[ac$DISTRITO == "CARABANCHEL"] <- "07911"
ac$GEOCODIGO[ac$DISTRITO == "CENTRO"] <- "07901"
ac$GEOCODIGO[ac$DISTRITO == "CHAMARTÍN"] <- "07905"
ac$GEOCODIGO[ac$DISTRITO == "CHAMBERÍ"] <- "07907"
ac$GEOCODIGO[ac$DISTRITO == "CIUDAD LINEAL"] <- "07915"
ac$GEOCODIGO[ac$DISTRITO == "FUENCARRAL-EL PARDO"] <- "07908"
ac$GEOCODIGO[ac$DISTRITO == "HORTALEZA"] <- "07916"
ac$GEOCODIGO[ac$DISTRITO == "LATINA"] <- "07910"
ac$GEOCODIGO[ac$DISTRITO == "MONCLOA-ARAVACA"] <- "07909"
ac$GEOCODIGO[ac$DISTRITO == "MORATALAZ"] <- "07914"
ac$GEOCODIGO[ac$DISTRITO == "PUENTE DE VALLECAS"] <- "07913"
ac$GEOCODIGO[ac$DISTRITO == "RETIRO"] <- "07903"
ac$GEOCODIGO[ac$DISTRITO == "SALAMANCA"] <- "07904"
ac$GEOCODIGO[ac$DISTRITO == "SAN BLAS-CANILLEJAS"] <- "07920"
ac$GEOCODIGO[ac$DISTRITO == "TETUÁN"] <- "07906"
ac$GEOCODIGO[ac$DISTRITO == "USERA"] <- "07912"
ac$GEOCODIGO[ac$DISTRITO == "VICÁLVARO"] <- "07919"
ac$GEOCODIGO[ac$DISTRITO == "VILLA DE VALLECAS"] <- "07918"
ac$GEOCODIGO[ac$DISTRITO == "VILLAVERDE"] <- "07917"

#eliminamos valores NA
ac <- ac %>% drop_na()

save(ac, file="datalimpia.rda")