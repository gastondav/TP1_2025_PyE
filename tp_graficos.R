#Instalamos paquetes
install.packages("googledrive")
install.packages("readxl")
install.packages("tidyverse")
install.packages("vctrs")

#Instalamos librerias
library(googledrive)
library(readxl)
library(tidyverse)


#https://docs.google.com/spreadsheets/d/1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy
googledrive::drive_download(as_id("1IRhvzOQkvuspQF3TAsBCI-68i8ya0_hy"), overwrite = T)

#cargo los datos
datos <- readxl::read_excel("Datos_LP.xlsx", col_names = FALSE, skip = 3)

# Veo la estructura del dataset
str(datos)

attach(datos)

#seleccionamos unas columnas
datos_chico1 <- datos %>% select(2,3,6,13,21,24,29,43,44,45,46,47,48,50,54,57,58)

#le ponemos nombres a las columnas
colnames(datos_chico1) <- c("Provincia","Barrio","Integrantes en vivienda"
                            ,"Ambientes que se usan como dormitorios"
                            ,"Costo del alquiler"
                            ,"Tipo de acceso al agua"
                            ,"Existencia de bano"
                            ,"Tipo calefaccion 1"
                            ,"Tipo calefaccion 2"
                            ,"Tipo calefaccion 3"
                            ,"Tipo calefaccion 4"
                            ,"Tipo calefaccion 5"
                            ,"Tipo calefaccion 6"
                            ,"Tipo conexio electrica"
                            ,"Frec de cortes electricos"
                            ,"Celular con acceso a internet"
                            ,"Cant abono datos moviles")

#filtramos por CABA
datos_reducido1 <-datos_chico1 %>%
  filter(Provincia == "CABA")