#Instalamos paquetes
install.packages("googledrive")
install.packages("readxl")
install.packages("tidyverse")
install.packages("vctrs")
install.packages("ggplot2")

#Instalamos librerias
library(googledrive)
library(readxl)
library(tidyverse)
library(ggplot2)

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
                            ,"Existencia de baño"
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


#----------------------TIPO DE ACCESO AL AGUA TABLA------------------------------------------------

datos_reducido1 %>% group_by(`Tipo de acceso al agua`) %>%filter(`Tipo de acceso al agua` != "No sabe") %>% 
  mutate(`Tipo de acceso al agua`=recode(`Tipo de acceso al agua`,
                                         "A través de una conexión con medidor a la red pública" = "red con medidor",
                                         "A través de un camión cisterna" = "camión cisterna",
                                         "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”"
                                         = "informalmente",
                                         "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda" 
                                         = "No posee"
                                         ))%>%
  ggplot() + 
  
  #aes(x = `Tipo de acceso al agua`) + # Frecuencias absolutas
  #aes(x = reorder(`Tipo de acceso al agua`, `Tipo de acceso al agua`, function(x) -length(x))) + # Ordenar según frecuencia
  #aes(x = `Tipo de acceso al agua`, y = after_stat(count) / sum(after_stat(count))) + # Porcentajes
  aes(x = reorder(`Tipo de acceso al agua`, `Tipo de acceso al agua`, function(x) -length(x)), 
  y = after_stat(count) / sum(after_stat(count))) +  # Porcentajes ordenados según frecuencia
  scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#CDAA7D',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Frecuencia", x = "Tipo de acceso al agua") + # Nombres de ejes
  
  ggtitle(" Tipo de acceso al agua de Barrios populares de CABA, año 2022") +
  
  theme_classic()# Temas preconfigurados de R https://r-charts.com/ggplot2/themes/

#-----------------------------------------------------------------------------------------------

#----------------------CANT DE ABONOS MOVILES------------------------------------------------

# Para ver cual es maximo valor de esa categoria
max(datos_reducido1$`Cant abono datos moviles`, na.rm = TRUE)

datos_reducido1 %>% group_by(`Cant abono datos moviles`) %>% 
  ggplot() + 
  
  aes(x = `Cant abono datos moviles`) + # Frecuencias absolutas
  #aes(x = reorder(`Cant abono datos moviles`, `Cant abono datos moviles`, function(x) -length(x))) + # Ordenar según frecuencia
  #aes(x = tiempo, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  #scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#CDAA7D',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Frecuencia", x = "Cantidad de Abonos Moviles") + # Nombres de ejes
  
  ggtitle(" Cantidad de abonos moviles en Barrios populares de CABA, año 2022") +
  
  theme_classic()
 
#La mediana 
median(datos_reducido1$`Cant abono datos moviles`, na.rm = TRUE)

#-----------------------------------------------------------------------------------------------
#----------------------acceso a baño o letrina------------------------------------------------

datos_reducido1 %>%
  count(`Existencia de baño`) %>%
  mutate(porcentaje = n / sum(n),
         etiqueta = paste0( round(porcentaje * 100), "%")) %>%
  ggplot() +
  aes(x = "", y = porcentaje, fill = `Existencia de baño`)+
  geom_col(width = 1,color='black') +                    # Barras apiladas
  coord_polar(theta = "y") +              # Convertir a torta
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5)) +
  ggtitle(" Acceso a baño o letrina en Barrios populares de CABA, año 2022") +
  scale_fill_manual(values = c("#4E79A7",  # Azul fuerte y profesional
                               "#F28E2B",  # Naranja cálido y alegre
                               "#E15759"))+
  theme_void()





