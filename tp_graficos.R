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
                                         "A través de una conexión con medidor a la red pública" = "Red con medidor",
                                         "A través de un camión cisterna" = "Camión cisterna",
                                         "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”"
                                         = "Informalmente",
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
           fill = '#FDBE85',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Porcentaje(%)", x = "Tipo de acceso al agua") + # Nombres de ejes
  
  ggtitle(" Tipo de acceso al agua de Barrios populares de CABA, año 2022") +
  
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )# Temas preconfigurados de R https://r-charts.com/ggplot2/themes/


#----------------------CANT DE ABONOS MOVILES------------------------------------------------

# Para ver cual es maximo valor de esa categoria
max(datos_reducido1$`Cant abono datos moviles`, na.rm = TRUE)

datos_reducido1 %>% group_by(`Cant abono datos moviles`) %>% 
  ggplot() + 
  
  #aes(x = `Cant abono datos moviles`) + # Frecuencias absolutas
  #aes(x = reorder(`Cant abono datos moviles`, `Cant abono datos moviles`, function(x) -length(x))) + # Ordenar según frecuencia
  aes(x = `Cant abono datos moviles`, y = ..count.. / sum(..count..)) + # Porcentajes
  # aes(x = reorder(tiempo, tiempo, function(x) -length(x)), 
  #		y = ..count.. / sum(..count..)) +  # Porcentajes ordenados según frecuencia
  scale_y_continuous(labels = scales::percent) +    # Eje para porcentajes
  
  geom_bar(width = 0.75,   # Ancho de barras
           fill = '#FDBE85',  # Color de relleno 
           col = "black",  # Color de línea
           alpha = 0.6) +  # Transparencia
  
  labs(y = "Porcentaje(%)", x = "Cantidad de abonos moviles") + # Nombres de ejes
  
  ggtitle("Cantidad de abonos moviles en Barrios populares de CABA, año 2022") +
  
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#La mediana 
median(datos_reducido1$`Cant abono datos moviles`, na.rm = TRUE)


#----------------------Acceso a baño o letrina------------------------------------------------

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
  scale_fill_manual(values = c("#FDBE85",  # Azul fuerte y profesional
                               "#A6CEE3",  # Naranja cálido y alegre
                               "#FB9A99"))+
  theme_void() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
  )

#----------------------Tipo de calefaccion------------------------------------------------

datos_reducido1 %>%
  pivot_longer(cols = starts_with("Tipo calefaccion"),
               names_to = "tipo_columna",
               values_to = "tipo_calefaccion") %>%
  filter(!is.na(tipo_calefaccion),
         tipo_calefaccion != "No usa",
         tipo_calefaccion != "No necesito calefaccionar mi vivienda en ninguna época del año") %>%
  count(tipo_calefaccion) %>%
  mutate(porcentaje = n / sum(n),
         etiqueta = paste0(round(porcentaje * 100), "%")) %>%
  ggplot(aes(x = reorder(tipo_calefaccion, porcentaje), y = porcentaje)) +
  geom_col(fill = "#FDBE85", color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Tipos de calefacción en Barrios populares de CABA, año 2022",
       x = "Tipo de calefacción",
       y = "Porcentaje(%)") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

#----------------------Costo alquier------------------------------------------------

datos_reducido1 %>%
ggplot() +
  aes(x = `Costo del alquiler`,
      y = after_stat(count) / sum(after_stat(count))) +
  scale_y_continuous(labels = scales::percent)+
  geom_histogram(fill = "#FDBE85", col = "black", 
                 breaks = seq(4000, 30000, 4000)) +
  scale_x_continuous(breaks = seq(4000, 30000, 4000)) +
  labs(title = "Precio de los alquileres en Barrios populares de CABA, año 2022" , 
       x = "Precio del alquiler ($Pesos)", 
       y = "Porcentaje(%)")+ theme(
         plot.title = element_text(face = "bold", hjust = 0.5),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold")
       )

#promedio
mean(datos_reducido1$`Costo del alquiler`, na.rm = TRUE)

#----------------------Tipo de conexión a la red con la frecuencia de cortes eléctricos en verano------------------------------------------------

datos_reducido1 %>% 
  mutate(`Tipo conexio electrica` = recode(`Tipo conexio electrica`,
                                           "Conexión a través de un medidor a la red eléctrica" = "Con medidor",
                                           "Conexión sin medidor a una red eléctrica (“informal”)" = "Informal",
                                           "No posee conexión a la red eléctrica en la vivienda" = "No posee",
                                           "Conexión a través de un medidor comunitario a la red eléctrica" = "Medidor\n comunitario")) %>%
  filter(`Tipo conexio electrica` != "no posee") %>%
  count(`Tipo conexio electrica`, `Frec de cortes electricos`) %>%
  group_by(`Tipo conexio electrica`) %>%
  mutate(prop = n / sum(n)) %>%  # ⚠️ SIN multiplicar por 100
  ggplot() +
  aes(x = `Tipo conexio electrica`, y = prop, fill = `Frec de cortes electricos`) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Frecuencia de cortes eléctricos segun tipo de conexión en la red en Barrios populares de CABA, año 2022"
       ,x = "Tipo de conexión a la red", 
       y = "Porcentaje(%)", 
       fill = "Frecuencia de cortes eléctricos") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


#----------------------Cantidad de integrantes en vivienda y posibilidad de tener un celular con acceso a internet------------------------------------------------

ggplot(datos_reducido1) +
  aes(x = `Integrantes en vivienda`, y = `Celular con acceso a internet`) +
  geom_boxplot(show.legend = F, fill = "#FDBE85") +
  labs(x = "Cantidad integrantes en la vivienda", y = "Posibilidad de acceder a internet") +
  #coord_flip() +
  ggtitle("Cantidad de integrantes en vivienda y posibilidad de acceder a internet en Barrios populares de CABA, año 2022") +
  theme_light() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

  #mediana
  datos_reducido1 %>%
  group_by(`Celular con acceso a internet`) %>%
  summarise(mediana_integrantes = median(`Integrantes en vivienda`, na.rm = TRUE))

  #----------------------Cantidad de integrantes en la vivienda y Cantidad de ambientes que se usan como dormitorio. ------------------------------------------------

ggplot(datos_reducido1) +
  aes(x = `Integrantes en vivienda`, y = `Ambientes que se usan como dormitorios`) +
  geom_point() +
  geom_jitter(width = 0.4, # Jitter horizontal
              height = 0.4, # Jitter vertical
              alpha = 0.4, # Transparencia
              color = "blue") + # Color de los puntos jitter
  labs(x = "Integrantes por vivienda", y = "Ambientes que se usan como dormitorio")+
  ggtitle("Relación entre integrantes en las viviendas y cantidad de dormitorios en Barrios populares de CABA, año 2022") +
  theme_bw() + theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
