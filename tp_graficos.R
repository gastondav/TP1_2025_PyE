install.packages("googledrive")
install.packages("readxl")

https://docs.google.com/spreadsheets/d/16_zhdrZIW72I45SHIsVkGv-KYQw1oeup

googledrive::drive_download(as_id("16_zhdrZIW72I45SHIsVkGv-KYQw1oeup"), overwrite = T)

datos <- readxl::read_excel("arbol.xlsx", col_names = FALSE, skip = 3)

# Veo la estructura del dataset
str(datos)
