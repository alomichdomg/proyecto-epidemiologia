library(owidR)
library(tidyr)
library(tidyverse)
library(dplyr)
##------------------------------------------------------------------------------
dengue_datos <- read_csv("RAW DATA/dengue_abierto.csv")
#-------------------------------------------------------------------------------
# Filtrado de los datos por estado
#-------------------------------------------------------------------------------

# DURANGO
durango <- dengue_datos %>% filter(ENTIDAD_RES == "10")
# save(durango, file = "03_out/durango.RData")

# GUANAJUATO
guanajuato <- dengue_datos %>% filter(ENTIDAD_RES == "11")
# save(guanajuato, file = "03_out/guanajuato.RData")

# GUERRERO
guerrero <- dengue_datos %>% filter(ENTIDAD_RES == "12")
# save(guerrero, file = "03_out/guerrero.RData")

# HIDALGO
hidalgo <- dengue_datos %>% filter(ENTIDAD_RES == "13")
# save(hidalgo, file = "03_out/hidalgo.RData")

# JALISCO
jalisco <- dengue_datos %>% filter(ENTIDAD_RES == "14")
# save(jalisco, file = "03_out/jalisco.RData")

# MÉXICO
mexico.filtrado <- dengue_datos %>% filter(ENTIDAD_RES == "15")
# save(mexico.filtrado, file = "03_out/mexico.filtrado.RData")

# MICHOACÁN DE OCAMPO
michoacan <- dengue_datos %>% filter(ENTIDAD_RES == "16")
# save(michoacan, file = "03_out/michoacan.RData")

# MORELOS
morelos <- dengue_datos %>% filter(ENTIDAD_RES == "17")
# save(morelos, file = "03_out/morelos.RData")

# NAYARIT
nayarit <- dengue_datos %>% filter(ENTIDAD_RES == "18")
# save(nayarit, file = "03_out/nayarit.RData")

# NUEVO LEÓN
nuevo.leon <- dengue_datos %>% filter(ENTIDAD_RES == "19")
# save(nuevo.leon, file = "03_out/nuevo.leon.RData")

# OAXACA
oaxaca <- dengue_datos %>% filter(ENTIDAD_RES == "20")
# save(oaxaca, file = "03_out/oaxaca.RData")

# PUEBLA
puebla <- dengue_datos %>% filter(ENTIDAD_RES == "21")
# save(puebla, file = "03_out/puebla.RData")

# QUERÉTARO
queretaro <- dengue_datos %>% filter(ENTIDAD_RES == "22")
# save(queretaro, file = "03_out/queretaro.RData")

# QUINTANA ROO
quintana.roo <- dengue_datos %>% filter(ENTIDAD_RES == "23")
# save(quintana.roo, file = "03_out/quintana.roo.RData")

# SAN LUIS POTOSÍ
san.luis.potosi <- dengue_datos %>% filter(ENTIDAD_RES == "24")
# save(san.luis.potosi, file = "03_out/san.luis.potosi.RData")

# SINALOA
sinaloa <- dengue_datos %>% filter(ENTIDAD_RES == "25")
# save(sinaloa, file = "03_out/sinaloa.RData")

# SONORA
sonora <- dengue_datos %>% filter(ENTIDAD_RES == "26")
# save(sonora, file = "03_out/sonora.RData")

# TABASCO
tabasco <- dengue_datos %>% filter(ENTIDAD_RES == "27")
# save(tabasco, file = "03_out/tabasco.RData")

# TAMAULIPAS
tamaulipas <- dengue_datos %>% filter(ENTIDAD_RES == "28")
# save(tamaulipas, file = "03_out/tamaulipas.RData")

# TLAXCALA
tlaxcala <- dengue_datos %>% filter(ENTIDAD_RES == "29")
# save(tlaxcala, file = "03_out/tlaxcala.RData")

# VERACRUZ DE IGNACIO DE LA LLAVE
veracruz <- dengue_datos %>% filter(ENTIDAD_RES == "30")
# save(veracruz, file = "03_out/veracruz.RData")

# YUCATÁN
yucatan <- dengue_datos %>% filter(ENTIDAD_RES == "31")
# save(yucatan, file = "03_out/yucatan.RData")

# ZACATECAS
zacatecas <- dengue_datos %>% filter(ENTIDAD_RES == "32")
# save(zacatecas, file = "03_out/zacatecas.RData")

#-------------------------------------------------------------------------------
# AGUASCALIENTES
  #no hay casos registrados en aguascalientes
aguascalientes <- dengue_datos %>% filter(ENTIDAD_RES == "1")


# BAJA CALIFORNIA
# No hay casos regitrados:
baja.california <- dengue_datos %>% filter(ENTIDAD_RES == "2")


# BAJA CALIFORNIA SUR
# No hay casos regitrados:
baja.california.sur <- dengue_datos %>% filter(ENTIDAD_RES == "3")

# CAMPECHE
# No hay casos regitrados:
campeche <- dengue_datos %>% filter(ENTIDAD_RES == "4")

# COAHUILA DE ZARAGOZA
# No hay casos regitrados:
coahuila <- dengue_datos %>% filter(ENTIDAD_RES == "5")


# COLIMA
# No hay casos regitrados:
colima <- dengue_datos %>% filter(ENTIDAD_RES == "6")

# CHIAPAS
chiapas <- dengue_datos %>% filter(ENTIDAD_RES == "7")
save(chiapas, file = "03_out/chiapas.RData")

# CHIHUAHUA
# No hay casos regitrados:
chihuahua <- dengue_datos %>% filter(ENTIDAD_RES == "8")


# CIUDAD DE MÉXICO:
# No hay casos regitrados:
ciudad.mexico <- dengue_datos %>% filter(ENTIDAD_RES == "9")

#-------------------------------------------------------------------------------
