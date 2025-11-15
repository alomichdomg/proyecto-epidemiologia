##############        INCIDENCIA POR ESTADO        ##############  
library(tidyverse)
library(dplyr)


#### Función de incidencia 

incidencia_completa <- function (estado) {
  #Fecha tipo as.Date
  estado$FECHA_SIGN_SINTOMAS <- as.Date(estado$FECHA_SIGN_SINTOMAS) #La puse porque no lo lee como Date
  
  #Incidencia diaria 
  incidencia_day <- estado %>% count (FECHA_SIGN_SINTOMAS, name = "positivos")  %>% 
    arrange (FECHA_SIGN_SINTOMAS)  %>% #ordenar por fecha
    mutate (num_dia = row_number())
  
  #Completar fechas que faltan con 0 
  incidencia_full <- incidencia_day %>% complete(FECHA_SIGN_SINTOMAS = seq.Date(min(FECHA_SIGN_SINTOMAS), 
                                                                        max(FECHA_SIGN_SINTOMAS), 
                                                                        by = "day"))  %>%
    mutate (positivos = ifelse(is.na(positivos), 0, positivos),
            num_dia = row_number())  
  
  return(incidencia_full)
}


###### INCIDENCIA COMPLETA POR ESTADO   ######

#Durango
dg_inc <- incidencia_completa (durango)

#Guanajuato 
gto_inc <- incidencia_completa (guanajuato)

#Guerrero
gr_inc <- incidencia_completa(guerrero)

#Hidalgo 
hg_inc <- incidencia_completa(hidalgo)

#Jalisco
ja_inc <- incidencia_completa(jalisco)

#México 
em_inc <- incidencia_completa(mexico.filtrado)

#Michoacán
mi_inc <- incidencia_completa(michoacan)

#Morelos
mo_inc <- incidencia_completa(morelos)

#Nayarit 
nt_inc <- incidencia_completa(nayarit)

#Nuevo León
nl_inc <- incidencia_completa(nuevo.leon)

#Oaxaca
oa_inc <- incidencia_completa(oaxaca)

#Puebla
pu_inc <-incidencia_completa(puebla)

#Querétaro
qro_inc <- incidencia_completa(queretaro)

#Quintana Roo
qroo_inc <- incidencia_completa (quintana.roo)

#San Luis Potosí 
slp_inc <- incidencia_completa(san.luis.potosi)

#Sinaloa 
sin_inc <- incidencia_completa(sinaloa)

#Sonora
so_inc <- incidencia_completa (sonora)

#Tabasco 
tb_inc <- incidencia_completa(tabasco)

#Tamaulipas 
tm_inc <- incidencia_completa(tamaulipas)

#Tlaxcala
tl_inc <- incidencia_completa(tlaxcala)

#Veracruz 
vz_inc <- incidencia_completa(veracruz)

#Yucatán 
yu_inc <- incidencia_completa (yucatan)

#Zacatecas
za_inc <- incidencia_completa (zacatecas)



## INCIDENCIA POR MES 
# Añadir columna que indique el mes
dg_inc_mes <- dg_inc %>% mutate(MES = month(FECHA_SIGN_SINTOMAS))

# b) Pico de incidencia por mes 
dg_pico <- dg_inc_mes %>% group_by(MES) %>%
  summarise (max_dia = max(MES) ) #pico en oct




## Histoframa 
ggplot(dg_inc, aes(x = FECHA_SIGN_SINTOMAS, y = positivos)) +
  geom_col(fill = "seagreen3") +
  labs(title = "Incidencia diaria en Durango",
       x = "Fecha", y = "Casos diarios")

 







