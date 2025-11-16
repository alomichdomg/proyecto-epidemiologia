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

# DURANGO
dg_inc <- incidencia_completa (durango)

# GUANAJUATO  
gto_inc <- incidencia_completa (guanajuato)

# GUERRERO 
gr_inc <- incidencia_completa(guerrero)

# HIDALGO  
hg_inc <- incidencia_completa(hidalgo)

# JALISCO 
ja_inc <- incidencia_completa(jalisco)

# MÉXICO  
em_inc <- incidencia_completa(mexico.filtrado)

# MICHOACAN 
mi_inc <- incidencia_completa(michoacan)

# MORELOS 
mo_inc <- incidencia_completa(morelos)

# NAYARIT  
nt_inc <- incidencia_completa(nayarit)

# NUEVO LEON
nl_inc <- incidencia_completa(nuevo.leon)

# OAXACA 
oa_inc <- incidencia_completa(oaxaca)

# PUEBLA 
pu_inc <-incidencia_completa(puebla)

# QUERETARO 
qro_inc <- incidencia_completa(queretaro)

# Q. ROO
qroo_inc <- incidencia_completa (quintana.roo)

# SAN LUIS POTOSI 
slp_inc <- incidencia_completa(san.luis.potosi)

# SINALOA  
sin_inc <- incidencia_completa(sinaloa)

# SONORA 
so_inc <- incidencia_completa (sonora)

# TABASCO  
tb_inc <- incidencia_completa(tabasco)

# TAMAULIPAS  
tm_inc <- incidencia_completa(tamaulipas)

# TLAXCALA 
tl_inc <- incidencia_completa(tlaxcala)

# VERACRUZ  
vz_inc <- incidencia_completa(veracruz)

# YUCATAN  
yu_inc <- incidencia_completa (yucatan)

# ZACATECAS 
za_inc <- incidencia_completa (zacatecas)


# Si se quiere graficar
#Histograma de casos 
ggplot (dg_inc, aes(x = FECHA_SIGN_SINTOMAS, y = positivos)) +
  geom_col (fill = "seagreen3") +
  labs(title = "Incidencia diaria en Durango",
       x = "Fecha", y = "Casos diarios")

## INCIDENCIA POR MES 
# Añadir columna que indique el mes

dg_inc <- dg_inc %>% mutate (MES = month(FECHA_SIGN_SINTOMAS))
#View (dg_inc)

# Inc. total por mes 
dg_mes <- dura_inc_mes %>% group_by (MES) %>%
  summarise (inc_mes = sum (positivos) )
dg_mes





dura <- dengue_datos %>% filter(ENTIDAD_RES == "10", ESTATUS_CASO == "2")
dura_inc <-  incidencia_completa(dura)

 







