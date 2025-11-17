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


###########          INCIDENCIA POR MES    ###############   

incidencia_mes <- function (estado_inc, codigo_estado) {
  #Lista para que se añadan las inciedncias 
  lista_incidencias <- list()
  #Añdir columna que indique el mes con month 
  estado_inc <- estado_inc %>% mutate (MES = month (FECHA_SIGN_SINTOMAS))
  #Incidencia por mes -- el conteo total + añadir el nombre del estado 
  inc_mes  <- estado_inc %>% group_by (MES) %>%
    summarise (inc_mes = sum (positivos) ) %>%
    mutate (ENTIDAD = codigo_estado ) # le añadí para no confundirme con el estado
  
  return (inc_mes)
}


#Prueba: estados_inc <- incidencia_mes(dg_inc, 10)

# Relizar una lista con todas las incidencias
estado_lista <- list( dg_inc, gto_inc, gr_inc, hg_inc, ja_inc, em_inc, mi_inc, mo_inc,
  nt_inc, nl_inc, oa_inc, pu_inc, qro_inc, qroo_inc, slp_inc, sin_inc,
  so_inc, tb_inc, tm_inc, tl_inc, vz_inc, yu_inc, za_inc )

#Aplicar la función de incidencia por mes a cada uno de los estadoa 
df_estados_inc <- data.frame()
codigos_estados <- 10:32

for (i in 1:length(estado_lista)) {
  df_estados_inc <- bind_rows ( #bind rows permite que los df se añadan y NO se reescriban 
    df_estados_inc,
    incidencia_mes (estado_lista [[i]], codigos_estados[i])
  )
}

df_estados_inc #la incidencia por cada uno de los eatados del país


# as.integer(codigo_nombre)-> c
codigo_nombre <- c("DURANGO",
                   "GUANAJUATO",
                   "GUERRERO",
                   "HIDALGO",
                   "JALISCO",
                   "MÉXICO",
                   "MICHOACÁN",
                   "MORELOS",
                   "NAYARIT",
                   "NUEVO LEON",
                   "OAXACA",
                   "PUEBLA",
                   "QUERÉTARO",
                   "QUINTANA",
                   "SAN LUIS POTOSI",
                   "SINALOA",
                   "SONORA",
                   "TABASCO",
                   "TAMAULIPAS",
                   "TLAXCALA",
                   "VERACRUZ",
                   "YUCATÁN",
                   "ZACATECAS"
)






ggplot(df_estados_inc, 
       aes(x = MES, 
           y = inc_mes, 
           group = ENTIDAD,
           color = as.factor(ENTIDAD))) +
  geom_line(linewidth =  1) +
  geom_point() +
  labs(x = "Mes",
       y = "Incidencia mensual",
       color = "Entidad") +
  theme_minimal()



############# MAPEO DE INCIDENCIA  #############
#install.packages ("sf")
install.packages ("mapview")

library (sf)
library (mapview)

mapview()

 







