
#tasa de incidencia por año

library(readr)
#datos de poblacion por estado de 2020 (inegi)
población_por_estado <- read_csv("RAW DATA/poblacion_por_estado.csv")

library(tidyverse)
poblacion_estatal<- read_csv ("RAW DATA/poblacion_por_estado.csv") %>% 
  mutate (Total= as.numeric( gsub ("," , "" , Total)))#el gsub es para que quite las comas


casos_totales <- tibble( entidad_federativa = c (
  "Durango", 
  "Guanajuato", 
  "Guerrero", 
  "Hidalgo", 
  "Jalisco", 
  "Estado de México", 
  "Michoacán de Ocampo", 
  "Morelos", 
  "Nayarit", 
  "Nuevo León", 
  "Oaxaca", 
  "Puebla", 
  "Querétaro", 
  "Quintana Roo", 
  "San Luis Potosí", 
  "Sinaloa", 
  "Sonora", 
  "Tabasco", 
  "Tamaulipas", 
  "Tlaxcala", 
  "Veracruz de Ignacio de la Llave", 
  "Yucatán", 
  "Zacatecas"),
  casos= c (sum(dg_inc$positivos), #sumamos los casos dorante todo el año de cada estado para tener nro de casos nuevos durante un periodo
            sum(gto_inc$positivos),
            sum(gr_inc$positivos),
            sum(hg_inc$positivos),
            sum(ja_inc$positivos),
            sum(em_inc$positivos),
            sum(mi_inc$positivos),
            sum(mo_inc$positivos),
            sum(nt_inc$positivos),
            sum(nl_inc$positivos),
            sum(oa_inc$positivos),
            sum(pu_inc$positivos),
            sum(qro_inc$positivos),
            sum(qroo_inc$positivos),
            sum(slp_inc$positivos),
            sum(sin_inc$positivos),
            sum(so_inc$positivos),
            sum(tb_inc$positivos),
            sum(tm_inc$positivos),
            sum(tl_inc$positivos),
            sum(vz_inc$positivos),
            sum(yu_inc$positivos),
            sum(za_inc$positivos)
            )
) 



#ahora sacar la tasa de incidencia :)
library(dplyr)

incidencia_por_año<- poblacion_estatal %>%
  left_join(casos_totales, by = "entidad_federativa") %>%
  mutate(
    casos = replace_na(casos, 0),
    incidencia_multiplicada = (casos / Total) * 100000) %>%
  mutate(
    incidencia = (casos )
  )

view(incidencia_por_año)


########################
interpretacion<- paste(
   
  round(incidencia_por_año$incidencia_multiplicada, 0), "de cada 100,000 habitantes tuvieron dengue durante 2025 en ", incidencia_por_año$entidad_federativa) #roud es para que redonde pero le dije q me dejara solo con nuemros enteros

interpretacion 

###################
#grafica 
library(ggplot2)
library(RColorBrewer) #pa q se vea bonis

grafica_inc<- ggplot(incidencia_por_año, aes (x=entidad_federativa, y=incidencia_multiplicada))+
  geom_bar(stat = "identity", fill= "steelblue") + labs( title = "Incidencia de dengue por entidades federativas " ,
                                                    x="ENTIDAD FEDERATIVA",
                                                    y= "Incedencia por cada 100,000 habitantes")+
  theme(axis.text.x = element_text( angle= 65, hjust=1))
grafica_inc
