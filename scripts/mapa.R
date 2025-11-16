# install.packages("devtools")
# devtools::install_github("ropensci/rnaturalearthhires")
# devtools::install_github("ropensci/rnaturalearth")

library(rnaturalearth)
library(sf)
library(ggplot2)
library(patchwork)
library(leaflet) # Para hacer el mapa interactivo
library(sf)

###################       GENERACIÓN DEL MAPA       ##############################
map2 <- rnaturalearth::ne_states("Mexico", returnclass = "sf") # Obt datos espaciales de mexico
unique(map2$name) # Ver nombre de estados
ggplot(map2) + geom_sf() # Comprobar mapa

#######   Calcular casos totales
casos_por_estado <- nueva %>%
  group_by(estado) %>%
  summarise(positivos = sum(positivos, na.rm = TRUE)) # Sumar casos por estado

#######   Agregar/enlazar datos (num de casos) al mapa
mapa_dengue <- map2 %>%
  left_join(casos_por_estado, by = c("name" = "estado")) # Unir mapa con los datos 


#####################         MAPA   ANUAL        ##############################
# Establecer escala de valores para generar los colores del mapa
pal <- colorNumeric( # Función de colores
  palette = "PuBuGn",
  domain = mapa_dengue$positivos # Establecer valores min. y max. 
)

# Mapa intercativo
leaflet(mapa_dengue) %>%
  
  addProviderTiles("CartoDB.Positron") %>%   # Fondo claro
  
  # Colorear por numero de casos para cada estado
  addPolygons(
    fillColor = ~pal(positivos),
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    
  #Agregar texto informativo de casos para cada estado
    label = ~paste0(name, "; Casos: ", positivos), # Leyenda
    highlightOptions = highlightOptions( # Formato
      color = "white", # Color de fondo
      bringToFront = TRUE # Posición
    )
  ) %>%
  
  # Escala de colores
  addLegend(pal = pal,
    values = ~positivos,
    title = "Casos positivos"
  )


######################          MAPA MENSUAL               #####################
# Ver diferencias por mes

df_estados_inc # Incidencia de cada pais por mes

incidencia_mes <- df_estados_inc %>% mutate(estado = 
                            if_else(ENTIDAD == 10, "Durango",
                            if_else(ENTIDAD == 11, "Guanajuato",
                            if_else(ENTIDAD == 12, "Guerrero",
                            if_else(ENTIDAD == 13, "Hidalgo",
                            if_else(ENTIDAD == 15, "Jalisco",
                            if_else(ENTIDAD == 16, "México",
                            if_else(ENTIDAD == 17, "Michoacán", 
                            if_else(ENTIDAD == 18, "Morelos", 
                            if_else(ENTIDAD == 19, "Nayarit",
                            if_else(ENTIDAD == 20, "Nuevo León",
                            if_else(ENTIDAD == 21, "Oaxaca",
                            if_else(ENTIDAD == 22, "Puebla",
                            if_else(ENTIDAD == 23, "Querétaro",
                            if_else(ENTIDAD == 24, "Quintana Roo",
                            if_else(ENTIDAD == 25, "San Luis Potosí",
                            if_else(ENTIDAD == 26, "Sinaloa",
                            if_else(ENTIDAD == 27, "Sonora",
                            if_else(ENTIDAD == 28, "Tabasco", 
                            if_else(ENTIDAD == 29, "Tamaulipas", 
                            if_else(ENTIDAD == 30, "Tlaxcala", 
                            if_else(ENTIDAD == 31, "Veracruz", "Yucatán")))))))))))))))))))))) %>% print()

mapa_dengue2 <- left_join(map2, incidencia_mes, by = c("name" = "estado"))

ggplot(mapa_dengue2) + geom_sf(aes(fill = inc_mes)) +
  facet_wrap(~MES, dir = "h", ncol = 7) +
  ggtitle("Incidencia de Dengue por mes") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "purple"
  )
