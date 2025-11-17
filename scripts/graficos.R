################################################################################
#                     Comparación de incidencia diaria 

###### Establecer nombres para cada base
estado <- "Durango"
dg_inc2<-  cbind(estado, dg_inc)

estado <- "Guanajuato"
gto_inc2 <- cbind(estado, gto_inc)

estado <- "Guerrero"
gr_inc2 <- cbind(estado, gr_inc)

estado <- "Hidalgo"
hg_inc2 <- cbind(estado, hg_inc)

estado <- "Jalisco"
ja_inc2 <- cbind(estado, ja_inc)

estado <- "México"
em_inc2 <- cbind(estado, em_inc)

estado <- "Michoacán"
mi_inc2 <- cbind(estado, mi_inc)

estado <- "Morelos"
mo_inc2 <- cbind(estado, mo_inc)

estado <- "Nayarit"
nt_inc2 <- cbind(estado, nt_inc)

estado <- "Nuevo León"
mo_inc2 <- cbind(estado, mo_inc)

estado <- "Oaxaca"
oa_inc2 <- cbind(estado, oa_inc)

estado <- "Puebla"
pu_inc2 <- cbind(estado, pu_inc)
  
estado <- "Querétaro"
qro_inc2 <- cbind(estado, qro_inc)

estado <- "Quintana Roo"
qroo_inc2 <- cbind(estado, qroo_inc)

estado <- "San Luis Potosí"
slp_inc2 <- cbind(estado, slp_inc)

estado <- "Sinaloa"
sin_inc2 <- cbind(estado, sin_inc)

estado <- "Sonora"
so_inc2 <- cbind(estado, so_inc)

estado <- "Tabasco"
tb_inc2 <- cbind(estado, tb_inc)

estado <- "Tamaulipas"
tm_inc2 <- cbind(estado, tm_inc)

estado <- "Tlaxcala"
tl_inc2 <- cbind(estado, tl_inc)

estado <- "Veracruz"
vz_inc2 <- cbind(estado, vz_inc)

estado <- "Yucatán"
yu_inc2 <- cbind(estado, yu_inc)

estado <- "Zacatecas"
za_inc2 <- cbind(estado, za_inc)
 
# Juntar (concatenar) en una sola base 
nueva <- rbind(dg_inc2,gto_inc2,gr_inc2,hg_inc2,ja_inc2,em_inc2,mi_inc2,mo_inc2,
               nt_inc2,mo_inc2,oa_inc2,pu_inc2,qro_inc2,qroo_inc2,slp_inc2,sin_inc2,
               so_inc2,tb_inc2,tm_inc2,tl_inc2,vz_inc2,yu_inc2,za_inc2)

# Para ver el total de los casos:
totales_por_estado <- function(data){
  data %>%
    group_by(estado) %>%
    summarise(
      total_positivos = sum(positivos, na.rm = TRUE),
      dias_reportados = n(), 
      .groups = "drop"
    )
}

# Casos totales por de todo el estudio:
totales_por_estado(nueva) -> incidencia.total.periodo
max(incidencia.total.periodo$total_positivos)

#PARA VER LOS ESTADOS CON MAYOR CASOS REPORTADOS
incidencia.total.periodo %>%
  arrange(desc(total_positivos)) 



# Gráfico
casos <- ggplot(nueva, aes(x = FECHA_SIGN_SINTOMAS, y = positivos, group = estado, color = estado)) + 
  geom_line() + geom_point (alpha = 0.5) + theme_bw() +
  labs(title = "Incidencia diaria de Dengue",
       x = "Fecha", y = "Casos")
casos

ggplotly(casos)

ggplot(nueva, aes(x = FECHA_SIGN_SINTOMAS, y = positivos)) +
  geom_line(color = "steelblue") +
  theme_bw() +
  facet_wrap(~ estado, scales = "free_y") +
  labs(title = "Incidencia diaria de Dengue por estado",
       x = "Fecha", y = "Casos")

