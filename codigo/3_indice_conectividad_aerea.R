library(tidyverse)
library(sf)

source('codigo/1_tabla_aeropuertos.R')

puna_aeropuertos <- read_rds('salidas/puna_aeropuertos_osrm.rds')

puna_aeropuertos <- puna_aeropuertos %>%
  unnest_wider('osrm')

puna_aeropuertos <- puna_aeropuertos %>%
  select(-c(12:35))

puna_aeropuertos <- left_join(puna_aeropuertos, aeropuertos)

puna_aeropuertos <- puna_aeropuertos %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(distances, durations))


write_rds(puna_aeropuertos, 'salidas/puna_aeropuertos.rds')

# armado de variables proxy |> pasar df a 1 fila x localidad puna
puna_aeropuertos <- puna_aeropuertos %>%
  # excluyo palomar
  filter(!str_detect(puna_aeropuertos$aeropuerto_etiqueta_anac, 'Palomar')) %>%
  select(-c(9:13))

puna_aeropuertos <- puna_aeropuertos %>%
  group_by(provincia.x, departamento_partido, localidad) %>%
  mutate(aero_mas_cerca = aeropuerto_etiqueta_anac[which.min(duracion)],
         vuelos_aero_cerca = max_frecuencias[which.min(duracion)],
         max_asientos_cerca = max_asientos[which.min(duracion)],
         tiempo_aero_cerca = duracion[which.min(duracion)],
         indice_cabotaje_mas_cerca = indice_cabotaje[which.min(duracion)],
         indice_int_mas_cerca = indice_int[which.min(duracion)],
         aero_ppal = aeropuerto_etiqueta_anac[which.max(max_asientos)],
         vuelos_aero_ppal = max_frecuencias[which.max(max_asientos)],
         pax_aero_ppal = max_asientos[which.max(max_asientos)],
         tiempo_aero_ppal = duracion[which.max(max_asientos)],
         indice_cabotaje_mas_cerca = indice_cabotaje[which.max(max_asientos)],
         indice_int_mas_cerca = indice_int[which.max(max_asientos)]
         )
# 
puna_aeropuertos <- puna_aeropuertos %>%
  distinct(pick(-c(8:21)))

write_rds(puna_aeropuertos, 'salidas/puna_aeropuertos.rds')


# 
# # tabla <- tabla %>%
# #   filter(! str_detect(localidad, 'ciudad de buenos aires|mar del plata'))
#   
# tabla1 <- tabla %>% 
#   ungroup() %>% 
#   select(where(is.numeric))
# 
# 
# distancias <- tabla1 %>% 
#   scale() %>% 
#   dist() 
# 
# indice1D <- stats::cmdscale(distancias, k = 1)
# 
# 
# # tabla %>% 
# #   bind_cols(indice1D) %>% 
# #   view()
# 
# hc_completo <- distancias %>% 
#   hclust(method = 'complete')
# 
# plot(hc_completo )
# rect.hclust(hc_completo, k=3,
#             border="red")
# 
# grupos_c <-cutree(hc_completo,k=3)
# grupos_c <- split(rownames(tabla1),grupos_c)
# 
# grupos_c
# 
# tabla[grupos_c$`3`,] # las grutas y bariloche
# tabla[grupos_c$`2`,] # caba y mardel
# 
# 
# hc_upgma  <- distancias %>% 
#   hclust(method = 'average')
# 
# plot(hc_upgma )
# rect.hclust(hc_upgma, k=3, border="red")
# 
# grupos_a<-cutree(hc_upgma,k=3)
# grupos_a <- split(rownames(tabla1),grupos_a)
#  
# tabla[grupos_a$`3`,] # las grutas
# tabla[grupos_a$`2`,] # caba y mardel
# 
# cor(x = distancias, cophenetic(hc_completo))
# 
# cor(x = distancias, cophenetic(hc_upgma))
# 
# ######
# 
# tabla2 <- tabla %>% 
#   ungroup() %>% 
#   select(where(is.numeric))
# 
# tabla2 <- tabla2 %>% 
#   select(!contains('ppal'))
# 
# tabla2 <- tabla2 %>% 
#   mutate(across(-contains('rutas|internacional|tiempo'), ~ log(.x+0.000001)))
# 
# distancias <- tabla2 %>% 
#   scale() %>% 
#   dist() 
# 
# indice1D <- stats::cmdscale(distancias, k = 1)
# 
# 
# # tabla %>% 
# #   bind_cols(indice1D) %>% 
# #   view()
# 
# hc_completo <- distancias %>% 
#   hclust(method = 'complete')
# 
# plot(hc_completo )
# rect.hclust(hc_completo, k=3,
#             border="red")
# 
# grupos_c <-cutree(hc_completo,k=3)
# grupos_c <- split(rownames(tabla1),grupos_c)
# 
# grupos_c
# 
# tabla[grupos_c$`3`,] %>% view # localidades muy chicas y muy lejos de aeropuertos
# tabla[grupos_c$`2`,] %>% view # caba  mardel barilo y las grutas
# 
# 
# # hc_upgma  <- distancias %>% 
# #   hclust(method = 'average')
# 
# cor(x = distancias, cophenetic(hc_completo))
# 
# ##### 
# 
# 
# 
