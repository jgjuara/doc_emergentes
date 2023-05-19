library(tidyverse)
library(sf)

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

# armado de variables proxy |> pasar df a 1 fila x localidad puna
tabla <- puna_aeropuertos %>% 
  # excluyo palomar
  filter(!str_detect(puna_aeropuertos$aeropuerto_etiqueta_anac, 'Palomar')) %>% 
  select(-c(9:13)) 

tabla <- tabla %>%
  select(-c(13:34)) %>% 
  group_by(provincia.x, departamento_partido, localidad) %>% 
  mutate(vuelos_aero_cerca = frecuencias[which.min(duracion)],
         pax_aero_cerca = pasajeros[which.min(duracion)],
         tiempo_aero_cerca = duracion[which.min(duracion)],
         rutas_mas_cerca = rutas[which.min(duracion)], 
         internacional_mas_cerca = internacional[which.min(duracion)],
         vuelos_aero_ppal = frecuencias[which.max(pasajeros)],
         pax_aero_ppal = pasajeros[which.max(pasajeros)],
         tiempo_aero_ppal = duracion[which.max(pasajeros)],
         rutas_mas_cerca = rutas[which.max(pasajeros)], 
         internacional_mas_cerca = internacional[which.max(pasajeros)],
         )

tabla <- tabla %>%
  distinct(pick(-c(9:14)))

# tabla <- tabla %>%
#   filter(! str_detect(localidad, 'ciudad de buenos aires|mar del plata'))
  
tabla1 <- tabla %>% 
  ungroup() %>% 
  select(where(is.numeric))


distancias <- tabla1 %>% 
  scale() %>% 
  dist() 

indice1D <- stats::cmdscale(distancias, k = 1)


# tabla %>% 
#   bind_cols(indice1D) %>% 
#   view()

hc_completo <- distancias %>% 
  hclust(method = 'complete')

plot(hc_completo )
rect.hclust(hc_completo, k=3,
            border="red")

grupos_c <-cutree(hc_completo,k=3)
grupos_c <- split(rownames(tabla1),grupos_c)

grupos_c

tabla[grupos_c$`3`,] # las grutas y bariloche
tabla[grupos_c$`2`,] # caba y mardel


hc_upgma  <- distancias %>% 
  hclust(method = 'average')

plot(hc_upgma )
rect.hclust(hc_upgma, k=3, border="red")

grupos_a<-cutree(hc_upgma,k=3)
grupos_a <- split(rownames(tabla1),grupos_a)
 
tabla[grupos_a$`3`,] # las grutas
tabla[grupos_a$`2`,] # caba y mardel

cor(x = distancias, cophenetic(hc_completo))

cor(x = distancias, cophenetic(hc_upgma))

######

tabla2 <- tabla %>% 
  ungroup() %>% 
  select(where(is.numeric))

tabla2 <- tabla2 %>% 
  select(!contains('ppal'))

tabla2 <- tabla2 %>% 
  mutate(across(-contains('rutas|internacional|tiempo'), ~ log(.x+0.000001)))

distancias <- tabla2 %>% 
  scale() %>% 
  dist() 

indice1D <- stats::cmdscale(distancias, k = 1)


# tabla %>% 
#   bind_cols(indice1D) %>% 
#   view()

hc_completo <- distancias %>% 
  hclust(method = 'complete')

plot(hc_completo )
rect.hclust(hc_completo, k=3,
            border="red")

grupos_c <-cutree(hc_completo,k=3)
grupos_c <- split(rownames(tabla1),grupos_c)

grupos_c

tabla[grupos_c$`3`,] %>% view # localidades muy chicas y muy lejos de aeropuertos
tabla[grupos_c$`2`,] %>% view # caba  mardel barilo y las grutas


# hc_upgma  <- distancias %>% 
#   hclust(method = 'average')

cor(x = distancias, cophenetic(hc_completo))

##### 



