library(tidyverse)
library(sf)

puna_aeropuertos <- read_rds('salidas/puna_aeropuertos_osrm.rds')

puna_aeropuertos <- puna_aeropuertos %>%
  unnest_wider('osrm')

puna_aeropuertos <- puna_aeropuertos %>%
  select(-c(38:39))

puna_aeropuertos <- puna_aeropuertos %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(distances, durations))

# armado de variables proxy |> pasar df a 1 fila x localidad puna
puna_aeropuertos %>% 
  select(-c(14:22, 24:26,28:34)) %>% 
  group_by(provincia.x, departamento_partido, localidad) %>% 
  mutate(vuelos_aero_cerca = frecuencias[which.min(duracion)],
         pax_aero_cerca = pasajeros[which.min(duracion)],
         tiempo_aero_cerca = duracion[which.min(duracion)],
         nom_aero_cerca =  aeropuerto_etiqueta_anac[which.min(duracion)],
         vuelos_aero_ppal = frecuencias[which.max(frecuencias)],
         pax_aero_ppal = pasajeros[which.max(frecuencias)],
         tiempo_aero_ppal = duracion[which.max(frecuencias)],
         nom_aero_ppal =  aeropuerto_etiqueta_anac[which.max(frecuencias)],
         ) %>% view
