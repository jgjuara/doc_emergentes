library(tidyverse)

puna_aeropuertos <- read_rds('salidas/puna_aeropuertos_osrm.rds')

# puna_aeropuertos <- puna_aeropuertos %>% 
#   unnest_wider('osrm')
# 
# puna_aeropuertos <- puna_aeropuertos %>% 
#   select(-c(38:39))

puna_aeropuertos <- puna_aeropuertos %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(distances, durations))

puna_aeropuertos <- puna_aeropuertos %>% 
  select(-c(14:22, 24:26,28:34)) %>% 
  group_by(provincia.x, departamento_partido, localidad) %>% 
  mutate(aero_mas_cerca =  aeropuerto_etiqueta_anac[which.min(duracion)]) 
