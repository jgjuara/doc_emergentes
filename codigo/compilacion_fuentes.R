library(tidyverse)
library(herramientas)

anio_corte <- 2022

# puna --------------------------------------------------------------------


puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra_2022.gpkg") %>%
  mutate(anio = anio_corte)

puna_geo_dist <- puna %>% 
  distinct(provincia, departamento_partido, localidad, cod_pcia, cod_depto, geom)


# aero --------------------------------------------------------------------


aeropuertos <- read_rds("salidas/puna_aeropuertos_osrm.rds")

anti_join(puna_geo_dist, aeropuertos, by = c("provincia" = "provincia.x", "departamento_partido" = "departamento_partido",
                                             "localidad" = "localidad")) %>% 
  st_drop_geometry() %>% 
  filter(!is.na(cod_pcia)) %>% view

dict_aeros_puna <- aeropuertos %>% 
  distinct(provincia.x, departamento_partido, localidad, aeropuerto_etiqueta_anac, oaci, osrm)

dict_aeros_puna <- dict_aeros_puna %>% 
  unnest_wider(col = osrm)

read_file_srv("/srv/")

