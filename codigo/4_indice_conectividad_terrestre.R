library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
library(osrm)

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

## Terminales de ómnibus
#Capa IGN
terminales <- read_sf("entradas/terminales_omnibus.json") %>% 
  select(gid, gna, nam)


puna_terminales <- st_join(puna, terminales, join = sf::st_nearest_feature) %>% 
  as.data.frame() %>% 
  #mutate(x = as.character(geom)) %>% 
  #st_drop_geometry() %>% 
  left_join(terminales) #%>% mutate(y = as.character(geometry)) %>% st_drop_geometry())

ruteo <- function(x,y) {
  list(osrm::osrmTable(src = st_cast(x, "POINT"), 
                       dst = st_cast(y, "POINT"), 
                       measure = c('duration', 'distance'))
  )
}

j <- 1

for (i in j:nrow(puna_terminales)) {
  
  puna_terminales[i,] %>% 
    mutate(osrm = ruteo(geom, geometry)) %>% 
    write_rds(.,file = glue::glue("entradas/ruteos/terminales/puna_terminales_osrm_{i}.rds"))
  
  
  print(i)
  Sys.sleep(runif(1, min = 2, max = 3))
  
  j <- i
}

archivos <- list.files("entradas/ruteos/terminales/", full.names = T)

puna_terminales_osrm <- map(archivos, function(x) {read_rds(x)}) %>% 
  list_rbind()

puna_terminales_osrm <- bind_rows(puna_terminales_osrm) %>% 
  as_tibble()

puna_terminales_osrm <- puna_terminales_osrm %>%
  unnest_wider('osrm')

puna_terminales <- puna_terminales_osrm %>% 
  mutate(distancia = map_int(distances, function(x) {unlist(x)[[1]]}),
         duracion = map_dbl(durations, function(x) {unlist(x)[[1]]})) %>% 
  select(-c(13:16))

write_rds(puna_terminales, "salidas/puna_terminales_osrm.rds")


## Red vial
vias_nac <- read_sf("entradas/vial_nacional.json") %>%
  mutate(typ = case_when(typ == 40 ~ "Ruta",
                         typ == 41 ~ "Autopista",
                         typ == 47 ~ "Autovía",
                         TRUE  ~ "Sin dato"),
         rst = case_when(rst == 1 ~ "Pavimentado",
                         rst == 2 ~ "Consolidado",
                         rst == 3 ~ "Tierra",
                         TRUE ~ "Sin dato")) %>% 
  mutate(gna = "RN")

vias_prov <- read_sf("entradas/vial_provincial.json") %>%
  mutate(typ = case_when(typ == 40 ~ "Ruta",
                         typ == 41 ~ "Autopista",
                         typ == 47 ~ "Autovía",
                         typ == 49 ~ "Camino",
                         TRUE  ~ "Sin dato"),
         rst = case_when(rst == 1 ~ "Pavimentado",
                         rst == 2 ~ "Consolidado",
                         rst == 3 ~ "Tierra",
                         TRUE ~ "Sin dato")) %>% 
  mutate(gna = "RP")

#Agrupo tramos
group_vias_nac <- vias_nac %>% 
  group_by(rtn, typ, rst, gna) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

group_vias_prov <- vias_prov %>% 
  group_by(rtn, typ, rst, gna) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()


red_vial <- bind_rows(group_vias_nac, group_vias_prov)

red_vial <- st_set_crs(red_vial, value = 4326)

mapview::mapview(list(puna, red_vial))

#Distancia lineal a punto más cercano ruta pavimentada
#Distancia/tiempo a punto más cercano ruta pavimentada
#Cantidad de rutas pavimentadas en buffer
#Cantidad de rutas totales en buffer
test <- st_join(puna, red_vial, join = sf::st_nearest_feature)

