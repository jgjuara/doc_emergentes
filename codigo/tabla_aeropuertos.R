library(tidyverse)
library(arrow)
library(comunicacion)

base_total <- herramientas::read_file_srv("aerocomercial/anac/base_anac_agrupada_diaria.parquet")

base_total <- base_total %>%
  pivot_longer(cols = c(origen_oaci, destino_oaci),
               names_to = "variable",
               values_to = "oaci") %>%
  filter(oaci != "EGYP" & anio_local == 2019) %>%
  group_by(oaci, mes_local) %>%
  summarise(pasajeros = sum(pax_ad, na.rm = T),
            frecuencias = sum(ceiling(vuelos), na.rm = T)) %>%
  group_by(oaci) %>% 
  summarise(pasajeros = max(pasajeros),
            frecuencias = max(frecuencias)) %>% 
  ungroup()


aeropuertos <- herramientas::read_file_srv("aerocomercial/libros_de_codigos/ANAC/aeropuertos_nuevos.xlsx",
                                           col_types = "text") %>%
  mutate(aeropuerto_etiqueta_anac = case_when(is.na(aeropuerto_etiqueta_anac) ~ nombre_ingles,
                                              T ~ aeropuerto_etiqueta_anac),
         localidad_etiqueta_indec = case_when(is.na(localidad_etiqueta_indec) ~ localidad_ingles,
                                              T ~ localidad_etiqueta_indec))

base_total <- base_total %>%
  left_join(aeropuertos, by = c("oaci" = "codigo_oaci"))

base_total <- base_total %>%
  filter(iso_country == "AR")

aeropuertos <- base_total %>%
  st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)


aeros_regulares <- herramientas::read_file_srv("/srv/DataDNMYE/aerocomercial/tablero/mapa_conectividad.gpkg")


aeropuertos <- aeropuertos %>% 
  filter(aeropuertos$aeropuerto_etiqueta_anac %in%  unique(aeros_regulares$aeropuerto_etiqueta_anac.y, 
                                                      aeros_regulares$aeropuerto_etiqueta_anac.x))
