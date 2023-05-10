library(tidyverse)
library(arrow)
library(comunicacion)

base_total <- herramientas::read_file_srv("/srv/DataDNMYE/aerocomercial/anac/base_anac_agrupada_diaria.parquet")

base_total <- base_total %>%
  pivot_longer(cols = c(origen_oaci, destino_oaci),
               names_to = "variable",
               values_to = "oaci") %>%
  filter(oaci != "EGYP" & anio_local == 2023) %>%
  group_by(oaci) %>%
  summarise(pasajeros_media_mensual = sum(pax_ad, na.rm = T)/n_distinct(mes_local),
            frecuencias_media_mensual = sum(vuelos, na.rm = T)/n_distinct(mes_local)) %>%
  ungroup()


aeropuertos <- readxl::read_xlsx("/srv/DataDNMYE/aerocomercial/libros_de_codigos/ANAC/aeropuertos_nuevos.xlsx", col_types = "text") %>%
  mutate(aeropuerto_etiqueta_anac = case_when(is.na(aeropuerto_etiqueta_anac) ~ nombre_ingles,
                                              T ~ aeropuerto_etiqueta_anac),
         localidad_etiqueta_indec = case_when(is.na(localidad_etiqueta_indec) ~ localidad_ingles,
                                              T ~ localidad_etiqueta_indec))

base_total <- base_total %>%
  left_join(aeropuertos, by = c("oaci" = "codigo_oaci"))

base_total <- base_total %>%
  filter(!is.na(provincia))

aeropuertos <- base_total %>%
  st_as_sf(coords = c('longitud', 'latitud'), crs = 4326)
