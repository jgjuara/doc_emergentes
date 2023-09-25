library(tidyverse)
library(tidygeocoder)
library(herramientas)
library(sf)

readxl::excel_sheets("/srv/DataDNMYE/cnrt/backup/Pasajeros SR-2022.xlsx")

cnrt_ene_mar <- map(1:3, ~ read_file_srv("/srv/DataDNMYE/cnrt/backup/Pasajeros SR-2022.xlsx",
                                  sheet = .x,
                                  col_types = c("text", "date", "date",
                                                "text","text","text","text",
                                                "text"))) %>% 
  bind_rows() %>% 
  janitor::clean_names()


cnrt_abr_dic <- map(4:12, ~ read_file_srv("/srv/DataDNMYE/cnrt/backup/Pasajeros SR-2022.xlsx",
                                         sheet = .x,
                                         col_types = c("text", "date", "date","date",
                                                       "text","text","text","text",
                                                       "text"))) %>% 
  bind_rows() %>% 
  janitor::clean_names()


cnrt <- bind_rows(
  cnrt_ene_mar, cnrt_abr_dic
)

rm(list = c("cnrt_ene_mar", "cnrt_abr_dic"))

# cnrt %>% 
#   mutate(across(c(`Localidad Origen`, `Localidad Destino`), ~ tolower(.x))) %>% 
#   distinct(`Localidad Origen`, `Localidad Destino`)  %>% 
#   pivot_longer(cols = everything()) %>% distinct(value) %>% view
  
# usar diccionario 

cnrt <- cnrt %>% 
  mutate(pax = as.numeric(ifelse(is.na(cantidad_pasajeros), cantidad_de_pasajeros, cantidad_pasajeros)))

sum(cnrt$pax)

cnrt <- cnrt %>% 
  mutate(across(c(localidad_origen, provincia_origen,
                  localidad_destino, provincia_destino),
                tolower)) %>%
  group_by(localidad_origen, provincia_origen, localidad_destino, provincia_destino) %>% 
  summarise(pax = sum(pax)) %>% ungroup() %>% 
  mutate(across(c(localidad_origen, provincia_origen,
                  localidad_destino, provincia_destino),
                ~ herramientas::limpiar_texto(.x, enie = F))) %>%
  group_by(localidad_origen, provincia_origen, localidad_destino, provincia_destino) %>% 
  summarise(pax = sum(pax)) %>% ungroup()


####  control de autoridades -----------------

cnrt <- cnrt %>% 
  mutate(across(matches("localidad|provincia"), str_squish)) 

cnrt <- bind_rows(
  cnrt %>% 
    group_by(prov = provincia_destino, loc = localidad_destino) %>% 
    summarise(pax = sum(pax)),
  cnrt %>% 
    group_by(prov = provincia_origen, loc =localidad_origen) %>% 
    summarise(pax = sum(pax))
  ) %>%
  group_by(prov, loc) %>% 
  summarise(pax = sum(pax)) %>% 
  ungroup() %>% 
  arrange(desc(pax)) %>% 
  mutate(pax_acum = 100*cumsum(pax)/sum(pax))


cnrt <- cnrt %>% 
  filter(loc != "n" &
           ! prov %in% c("alto parana", "asuncion", "caaguazu",
                         "central", "cordillera", "guaira",
                         "itapua", "n", "paraguari",
                         "rio grande do sul", "santa catarina"))

cnrt <- cnrt %>% 
  mutate(prov = case_when(
    str_detect(loc, "ciudad autonoma|retiro|liniers|dellepiane") ~ "ciudad autonoma de buenos aires",
    T ~ prov
  ))

cnrt <- cnrt %>% 
  mutate(loc  = str_remove(loc, "bs as$| capital federal$| bue | bue$|bue |
                                 tuc$| sju$| slu|ctesaxion|
                                 sta fe$| sfe$| mis$|
                                 lpa$| cba$| rng$| rn$|mza |
                                 mza$| lpa$| lpm$| mzalpampa|rotonda|
                                 ctes$| cor$| sgo$| eri$| erios$| er$|001"))


cnrt <- cnrt %>% 
  mutate(across(.cols = matches("loc|prov"), ~ str_squish(.))
  ) %>% 
  mutate(loc = str_remove(loc,
                                             " acc$|acc |acceso") %>% 
                str_replace(., "pto ", "puerto ") %>% 
          str_replace(., " acc$|acc | acc |acceso", " ") %>% 
           str_replace(., "cdro", "comodoro") %>% 
           str_remove(., "pdor|parador") %>% 
           str_replace(., "term |terminal|teminal| tnal$", " ") %>% 
           str_replace(., "gral | gral$", " general ") %>% 
           str_remove(., " dover|gnc puma| ch$") %>% 
           str_replace(., "mza | mza$", " ") %>% 
           str_replace(., " lri$| lrj$", " ") %>% 
           str_replace(., " cat$", " ") %>% 
           str_replace(., " tuc$", " ") %>% 
           str_replace(., " cha$", " ") 
           ) %>% 
  mutate(across(.cols = matches("loc|prov"), ~ str_squish(.))) %>% 
  mutate(loc_clean = case_when(
    str_detect(loc, "colivia") & prov == "santa cruz" ~ "caleta olivia",
    str_detect(loc, "moreno las piedras") & prov == "buenos aires" ~ "moreno",
    str_detect(loc, "vergara") & prov == "buenos aires" ~ "moron",
    str_detect(loc, "cap sarmiento") & prov == "buenos aires" ~ "capitan sarmiento",
    str_detect(loc, "guemes") & prov == "salta" ~ "general guemes",
    str_detect(loc, "libertador") & prov == "entre rios" ~ "libertador san martin",
    str_detect(loc, "ocampo") & prov == "santa fe" ~ "villa ocampo",
    str_detect(loc, "merlo") & prov == "san luis" ~ "merlo",
    str_detect(loc, "combinacion") & prov == "san luis" ~ "san luis",
    str_detect(loc, "mercedes") & prov == "san luis" ~ "villa mercedes",
    str_detect(loc, "conlara") & prov == "san luis" ~ "santa rosa del conlara",
    str_detect(loc, "trunca") & prov == "santa cruz" ~ "pico truncado",
    str_detect(loc, "piedra") & prov == "santa cruz" ~ "luis piedra buena",
    str_detect(loc, "mdp") & prov == "buenos aires" ~ "mar del plata",
    str_detect(loc, "santa fe") & prov == "santa fe" ~ "cordoba",
    str_detect(loc, "corrientes") & prov == "corrientes" ~ "corrientes",
    str_detect(loc, "bariloche") ~ "san carlos de bariloche",
    str_detect(loc, "cordoba") ~ "cordoba",
    str_detect(loc, "moron") ~ "moron",
    str_detect(loc, "resistenciat") ~ "resistencia",
    str_detect(loc, "197|talar pch|talar") ~ "el talar",
    str_detect(loc, "eldorado") ~ "el dorado",
    str_detect(loc, "montecaseros") ~ "monte caseros",
    str_detect(loc, "santo tom") ~ "santo tome",
    str_detect(loc, "catamarca") & prov  == "catamarca" ~ "san fernando del valle de catamarca",
    str_detect(loc, "ptoiguaz") ~ str_replace(loc, "ptoiguaz", "puerto iguazu"),
    str_detect(loc, "cdro|c riv|comodoro|com rivadavia|rivada") & 
      prov == "chubut" ~ "comodoro rivadavia",
    str_detect(loc, "castelli") & prov == "chaco" ~ "juan jose castelli",
    str_detect(loc, "pico") & prov == "la pampa" ~ "general pico",
    str_detect(loc, "25 de mayo") & prov == "la pampa" ~ "colonia 25 de mayo",
    str_detect(loc, "tucuman|tucumn") & prov == "tucuman" ~ "san miguel de tucuman",
    str_detect(loc, "termas") & prov == "santiago del estero" ~ "termas de rio hondo",
    str_detect(loc, "ovanta") & prov == "catamarca" ~ "bañado de ovanta",
    str_detect(loc, "retiro") ~ "retiro",
    str_detect(loc, "liniers") ~ "liniers",
    str_detect(loc, "dellepiane") ~ "dellepiane",
    str_detect(loc, "vc paz") ~ "villa carlos paz",
    str_detect(loc, "mar d tuyu") ~ "mar de tuyu",
    str_detect(loc, "sta ") ~ str_replace(loc, "sta ", "santa "),
    str_detect(loc, "la parada de cdn") ~ str_replace(loc, "la parada de cdn", "san justo"),
    str_detect(loc, "j v gonzalez") ~ str_replace(loc, "j v gonzalez", "joaquin v gonzalez"),
    str_detect(loc, "jujuy") & prov == "jujuy" ~ "san salvador de jujuy",
    str_detect(loc, "estero|santiago") & prov == "santiago del estero" ~ "santiago del estero",
    str_detect(loc, "rosario") & prov == "santa fe"       ~ "rosario",
    T ~ loc
  )) %>% 
  mutate(across(.cols = matches("loc|prov"), ~ str_squish(.))
  ) %>% 
  group_by(prov, loc_clean) %>%
  summarise(pax = sum(pax)) %>%
  ungroup() %>%
  arrange(desc(pax)) %>%
  mutate(pax_acum = 100*cumsum(pax)/sum(pax)) %>%
  ungroup() 


cnrt %>% 
  write_rds("entradas/cnrt_lista_limpia.rds")

cnrt %>% 
  mutate(prop = pax/sum(pax)) %>% 
  filter(pax > 365) %>%
  nrow()

dict_cnrt_osm <- cnrt %>%
  mutate(direccion  = paste(loc_clean, prov, sep = ", ")) %>% 
  tidygeocoder::geocode(address = direccion,
                        custom_query = list("accept_language" = "es",
                                           "countrycodes" = "AR"), 
                        full_results = TRUE)
dict_cnrt_osm %>% 
  write_rds("/srv/DataDNMYE/capas_sig/dict_cnrt_osm.rds")

sum(is.na(dict_cnrt_osm$place_id[dict_cnrt_osm$pax >365]))/nrow(dict_cnrt_osm[dict_cnrt_osm$pax >365,])

# dict_cnrt_osm %>% 
#   filter(!is.na(lat)) %>% 
#   st_as_sf(coords  = c("long", "lat"), crs  = 4326) %>% 
#   mapview::mapview()
# 
# dict_cnrt_osm %>% 
#   filter(is.na(lat)) %>% view

# dict_cnrt_google <- cnrt %>%
#   filter(pax > 365) %>% 
#   mutate(direccion  = paste(loc_clean, prov, sep = ", ")) %>% 
#   tidygeocoder::geocode(address = direccion, method  = "google",
#                         custom_query = list("components" = "country:AR"), 
#                         full_results = TRUE)

# dict_cnrt_google %>% 
#   write_rds("/srv/DataDNMYE/capas_sig/dict_cnrt_google.rds")
# 
# dict_cnrt_google %>% 
#   # filter(! is.na(lat) & !  is.na(long)) %>%
#   # sf::st_as_sf(coords = c( "long", "lat"), crs = 4326) %>% 
#   mapview::mapview()
# 
# dict_cnrt_google <- dict_cnrt_google %>% 
#   filter(! is.na(lat) & !  is.na(long)) %>% 
#   sf::st_as_sf(coords = c( "long", "lat"), crs = 4326)

puna <- read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra_2022.gpkg")

puna_faja4 <- st_transform(puna %>%
                             filter(!st_is_empty(geom)), crs = 5346)

dict_cnrt_faja4 <- st_transform(dict_cnrt_osm %>% 
                                  filter(!is.na(lat)) %>% 
                                  st_as_sf(coords= c("long", "lat"),
                                           crs = 4326),
                                crs = 5346)

# promedio de distancias entre puntos puna
dm <- st_distance(puna_faja4, puna_faja4)

dm <- dm %>% units::drop_units()

j <- list()

for (i in 1:nrow(dm)) {
  j[i] <- min(dm[i,1:nrow(dm)][-i], na.rm = T)
}

j %>% unlist() %>% median()/1000 # mediana de distancias mínimas en km
sum(dm) / (nrow(dm) * (nrow(dm) - 1) / 2)/1000 # Calcular el promedio de distancias en km


puna_cnrt <- st_join(puna_faja4, dict_cnrt_faja4,
                     join  = st_is_within_distance, left = T, dist = 15000)

puna_cnrt <- left_join(puna_cnrt, dict_cnrt_faja4 %>% 
            select(direccion, geometry) %>% 
              as_tibble(.))

puna_cnrt <- puna_cnrt %>% 
  mutate(dist  = map2(geom, geometry, .f = ~ st_distance(.x,.y)))

# puna_cnrt %>% 
#   st_drop_geometry() %>% 
#   select(provincia, departamento_partido, localidad,
#          direccion, plazas, pax, dist) %>% 
#   group_by(provincia, departamento_partido, localidad,
#            plazas) %>% 
#   summarise(pax = sum(pax, na.rm = T)) %>% 
#   filter(pax == 0) %>% 
#   view()
# 
# puna_cnrt %>% 
#   st_drop_geometry() %>% 
#   select(provincia, departamento_partido, localidad,
#          direccion, plazas, pax) %>% 
#   group_by(provincia, departamento_partido, localidad,
#            plazas) %>% 
#   summarise(pax = sum(pax, na.rm = T)) %>% 
#   view()

puna_cnrt <- puna_cnrt %>% 
  st_drop_geometry() %>% 
  select(provincia, departamento_partido, localidad,
         direccion, plazas, pax) %>% 
  group_by(provincia, departamento_partido, localidad,
           plazas) %>% 
  summarise(pax_cnrt = sum(pax, na.rm = T)) %>% 
  ungroup()

puna_cnrt %>% 
  write_rds("salidas/puna_cnrt.rds")


