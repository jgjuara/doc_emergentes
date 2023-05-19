library(herramientas)

padron_afip <- read_file_srv('/srv/DataDNMYE/padron_afip/ubicacion_claes_turismo_empleo_dic19.csv')


bahra <- geoAr::get_bahra()


padron_afip <- padron_afip %>% 
  filter(! (is.na(departamento_arcgis) & is.na(provincia) )) %>% 
  mutate(across(c(provincia, departamento_arcgis, localidad_arcgis), 
                .fns = \(x) str_squish(textclean::replace_non_ascii(tolower(x))))) %>% 
  mutate(provincia = case_when(provincia == 'caba' ~  'ciudad de buenos aires', 
                               T ~ provincia))
padron_afip <- padron_afip %>% 
  mutate(cat_rct = case_when(substr(clae6,1,3) %in% c(473,491,492,501,502,511,524,771)~ "Transporte",
                           substr(clae6,1,3) %in% c(551,552)~ "Alojamiento",
                           substr(clae6,1,3) %in% c(561,562) ~ "Gastronomía",
                           substr(clae6,1,3) == 791 ~ "Agencias de Viaje",
                           substr(clae6,1,3) %in% c(591,592,681,780,854,900,910,920,931,939)~ "Otros Servicios Turísticos"))

padron_afip <- padron_afip %>% 
  filter(empleadora_dic19 == 1) %>% 
  group_by(localidad_arcgis, departamento_arcgis, provincia, cat_rct) %>% 
  summarise(prestadores = n()) 

padron_afip <- padron_afip %>% 
  pivot_wider(names_from = cat_rct, values_from = prestadores)

bahra <-  bahra %>% 
  mutate(across(c(nom_pcia, nom_depto, nombre), 
                .fns = \(x) str_squish(textclean::replace_non_ascii(tolower(x))))) %>% 
  mutate(nom_depto = case_when(codprov_censo == '02' & coddepto_censo == '012' ~ 'comuna 12', 
                               T ~ nom_depto))

padron_afip_bahra <- left_join(padron_afip, bahra, by = c("provincia" = "nom_pcia",
                                          "departamento_arcgis" = "nom_depto",
                                          "localidad_arcgis" = "nombre"))


# padron_afip_bahra %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   view
# 
# padron_afip_bahra %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   filter(is.na(codprov_censo) & localidad_arcgis !=  'no_pertenece') %>%
#   group_by(provincia, departamento_arcgis, localidad_arcgis) %>%
#   summarise(n = n()) %>% view
# 
# padron_afip_bahra %>%
#   ungroup() %>%
#   distinct(pick(1:6), .keep_all = T) %>%
#   filter(is.na(codprov_censo) & localidad_arcgis !=  'no_pertenece') %>%
#   summarise(sum(prestadores))

padron_afip_bahra <- padron_afip_bahra %>% 
  filter(!is.na(codprov_censo))

class(padron_afip_bahra)

padron_afip_bahra %>% 
  st_as_sf(crs = 4326) %>% 
  mapview::mapview()

puna_aero_afip <- left_join(puna_aeropuertos, padron_afip_bahra, by = c("cod_pcia" = "codprov_censo"))
