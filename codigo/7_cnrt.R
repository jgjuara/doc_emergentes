library(tidyverse)
library(tidygeocoder)

cnrt <- read_file_srv("/srv/DataDNMYE/cnrt/backup/Pasajeros SR-2022.xlsx")

cnrt %>% 
  mutate(across(c(`Localidad Origen`, `Localidad Destino`), ~ tolower(.x))) %>% 
  distinct(`Localidad Origen`, `Localidad Destino`)  %>% 
  pivot_longer(cols = everything()) %>% distinct(value) %>% view
  
# usar diccionario 
cnrt <- cnrt %>% 
  janitor::clean_names()

cnrt <- cnrt %>% 
  mutate(across(c(localidad_origen, provincia_origen,
                  localidad_destino, provincia_destino),
                tolower)) %>%
  group_by(localidad_origen, provincia_origen, localidad_destino, provincia_destino) %>% 
  summarise(pax = sum(cantidad_de_pasajeros)) %>% ungroup() %>% 
  mutate(across(c(localidad_origen, provincia_origen,
                  localidad_destino, provincia_destino),
                ~ herramientas::limpiar_texto(.x, enie = F))) %>%
  group_by(localidad_origen, provincia_origen, localidad_destino, provincia_destino) %>% 
  summarise(pax = sum(pax)) %>% ungroup()



cnrt <- cnrt %>% 
  mutate(across(c(localidad_origen, localidad_destino), str_squish)) 

cnrt <- cnrt %>% 
  mutate(across(c(localidad_origen, localidad_destino),
                function(x) {
                  str_remove(x, "bs as|capital federal| sta sfe| sfe| lpa| cba| rgn| rn| mza| lp")
                  })
  )

cnrt <- cnrt %>% 
  mutate(across(.cols = contains(c("dest", "orig")), ~ str_to_lower(.)),
         across(.cols = contains(c("dest", "orig")), ~ str_squish(.)),
         across(.cols = contains(c("dest", "orig")), ~ str_trim(., side = "both"))) %>% 
  mutate(localidad_destino = case_when(localidad_destino == "alta gracia" ~ "alta gracia",
                                       localidad_destino == "junin"       ~ "junin",
                                       localidad_destino == "jovita"      ~ "jovita",
                                     localidad_destino == "mar de ajo"  ~ "mar de ajo",
                                     localidad_destino == "ciudadela"   ~ "liniers",
                                     localidad_destino == "pehuajo"     ~ "pehuajo",
                                     localidad_destino == "virasoro"    ~ "virasoro",
                                     str_detect(localidad_destino, "los toldos")    ~ "los toldos",
                                     localidad_destino == "telen"       ~ "telen",
                                     localidad_destino == "cuatia"       ~ "curuzu cuatia",
                                     str_detect(localidad_destino, "camboriu") ~ "camboriu",
                                     TRUE ~ localidad_destino) %>% 
           str_replace(., " \\s*\\([^\\)]+\\)", ""),
         localidad_origen = case_when(localidad_origen == "alta gracia" ~ "alta gracia",
                                     localidad_origen == "junin"       ~ "junin",
                                     localidad_origen == "jovita"      ~ "jovita",
                                     localidad_origen == "mar de ajo"  ~ "mar de ajo",
                                     localidad_origen == "ciudadela"   ~ "liniers",
                                     localidad_origen == "pehuajo"     ~ "pehuajo",
                                     localidad_origen == "virasoro"    ~ "virasoro",
                                     str_detect(localidad_origen, "los toldos")    ~ "los toldos",
                                     localidad_origen == "telen"       ~ "telen",
                                     localidad_origen == "cuatia"       ~ "curuzu cuatia",
                                     str_detect(localidad_origen, "camboriu") ~ "camboriu",
                                     TRUE ~ localidad_origen) %>% 
           str_replace(., " \\s*\\([^\\)]+\\)", ""))


cnrt <- cnrt %>% 
  mutate(across(.cols = c(localidad_destino,localidad_origen), 
                ~ case_when(
                  str_detect(., "terminal|term|tnal|agencia|parador") ~ str_trim(str_replace_all(string = ., 
                                                                                                 pattern = "terminal|term|agencia|parador", 
                                                                                                 replacement = ""),
                                                                                 side = "both"), 
                  TRUE ~ .)),
         provincia_destino = case_when(provincia_destino == "salto" ~ "salta",
                                       TRUE ~ provincia_destino))
         # across(.cols = c(loc_dest_limpia, loc_orig_limpia), 
         #        ~ remover_tildes(.)),

cnrt <- cnrt %>% 
  mutate(
         across(.cols =  c(localidad_destino,localidad_origen), 
                ~ case_when(str_detect(., "mar del plata|punta mogotes") ~ "mar del plata",
                            str_detect(., "	zbuenos aires km35|nos aires|capital federal") & . != "resistencia" & . != "macia" & . != "florencio varela" & . != "san francisco" & . != "presidencia roque saenz peña" ~ "ciudad autonoma de buenos aires",
                            str_detect(., "retiro|dellepiane|hotel savoy|pte saavedra|pte saavedra") ~ "ciudad autonoma de buenos aires",
                            str_detect(., "rosario")       ~ "rosario",
                            str_detect(., "zarate ciudad") ~ "zarate",
                            str_detect(., "jujuy")         ~ "san salvador de jujuy",
                            str_detect(., "la plata")      ~ "la plata",
                            str_detect(., "salta|j v gonzalez")         ~ "salta",
                            str_detect(., "asuncion")      ~ "asuncion",
                            str_detect(., "curuzucuatia")  ~ "curuzu cuatia",
                            str_detect(., "federal sjo")   ~ "federal",
                            str_detect(., "frias")         ~ "frias",
                            str_detect(., "ciudadela")     ~ "ciudadela",
                            str_detect(., "gonz. chaves")  ~ "gonzales chaves",
                            str_detect(., "posadas")       ~ "posadas",
                            str_detect(., "prince varela") ~ "berazategui",
                            str_detect(., "pergano")       ~ "pergamino",
                            str_detect(., "vergara y gaona") ~ "villa tesei",
                            str_detect(., "rio ceballos cba") ~ "rio ceballos",
                            str_detect(., "santo tom")     ~ "santo tome",
                            str_detect(., "moron nueva")   ~ "moron",
                            str_detect(., "saujil")        ~ "saujil",
                            str_detect(., "bde irigoyen")  ~ "bernardo de irigoyen",
                            str_detect(., "ldor. general  s. martin") ~ "libertador general san martin",
                            str_detect(., "del libertador|villa libertador") ~ "gualeguaychu",
                            str_detect(., "general  san martin lp") ~ "general san martin",
                            str_detect(., "moreno las piedras") ~ "las piedras",
                            str_detect(., "corrientes san jose") ~ "san jose",
                            str_detect(., "moreno dover") ~ "moreno",
                            str_detect(., "s. miguel de tucuman") ~ "san miguel de tucuman",
                            str_detect(., "san miguel rotonda bue") ~ "san miguel",
                            str_detect(., "sjusto centro ag serena|san justo rotonda bs as|m. san justo|. san justo|la parada de cdn") ~ "san justo",
                            str_detect(., "bariloche")     ~ "san carlos de bariloche",
                            str_detect(., "santiago del eso|sgo del eso|sgo del estero") ~ "santiago del estero",
                            str_detect(., "america rivadavia")    ~ "america",
                            str_detect(., "b. blanca")            ~ "bahia blanca",
                            str_detect(., "c del este")           ~ "ciudad del este",
                            str_detect(., "c. brochero|cura brochero cba.") ~ "villa cura brochero",
                            str_detect(., "cdro|c. riv|comodoro|com. rivadavia") ~ "comodoro rivadavia",
                            str_detect(., "^com. rivadav\\.\\(ch\\)$") ~ "comodoro rivadavia",
                            str_detect(., "^rivadav.(ch)$") ~ "comodoro rivadavia",
                            str_detect(., "vc paz|carlos paz")         ~ "villa carlos paz",
                            str_detect(., "morteros")       ~ "morteros",
                            str_detect(., "e//squel")       ~ "esquel",
                            str_detect(., "jose c. paz desconectur") ~ "jose c. paz",
                            str_detect(., "tuyu")           ~ "mar del tuyu",
                            str_detect(., "pampa de los huanacos|p.de los guanacos") ~ "pampa de los guanacos",
                            str_detect(., "avellaneda")     ~ "avellaneda",
                            str_detect(., "laboulaye cba")  ~ "laboulaye",
                            str_detect(., "lnalem")         ~ "leandro n. alem",
                            str_detect(., "r hondo")        ~ "rio hondo",
                            str_detect(., "na clavero")     ~ "mina clavero",
                            str_detect(., "bermejo")        ~ "bermejo",
                            str_detect(., "clorinda")       ~ "clorinda",
                            str_detect(., "ibicuy")         ~ "ibicuy",
                            str_detect(., "punta mogotes|pta. mogotes")  ~ "punta mogotes",
                            str_detect(., "quili")          ~ "quimili",
                            str_detect(., "quilmes cal/12 oct") ~ "quilmes",
                            str_detect(., "burzaco")        ~ "burzaco",
                            str_detect(., "abra ventana")   ~ "valle abra de la ventana",
                            str_detect(., "cap del monte|cap. del monte|cap monte|cap del monte|cap.del monte|cap. monte")  ~ "capilla del monte",
                            str_detect(., "ros de la frona")~ "rosario de la frontera",
                            str_detect(., "chapadmalal")    ~ "chapadmalal",
                            str_detect(., "claromeco")      ~ "claromeco",
                            str_detect(., "cap sarmiento")  ~ "capitan sarmiento",
                            str_detect(., "colivia")        ~ "caleta olivia",
                            str_detect(., "colon")          ~ "colon",
                            str_detect(., "moron")          ~ "moron",
                            str_detect(., "tnal. merlo")    ~ "merlo",
                            str_detect(., "me ndoza|m//e ndoza")       ~ "mendoza",
                            str_detect(., "villa elisa")    ~ "villa elisa",
                            str_detect(., "bell ville tnal.") ~ "bell ville",
                            str_detect(., "villa traful")   ~ "villa traful",
                            str_detect(., "sta. esita")     ~ "santa teresita",
                            str_detect(., "snandes")        ~ "san martin de los andes",
                            str_detect(., "s.m.del monte")     ~ "san miguel del monte",
                            str_detect(., "sta.clara del mar") ~ "santa clara del mar",
                            str_detect(., "virasoro") ~ "virasoro",
                            str_detect(., "santa rosa") ~ "santa rosa",
                            str_detect(., "r.s. |r.s.|rs |saenz pena") & . != "santa rosa" & . != "virasoro" & . != "tres isletas" & . != "resistencia" & . != "crespo" & . != "tres arroyos" ~ "roque saenz peña",
                            str_detect(., "sta cruz de la sierra") ~ "santa cruz de la cierra",
                            str_detect(., "san guel|s.m. de tucuman|s.m. tucuman|smtucuman") ~ "san miguel de tucuman",
                            str_detect(., "s mazza|salvador mazza")         ~ "salvador mazza",
                            str_detect(., "sauce")         ~ "sauce viejo",
                            str_detect(., "gral.pico")         ~ "general pico",
                            str_detect(., "pozo h")         ~ "pozo hondo",
                            str_detect(., "san ignacio arg")         ~ "san ignacio",
                            str_detect(., "san pedro(mnes)|san pedro (mi)")         ~ "san pedro",
                            str_detect(., "^san pedro\\(mnes\\)$")         ~ "san pedro",
                            str_detect(., "san fdo del valle de catamarca|sfv catamarca|sfv. catamarca") ~ "san fernando del valle de catamarca",
                            str_detect(., "p truncado")                   ~ "pico truncado",
                            str_detect(., "Valle fértil|valle fertil")                 ~ "villa san agustin",
                            str_detect(., "p san nicolas parador|t san nicolas") ~ "san nicolas de los arroyos",
                            str_detect(., "cordoba capital|cbapta|tecordoba|cordoba cdad.")       ~ "cordoba",
                            str_detect(., "cruce de lomas")               ~ "lomas de zamora",
                            str_detect(., "varela") ~ "florencio varela", # cruce de varela|cruce varela|varela 1|varela 4|varela 8|varela vosa|f. varela|varela teminal|
                            str_detect(., "parana1")                      ~ "parana",
                            str_detect(., "trenque lauqen")               ~ "trenque lauquen",
                            str_detect(., "montecaseros")                 ~ "monte caseros",
                            str_detect(., "obera centro")                 ~ "obera",
                            str_detect(., "castelar")                 ~ "castelar",
                            str_detect(., "arapey uru")             ~ "arapey",
                            str_detect(., "resistencia")            ~ "resistencia",
                            str_detect(., "general viamonte")             ~ "los toldos",
                            str_detect(., "jj castelli|	j j castelli|j j castelli")    ~ "juan jose castelli",
                            str_detect(., "j.b.alberdi|j.b. alberdi|j. b. alberdi|j b alberdi")     ~ "juan bautista alberdi",
                            str_detect(., "pto.montt ch") ~ "puerto montt",
                            str_detect(., "pte ignacio loyola") ~ "clorinda",
                            str_detect(., "noria|pt. nra.|pte de la nor") ~ "ingeniero budge",
                            str_detect(., "rafaela nex|rafaela acc") ~ "rafaela",
                            #str_detect(., "santa esita") ~ "santa evita",
                            str_detect(., "v// maria|v maria") ~ "villa maria",
                            str_detect(., "pto.iguazu|pto iguazu|ptoiguaz|iguazu") ~ "puerto iguazu",
                            str_detect(., "m ")      ~ str_replace(., "m ", ""),
                            #str_detect(., " mis| mi") ~ str_replace(., " mis|mi", ""),
                            str_detect(., "gral. |gral ") ~ str_replace(., "gral.|gral", "general "),
                            str_detect(., "cnel. |cnel ") ~ str_replace(., "cnel.|cnel", "coronel "),
                            str_detect(., "ing. |ing ") ~ str_replace(., "ing. |ing ", "ingeniero "),
                            TRUE ~ .)),
         provincia_destino = case_when(provincia_destino == "ciudad autonoma de buenos aires" & localidad_destino == "ingeniero budge" ~ "buenos aires",
                                       TRUE ~ provincia_destino),
         provincia_origen  = case_when(provincia_origen  == "ciudad autonoma de buenos aires" & localidad_origen == "ingeniero budge" ~ "buenos aires",
                                       TRUE ~ provincia_origen),
         terminal_origen = localidad_origen,
         terminal_destino = localidad_destino,
         #across(.cols = contains(c("dest", "orig")), ~ str_to_title(.)),
         #par_origen_destino_ordenado = paste0(loc_dest_limpia, " - ", loc_orig_limpia),
         # par_origen_destino_aordenado = case_when(loc_orig_limpia < loc_dest_limpia ~ paste0(loc_orig_limpia,"-", loc_dest_limpia),
         #                                          TRUE             ~ paste0(loc_dest_limpia,"-",loc_orig_limpia)),
         # fecha_actualizacion = case_when(fecha_actualizacion == "\\N" ~ NA_character_,
         #                                 TRUE ~ fecha_actualizacion),
         # fecha_actualizacion = as.POSIXct(excel_numeric_to_date(as.numeric(fecha_actualizacion))),
         across(.cols = c(localidad_destino, localidad_origen),
                ~ case_when(str_detect(., "santiago") ~ "santiago del estero",
                            str_detect(., "tucuman") ~ "san miguel de tucuman",
                            str_detect(., "liniers") ~ "ciudad autonoma de buenos aires",
                            T ~ .)))

cnrt <- cnrt %>% 
  mutate(localidad_origen = str_remove(localidad_origen, "acc|acceso|pmadryn"),
         localidad_destino = str_remove(localidad_destino, "acc|acceso|pmadryn"))


cnrt <- cnrt %>% 
  filter(localidad_destino != "n" &
           localidad_origen != "n" &
           ! provincia_origen %in% c("alto parana", "asuncion", "caaguazu",
                                        "central", "cordillera", "guaira",
                                        "itapua", "n", "paraguari",
                                        "rio grande do sul", "santa catarina") &
          ! provincia_destino %in% c("alto parana", "asuncion", "caaguazu",
                                   "central", "cordillera", "guaira",
                                   "itapua", "n", "paraguari",
                                   "rio grande do sul", "santa catarina"))



cnrt <- cnrt %>%  
  group_by(localidad_origen, provincia_origen, localidad_destino, provincia_destino) %>% 
  summarise(pax = sum(pax)) %>% 
  ungroup()

dict_cnrt <- bind_rows(
  cnrt %>% 
  distinct(provincia_destino, localidad_destino) %>% 
    rename(prov = provincia_destino, localidad  = localidad_destino),
  cnrt %>% 
    distinct(provincia_origen, localidad_origen) %>% 
    rename(prov = provincia_origen, localidad  = localidad_origen)
  )



dict_cnrt %>% pull(prov) %>% unique()

dict_cnrt_osm <- dict_cnrt %>%
  mutate(direccion  = paste(localidad, prov, sep = ", ")) %>% 
  tidygeocoder::geocode(address = direccion,
                        custom_query = list("accept_language" = "es",
                                           "countrycodes" = "AR"), 
                        full_results = TRUE)

test2 <- dict_cnrt %>%
  mutate(direccion  = paste(localidad, prov, sep = ", ")) %>% 
  tidygeocoder::geocode(address = direccion, method  = "google",
                        custom_query = list("components" = "country:AR"), 
                        full_results = TRUE)
