library(tidyverse)
library(herramientas)
library(sf)
library(arrow)
# install.packages('osrm')



puna <- herramientas::read_file_srv("/srv/DataDNMYE/capas_sig/puna_localidades_bahra.gpkg")

puna <- puna %>%
  filter(! st_is_empty(geom))

# aeropuertos <- herramientas::read_file_srv("/srv/DataDNMYE/capas_sig/aeropuertos_anac_total.gpkg")

source("R/tabla_aeropuertos.R")

aeropuertos <- aeropuertos %>%
  mutate(indice = 1:n())

mdistancias <- st_distance(puna, aeropuertos)


ranking <- function(x, p) {
  sort(as.numeric(x), method = "shell", index.return = T)$ix[1:p]

}

top <- apply(mdistancias, MARGIN = 1,FUN = function(x) ranking(x, p = 3))
top <- as_tibble(t(top), )
colnames(top) <- c(paste0('aeropuerto', 1:ncol(top)))

puna_aeropuertos <- bind_cols(puna, top)

puna_aeropuertos <- puna_aeropuertos %>%
  pivot_longer(cols = colnames(top), names_to = "orden_aeropuerto", values_to = "aeropuerto")

puna_aeropuertos <-  left_join(puna_aeropuertos, as_tibble(aeropuertos),
                   by = c("aeropuerto" = "indice"))




