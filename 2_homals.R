# Nombre Programa:
# Ubicacion: GitHub/ISMT
# Autor: Gabriela Ulloa Contador
# Fecha Creacion: 19/10/2018
# Proyecto: Proyecto 4 - Encuesta
# Objetivo: Correr homals con puntajes de sociomaterialidad con especificidad territorial somet
# Output: C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output
# Notas:

# install.packages(c("tidyverse", "glue", "homals"))

library(tidyverse)
library(glue)
library(homals)

#carpeta de trabajo
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output"
indic_final <- readRDS(glue("{dir_loc}/Censo2017_ISMT_hogares.Rds"))


indic_sel <- indic_final %>% select(ptje_esc, ptje_hacin, ptje_mater)

hom <- homals(indic_sel,  ndim = 1, rank = 1, level = "numerical", sets = 0, active = TRUE,
           eps = 1e-06, itermax = 1000, verbose = 0)


hom

plot("hom")
