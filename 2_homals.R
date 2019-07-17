# Nombre Programa:
# Ubicacion: GitHub/ISMT
# Autor: Gabriela Ulloa Contador
# Fecha Creacion: 19/10/2018
# Proyecto: Proyecto 4 - Encuesta
# Objetivo: Correr homals con puntajes de sociomaterialidad con especificidad territorial somet
# Output: C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output
# Notas:

# install.packages(c("tidyverse", "glue", "homals"))

library(dplyr)
library(glue)
library(homals)

# Setear directorio local
ismt_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT"

reg <- 5
yyyy <- 2017

# Leer datos
indic_sl <- readRDS((glue("{ismt_dir}/Output/Censo{yyyy}_Hogar_ISMT_R{reg}.Rds")))

# #Agregar puntaje allegamiento
indic_sl <- indic_sl %>% select(ptje_esc, ptje_hacin, ptje_mater, ptje_alleg)
  
#Remove NAs
indic_sl <- na.omit(indic_sl)

# Sample - para grandes bases de datos
indic_sl <- indic_sl %>% sample_n(1000000)

# Homogeneity analysis
hom <- homals(indic_sl,  ndim = 1, rank = 1, level = "numerical", sets = 0, active = TRUE,
                eps = 1e-06, itermax = 1000, verbose = 0)

# Plotear
hom

