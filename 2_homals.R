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

#carpeta de trabajo
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output"
# indic_sl <- readRDS(glue("{dir_loc}/Censo2017_ISMT_hogares_v2.Rds"))
indic_sl <- readRDS(glue("{dir_loc}/Censo2017_ISMT_hogares_v3.Rds"))

#Agregar puntaje allegamiento
indic_sl <- indic_sl %>% select(ptje_esc, ptje_hacin, ptje_mater, ptje_alleg)
#Remove NAs
indic_sl <- na.omit(indic_sl)

indic_sl <- indic_sl %>% sample_n(1000000)

hom <- homals(indic_sl,  ndim = 1, rank = 1, level = "numerical", sets = 0, active = TRUE,
           eps = 1e-06, itermax = 1000, verbose = 0)

#Revisar limite de uso de memoria R
# memory.limit()

# Ver hom
# Sample 1mill valores
hom
# Eigenvalues:
#   D1 
# 0.067 
# 
# Variable Loadings:
#   D1
# ptje_esc   -0.3999909
# ptje_hacin -0.2686852
# ptje_mater  0.4118807

# Analisis de componentes principales

pca <- prcomp(indic_sl)
pca
summary(pca)

# ------------PC1        PC2        PC3
# ptje_esc   -0.7271375  0.5965606  0.3396859
# ptje_hacin -0.5951240 -0.7944328  0.1212602
# ptje_mater -0.3421967  0.1139823 -0.9326894

# -----------------------PC1      PC2      PC3
# Standard deviation     170.774 150.1786 125.6744
# Proportion of Variance   0.432   0.3341   0.2339
# Cumulative Proportion    0.432   0.7661   1.0000

# incluyen allegamiento - sample 500mil 

# Eigenvalues:
#   D1 
# 0.0412 
# 
# Variable Loadings:
#   D1
# ptje_esc   -0.2842796
# ptje_hacin -0.3217238
# ptje_mater  0.2930686
# ptje_alleg  0.2429153

# incluyen allegamiento - sample 1 millon

# Eigenvalues:
#   D1
# 0.0413
# 
# Variable Loadings:
#   D1
# ptje_esc   -0.2783314
# ptje_hacin -0.3271089
# ptje_mater  0.2867274
# ptje_alleg  0.2522210
