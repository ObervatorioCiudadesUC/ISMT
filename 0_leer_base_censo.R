# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue", "haven"))
library(tidyverse)
library(glue)
library(haven)

# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/02_Insumos/Censo 2017 Personas + Viviendas RM"

#Leer base
# base_rm <- read_spss(glue("{dir_loc}/Censo 2017 Personas + Viviendas RM.sav"))
base_rm <- read.csv2(glue("{dir_loc}/Censo 2017 Personas + Viviendas RM.csv"))
base_rm <- base_rm %>% mutate(region = ï..REGION) %>% 
  select(-ï..REGION, -filter_.) %>% rename_all(tolower) 

# Guardarla como .Rds - probar 
base_rm %>% 
  saveRDS(glue("{dir_loc}/Censo2017_Personas_ZC_RM.Rds"))
test <- readRDS(glue("{dir_loc}/Censo2017_Personas_ZC_RM.Rds"))

head(base_rm)
summary(base_rm)
