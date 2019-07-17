# Nombre Programa: 0_HomologacionDatosCenso2017
# Ubicacion: GitHub/ISMT
# Autor: Monica Flores
# Fecha Creacion: 15/01/2018
# Proyecto: ISMT
# Objetivo: Homogeneizar base de datos Censo 2017
# Output: 
# Notas:

# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue"))

library(tidyverse)
library(glue)


# Definiciones iniciales  ---------------------------------------------------

# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
censo_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/14_Microdatos/Clean_Personas"

# Definir año censal y regiones
yyyy <- 2017
regiones <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) # cambiar las regiones

# descomentar para aumentar limite memoria usando pendrive
memory.limit(12000)


# Iterar por cada region ----------------------------------------------------

for (reg in regiones) {
  
  # Leer una región determinada 
  data <- readRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Full.Rds")) %>% 
    filter(region==reg) %>% # filtrar region  
    mutate(year = yyyy)  # Agregar variable año
  
  
  # Estandarización 2017 ----------------------------------------------------
  
  data <- data %>% 
    # Unificar nombre variables - 2017
    transmute(
      year = as.integer(year),
      region = as.integer(region_15r),
      provincia = as.integer(provincia_15r),
      comuna = as.integer(comuna_15r),
      distrito = as.integer(dc),
      zona = as.integer(zc_loc),
      manzana = NA_integer_,
      geocode = as.numeric(geocode),
      manzent = NA_integer_,
      # Variables llave
      folio = NA_integer_,
      nviv = nviv,
      nhogar = nhogar,
      personan = personan,
      # Variables vivienda
      tipo_viv = p01,
      cond_ocup_viv = p02,
      tenencia = NA_integer_,
      cond_muro = case_when(
        p03a %in% c(1, 2, 3) ~ 3L, # Aceptable - Hormigón, armado; albañilería, tabique forrado por ambas caras
        p03a %in% c(4, 5) ~ 2L, # Recuperable  - Tabique sin forro interior; Adobe o quincha
        p03a %in% c(6) ~ 1L, # Irrecuperable - Materiales precarios o de desechos.
        TRUE ~ NA_integer_
      ),
      cond_cubierta = case_when(
        p03b %in% c(1, 2, 3) ~ 3L, # Aceptable - Tejas o tejuela, fibrocemento, losa hormigón, planchas zinc
        p03b %in% c(4, 5) ~ 2L, # Recuperable - Fonolita; paja, coirón, totora o caña     
        p03b %in% c(6, 7) ~ 1L, # Irrecuperable - Materiales precarios o de desecho; sin cubierta en el techo
        TRUE ~ NA_integer_
      ),
      cond_suelo = case_when(
        p03c %in% c(1) ~ 3L, # Aceptable - Parquet, madera, piso flotante o similar; cerámico, flexit; alfombra o cubre piso
        p03c %in% c(2, 3, 4) ~ 2L, # Recuperable - Baldosa de cemento, radier, capa de cemento
        p03c %in% c(5) ~ 1L, # Irrecuperable - Piso de tierra
        TRUE ~ NA_integer_
      ),
      n_dormitorios = if_else(p04 < 98, p04, NA_integer_),
      cant_hog = if_else(cant_hog < 98, cant_hog, NA_integer_),
      cant_per = cant_per,
      # variables persona
      parentesco = p07,
      sexo = p08,
      edad = p09,
      asiste_educ = if_else(p13 < 98, p13, NA_integer_),
      curso = if_else(p14 < 98, p14, NA_integer_),
      nivel_educ = if_else(p15 < 98, p15, NA_integer_),
      sit_laboral = if_else(p17 < 98, p17, NA_integer_),
      cesante = if_else(sit_laboral %in% c(4), 1L, 0L), # Buscando empleo
      no_trabaja = if_else(sit_laboral %in% c(6, 7, 8), 1L, 0L), # No trabaja ni busca empleo
      hijos_nac = if_else(p19 < 98, p19, NA_integer_),
      hijos_vivos = if_else(p20 < 98, p20, NA_integer_),
      escolaridad = if_else(escolaridad < 27, escolaridad, NA_integer_)
    ) %>% 
    # filtrar tipo hogar y ocupacion de la vivienda
    filter(tipo_viv < 8 & cond_ocup_viv==1) 
  
  
  # Guardar datos totales --------------------------------------------------
  
  data %>%  saveRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Clean_R{reg}.Rds"))

}