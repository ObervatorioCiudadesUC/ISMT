# Nombre Programa: 0_HomologacionDatosCenso2012
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
yyyy <- 2012
# regiones <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) # cambiar las regiones
reg <- c(13) # cambiar la región 

# descomentar para aumentar limite memoria usando pendrive
memory.limit(12000)


# Iterar por cada region ----------------------------------------------------

for (reg in regiones) {
  
  # Leer una región determinada 
  data <- readRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Full.Rds")) %>% 
    filter(region==reg) %>% # filtrar region  
    mutate(year = yyyy)  # Agregar variable año
  
  
  # Estandarización 2012 ----------------------------------------------------
  
  nhogares2012 <- data %>% 
    filter(dpar == 1) %>% # Filtrar por jefe de hogar - 1 obs por hogar
    group_by(folio, nviv) %>% 
    summarise(
      cant_hog = as.integer(n()) # Cuenta del número de hogares encuestados
    ) %>% 
    ungroup() 
  
  # Unificar nombre variables - 2012
  data <- data %>%
    # Unir hogares por vivienda
    left_join(nhogares2012, by = c("folio", "nviv")) %>% 
    transmute(
      year = as.integer(year),
      region = as.integer(region),
      provincia = as.integer(prov),
      comuna = as.integer(comuna),
      distrito = as.integer(dc),
      zona = as.integer(zona),
      manzana = as.integer(manzana),
      geocode = as.numeric(geocode),
      manzent = as.numeric(manzent),
      # Variables llave
      folio = folio,
      nviv = nviv,
      nhogar = nhog,
      personan = nper,
      # Variables vivienda
      tipo_viv = v02,
      cond_ocup_viv = v01,
      tenencia = h11,
      cond_muro = case_when(
        v03a %in% c(1, 2, 3) ~ 3L, # Aceptable - Hormigón, armado; albañilería, tabique forrado por ambas caras
        v03a %in% c(4, 5) ~ 2L, # Recuperable  - Tabique sin forro interior; Adobe o quincha
        v03a %in% c(6) ~ 1L, # Irrecuperable - Materiales precarios o de desechos.
        TRUE ~ NA_integer_
      ),
      cond_cubierta = case_when(
        v03b %in% c(1, 2, 3) ~ 3L, # Aceptable - Tejas o tejuela, fibrocemento, losa hormigón, planchas zinc
        v03b %in% c(4, 5) ~ 2L, # Recuperable - Fonolita; paja, coirón, totora o caña     
        v03b %in% c(6, 7) ~ 1L, # Irrecuperable - Materiales precarios o de desecho; sin cubierta en el techo
        TRUE ~ NA_integer_
      ),
      cond_suelo = case_when(
        v03c %in% c(1, 2, 3) ~ 3L, # Aceptable - Parquet, madera, piso flotante o similar; cerámico, flexit; alfombra o cubre piso
        v03c %in% c(4, 5, 6) ~ 2L, # Recuperable - Baldosa de cemento, radier, capa de cemento
        v03c %in% c(7) ~ 1L, # Irrecuperable - Piso de tierra
        TRUE ~ NA_integer_
      ),
      n_dormitorios = if_else(v04 < 98, v04, NA_integer_),
      cant_hog = if_else(cant_hog < 98 , cant_hog, NA_integer_),  # v10m2 Muchos NA, reemplazar por summarise by folio y nviv
      cant_per = if_else(v09m < 98 & v09m != 0, v09m, NA_integer_),
      # variables persona
      parentesco = dpar,
      sexo = p19,
      edad = p20c,
      asiste_educ = p31, 
      curso = p30, 
      nivel_educ = p28, 
      sit_laboral = p36,
      cesante = if_else(sit_laboral %in% c(5), 1L, 0L), # Buscando empleo
      no_trabaja = if_else(sit_laboral %in% c(6, 7, 8), 1L, 0L), # No trabaja ni busca empleo
      hijos_nac = p40, 
      hijos_vivos = p41, 
      escolaridad = case_when(
        nivel_educ == 1 ~ 0L, # No asistió
        nivel_educ == 2 ~ 0L, # Sala cuna
        nivel_educ == 3 ~ 0L, # Kinder
        nivel_educ == 4 ~ 0L, # Educación diferencial
        nivel_educ == 5 ~ curso, # Educación básica, primaria o preparatoria
        nivel_educ == 6 ~ curso + 8L, # Educación Media Científico Humanista o Humanidades
        nivel_educ == 7 ~ curso + 8L, # Media Técnico Profesional
        nivel_educ == 8 ~ curso + 12L, # Técnico de Nivel Superior
        nivel_educ == 9 ~ curso + 12L, # Profesional
        nivel_educ == 10 ~ curso + 12L, # Postítulo
        nivel_educ == 11 ~ curso + 17L, # Magíster
        nivel_educ == 12 & curso <= 2 ~ curso + 19L, # Doctorado
        nivel_educ == 12 & curso > 2 ~ 21L, # Doctorado - máximo 21 años
        TRUE ~ NA_integer_
      )
    ) %>% 
    # filtrar tipo hogar y ocupacion de la vivienda
    filter(tipo_viv < 12 & cond_ocup_viv==1) 
  
  # Guardar datos totales --------------------------------------------------
  
  data %>%  saveRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Clean_R{reg}.Rds"))

}