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
yyyy <- 1992
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
  
  
  # Estandarización 1992 ----------------------------------------------------
  
  
  # Clacular candidad de hogares por vivienda
  npers1992 <- data %>% 
    filter(parentesco == 1) %>% 
    group_by(portafolio, vivienda) %>% 
    summarise(
      # cant_hog = max(nhog),
      cant_per = sum(tp),
      n_dormitorios = sum(pieza_dormir) # suma dormitorios por hogar
    ) %>% 
    ungroup() 
  
  # Unificar nombre variables - 1992
  data <- data %>% 
    # Unir hogares por vivienda
    left_join(npers1992, by = c("portafolio", "vivienda")) %>% 
    # Homogeneizar variables
    transmute(
      year = as.integer(year),
      region = as.integer(region),
      provincia = as.integer(provincia),
      comuna = as.integer(comuna),
      distrito = as.integer(dc),
      zona = as.integer(zona),
      manzana = as.integer(manzana),
      geocode = as.numeric(geocode92),
      manzent = as.numeric(manzent92),
      # Variables llave
      folio = portafolio,
      nviv = vivienda,
      nhogar = hogar,
      personan = persona,
      # Variables vivienda
      tipo_viv = tipo_vivienda,
      cond_ocup_viv = cond_ocupacion,
      tenencia = cond_tenencia,
      cond_muro = case_when(
        paredes %in% c(1, 2) ~ 3L, # Aceptable - Hormigón, armado; albañilería, tabique forrado por ambas caras
        paredes %in% c(3, 4) ~ 2L, # Recuperable  - Tabique sin forro interior; Adobe o quincha
        paredes %in% c(5, 6) ~ 1L, # Irrecuperable - Materiales precarios o de desechos.
        TRUE ~ NA_integer_
      ),
      cond_cubierta = case_when(
        techo %in% c(1, 2, 3, 4, 5) ~ 3L, # Aceptable - Tejas o tejuela, fibrocemento, losa hormigón, planchas zinc
        techo %in% c(6, 7) ~ 2L, # Recuperable - Fonolita; paja, coirón, totora o caña     
        techo %in% c(8) ~ 1L, # Irrecuperable - Materiales precarios o de desecho; sin cubierta en el techo
        TRUE ~ NA_integer_
      ),
      cond_suelo = case_when(
        piso %in% c(1, 2, 3) ~ 3L, # Aceptable - Parquet, madera, piso flotante o similar; cerámico, flexit; alfombra o cubre piso
        piso %in% c(4, 5, 6) ~ 2L, # Recuperable - Baldosa de cemento, radier, capa de cemento
        piso %in% c(7, 8) ~ 1L, # Irrecuperable - Piso de tierra
        TRUE ~ NA_integer_
      ),
      n_dormitorios = if_else(n_dormitorios < 98, n_dormitorios, NA_integer_), # Variable sumada a nivel de vivienda
      cant_hog = if_else(hogares < 98, hogares, NA_integer_),
      cant_per = if_else(cant_per < 98, cant_per, NA_integer_), # Variable sumada a nivel de vivienda
      # variables persona
      parentesco = parentesco,
      sexo = sexo,
      edad = edad,
      asiste_educ = if_else(situacion_empleo == 7, 1L, 0L),
      curso = curso,
      nivel_educ = tipo_educacion,
      sit_laboral = situacion_empleo,
      cesante = if_else(sit_laboral %in% c(4, 5), 1L, 0L), # Buscando empleo
      no_trabaja = if_else(sit_laboral %in% c(6, 8, 9, 10), 1L, 0L), # No trabaja ni busca empleo
      hijos_nac = hijos_nacidos_vivos,
      hijos_vivos = hijos_vivos,
      escolaridad = case_when(
        nivel_educ == 0 ~ 0L, # No asistió
        nivel_educ == 1 ~ 0L, # Kinder
        nivel_educ == 2 ~ curso, # Educación básica, primaria o preparatoria
        nivel_educ == 3 ~ curso + 8L, # Educación Media Científico Humanista
        nivel_educ == 4 ~ curso + 8L, # Humanidades
        nivel_educ == 5 ~ curso + 8L, # Media Técnico Profesional comercial
        nivel_educ == 6 ~ curso + 8L, # Media Técnico Profesional industrial
        nivel_educ == 7 ~ curso + 8L, # Media Técnico Profesional agricola
        nivel_educ == 8 ~ curso + 8L, # Media Técnico Profesional maritima
        nivel_educ == 9 ~ curso + 8L, # minera
        nivel_educ == 10 ~ curso + 8L, # Media Técnico Profesional femenina
        nivel_educ == 11 ~ curso + 8L, # Normal
        nivel_educ == 12 ~ curso + 12L, # Centro formación técnica
        nivel_educ == 13 ~ curso + 12L, # Instituto profesional
        nivel_educ == 14 ~ curso + 12L, # Profesional o superior
        TRUE ~ NA_integer_
      )
    )%>% 
    # filtrar tipo hogar y ocupacion de la vivienda 1992
    filter(tipo_viv < 8 & cond_ocup_viv==1) 
  
  
  # Guardar datos totales --------------------------------------------------
  
  data %>%  saveRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Clean_R{reg}.Rds"))

}