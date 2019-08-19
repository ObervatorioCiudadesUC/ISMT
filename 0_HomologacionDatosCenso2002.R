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
yyyy <- 2002
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
  
  
  # Estandarización 2002 ----------------------------------------------------
  
  
  # Clacular candidad de hogares por vivienda
  nhogares2002 <- data %>% 
    filter(p17 == 1) %>% # Seleccionar solo jefe de hogar # y filtrar por vivienda no colectiva  & v1 < 9 & v2==1
    group_by(portafolio, vn) %>% 
    summarise(
      # cant_hog = max(nhog),
      cant_per = sum(tp, na.rm = TRUE),
      n_dormitorios = sum(h13, na.rm = TRUE) # suma dormitorios por hogar
      # n_piezas = max(v10a)
    ) %>% 
    ungroup()
  
  # Unificar nombre variables - 2002
  data  <- data %>%
    # Unir hogares por vivienda
    left_join(nhogares2002, by = c("portafolio", "vn")) %>% 
    transmute(
      year = as.integer(year),
      region = as.integer(region),
      provincia = as.integer(provincia),
      comuna = as.integer(comuna),
      distrito = as.integer(distrito),
      zona = as.integer(zona),
      manzana = as.integer(manzana),
      geocode = as.numeric(geocode),
      manzent = as.numeric(manzent),
      # Variables llave
      folio = portafolio,
      nviv = vn,
      nhogar = hn,
      personan = pn,
      # Variables vivienda
      tipo_viv = v1,
      cond_ocup_viv = v2,
      tenencia = v3,
      cond_muro = case_when(
        v4a %in% c(1, 2, 3, 4) ~ 3L, # Aceptable - Hormigón, armado; albañilería, tabique forrado por ambas caras
        v4a %in% c(5, 6) ~ 2L, # Recuperable  - Tabique sin forro interior; Adobe o quincha
        v4a %in% c(7) ~ 1L, # Irrecuperable - Materiales precarios o de desechos.
        TRUE ~ NA_integer_
      ),
      cond_cubierta = case_when(
        v4b %in% c(1, 2, 3, 4, 5, 6) ~ 3L, # Aceptable - Tejas o tejuela, fibrocemento, losa hormigón, planchas zinc
        v4b %in% c(7, 8) ~ 2L, # Recuperable - Fonolita; paja, coirón, totora o caña     
        v4b %in% c(9) ~ 1L, # Irrecuperable - Materiales precarios o de desecho; sin cubierta en el techo
        TRUE ~ NA_integer_
      ),
      cond_suelo = case_when(
        v4c %in% c(1, 2, 3, 4) ~ 3L, # Aceptable - Parquet, madera, piso flotante o similar; cerámico, flexit; alfombra o cubre piso
        v4c %in% c(5, 6, 7, 8) ~ 2L, # Recuperable - Baldosa de cemento, radier, capa de cemento
        v4c %in% c(9) ~ 1L, # Irrecuperable - Piso de tierra
        TRUE ~ NA_integer_
      ),
      n_dormitorios = if_else(n_dormitorios < 98, n_dormitorios, NA_integer_),
      cant_hog = if_else(v11 < 98, v11, NA_integer_),
      cant_per = if_else(cant_per < 98, cant_per, NA_integer_),
      # variables persona
      parentesco = p17,
      sexo = p18,
      edad = p19,
      asiste_educ = if_else(p29 == 7, 1L, 0L), 
      curso = p26b, 
      nivel_educ = p26a, 
      sit_laboral = p29, 
      cesante = if_else(sit_laboral %in% c(3, 5), 1L, 0L), # Buscando empleo
      no_trabaja = if_else(sit_laboral %in% c(6, 8, 9, 10), 1L, 0L), # No trabaja ni busca empleo
      hijos_nac = p34, 
      hijos_vivos = p35, 
      escolaridad = case_when(
        nivel_educ == 1 ~ 0L, # No asistió
        nivel_educ == 2 ~ 0L, # Sala cuna
        nivel_educ == 3 ~ 0L, # Educación diferencial
        nivel_educ == 4 ~ curso, # Educación básica, primaria o preparatoria
        nivel_educ == 5 ~ curso + 8L, # Educación Media Científico Humanista
        nivel_educ == 6 ~ curso + 8L, # Humanidades
        nivel_educ == 7 ~ curso + 8L, # Media Técnico Profesional comercial
        nivel_educ == 8 ~ curso + 8L, # Media Técnico Profesional industrial
        nivel_educ == 9 ~ curso + 8L, # Media Técnico Profesional agricola
        nivel_educ == 10 ~ curso + 8L, # Media Técnico Profesional maritima
        nivel_educ == 11 ~ curso + 8L, # Normal
        nivel_educ == 12 ~ curso + 8L, # Media Técnico Profesional femenina
        nivel_educ == 13 ~ curso + 12L, # Centro formación técnica
        nivel_educ == 14 ~ curso + 12L, # Instituto profesional
        nivel_educ == 15 ~ curso + 12L, # Profesional o superior
        TRUE ~ NA_integer_
      )
    ) %>% 
    # filtrar tipo hogar y ocupacion de la vivienda
    filter(tipo_viv < 9 & cond_ocup_viv==1) 
  
  
  
  # Guardar datos totales --------------------------------------------------
  
  data %>%  saveRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Clean_R{reg}.Rds"))

}