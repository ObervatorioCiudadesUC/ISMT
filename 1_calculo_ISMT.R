# Nombre Programa: 1_calculo_ISMT
# Ubicacion: GitHub/ISMT
# Autor: Monica Flores
# Fecha Creacion: 18/12/2018
# Proyecto: ISMT
# Objetivo: Calcular Indice Sociomaterial 
# Output: C:/Users/Xlab 3/iCloudDrive/Desktop/OCUC _3/Gabriela Ulloa/R/gabyR/Output
# Notas:

# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)

# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
in_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/02_Insumos/Censo 2017 Personas + Viviendas RM"
out_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output"


base_rm <- readRDS(glue("{in_dir}/Censo2017_Personas_ZC_RM.Rds"))

head(base_rm)
summary(base_rm)

# Probar codigo conun subset
# subset_rm <- base_rm %>% filter(id_zona_loc==1279)

# seleccionar variables y definir tramos edades
base_rm_hog <- base_rm %>%  select(provincia:cant_per, region) %>% 
  filter(personan == 1) #%>% 
  # mutate(
  #   emayor25 = if_else(p09 >= 25, 1L, 0L),
  #   emayor18 = if_else(p09 >= 18, 1L, 0L),
  #   emayor65 = if_else(p09 >= 65, 1L, 0L),
  #   emenor18 = if_else(p09 < 18, 1L, 0L),
  #   e18_25 = if_else(p09 >= 18 & p09 < 25 , 1L, 0L)
  # )


# Escolaridad -------------------------------------------------------------
# Nota - tratamiento especial a menores de edad 
# indice descerción/atraso escolar hasta 18 años: 
# (edad - años estudio) debiese ser menor a 7 años - para 18
# Indice distinto para jovenes entre 18 y 25 años

escolaridad <- base_rm_hog %>% mutate(
  esc_recode = case_when(
    p15 >= 1 & p15 <= 3   ~ 1L,
    p15 == 4              ~ 2L,
    p15 > 4 & p15 <= 6    ~ 3L,
    p15 > 6 & p15<=  10   ~ 4L,
    p15 == 11             ~ 5L,
    p15 == 12             ~ 6L,
    p15 >= 13 & p15 <= 14 ~ 7L,
    TRUE ~ NA_integer_
  ),
  esc_cat1 = if_else(esc_recode == 1, 1L, 0L),
  esc_cat2 = if_else(esc_recode == 2, 1L, 0L),
  esc_cat3 = if_else(esc_recode == 3, 1L, 0L),
  esc_cat4 = if_else(esc_recode == 4, 1L, 0L),
  esc_cat5 = if_else(esc_recode == 5, 1L, 0L),
  esc_cat6 = if_else(esc_recode == 6, 1L, 0L),
  esc_cat7 = if_else(esc_recode == 7, 1L, 0L)
)  

#  la ponderación dada a escolaridad 2: 
# educación diferencial, que no venía, fue finalmente 350; y para Postgrado 1000

# Materialidad de la Vivienda ---------------------------------------------

esc_materialidad <- escolaridad %>% mutate(
  cond_muro = case_when(
    # Aceptable
     p03a >= 1 & p03a <= 3 ~ 1L,
    # Recuperable    
     p03a >3 & p03a   <= 5 ~ 2L,
    # Irrecuperable
     p03a >5 & p03a   <= 7 ~ 3L,
    TRUE ~ NA_integer_
  ),
  cond_cubierta = case_when(
    # Aceptable
    p03b >= 1 & p03b <= 3  ~ 1L,
    # Recuperable    
    p03b >3 & p03b <= 5    ~ 2L,
    # Irrecuperable
    p03b >5 & p03b <= 7    ~ 3L,
    TRUE ~ NA_integer_
  ),
  cond_suelo = case_when(
    # Aceptable
    p03c == 1 & p03c <= 3 ~ 1L,
    # Recuperable    
    p03c >1 & p03c <= 4   ~ 2L,
    # Irrecuperable
    p03c == 5             ~ 3L,
    TRUE ~ NA_integer_
  ),
  mat_aceptable   = if_else(cond_muro == 1 & cond_cubierta == 1 & cond_suelo == 1, 1L, 0L),
  mat_irrecup     = if_else(cond_muro == 3 | cond_cubierta == 3 | cond_suelo == 3, 1L, 0L),
  mat_recuperable = if_else(mat_aceptable == 0 & mat_irrecup == 0, 1L, 0L)
)

# Hacinamiento ------------------------------------------------------------
# Personas / num. dormitorios
esc_mat_hacinam <- esc_materialidad %>% mutate(
  ind_hacinam = case_when(
    p04 >= 1 ~ cant_per/p04,
    # Indice Sin dormitorios se elige de tal manera que: 
    # 1 persona viviendo en un estudio no presenta hacinamiento, 
    # 2 personas hacinamiento medio y más de 3, hacinamiento critico
    p04 == 0 ~ cant_per*2),
  sin_hacin = if_else(ind_hacinam <= 2.4, 1L, 0L),
  hacin_medio = if_else(ind_hacinam >2.4 & ind_hacinam <= 4.9, 1L, 0L),
  # En metodologia falta valor entre 5 y 6, hacin critico partia en 6
  hacin_critico = if_else(ind_hacinam > 5, 1L, 0L)
  )
summary(hacinam$ind_hacinam)

# Asignar puntajes  -------------------------------------------------------

indic_final <- esc_mat_hacinam %>% 
  mutate(
    # Calculo puntaje escolaridad
    ptje_esc = case_when(
      esc_recode == 1 ~ 300,
      esc_recode == 2 ~ 350,
      esc_recode == 3 ~ 400,
      esc_recode == 4 ~ 550,
      esc_recode == 5 ~ 650,
      esc_recode == 6 ~ 790,
      esc_recode == 7 ~ 1000
    ),
    # Calculo Puntaje hacinamiento
    ptje_hacin = case_when(
      sin_hacin == 1     ~ 1000,
      hacin_medio == 1   ~ 400,
      hacin_critico == 1 ~ 200
    ),
    # Calculo puntaje materialidad
    ptje_mater = case_when(
      mat_aceptable == 1   ~ 1000,
      mat_recuperable == 1 ~ 600,
      mat_irrecup == 1     ~ 200
    ) 
  ) %>% 
  select(region, provincia, comuna, dc, zc_loc, id_zona_loc, nviv, nhogar, personan,
         esc_recode, esc_cat1, esc_cat2, esc_cat3, esc_cat4, esc_cat5, esc_cat6, esc_cat7,
         cond_muro, cond_cubierta, cond_suelo, 
         mat_aceptable, mat_irrecup, mat_recuperable,ind_hacinam, 
         sin_hacin, hacin_medio, hacin_critico, 
         ptje_esc, ptje_hacin, ptje_mater)

indic_final %>% write.csv2(glue("{out_dir}/Censo2017_ISMT_hogares.csv"))
indic_final %>% saveRDS(glue("{out_dir}/Censo2017_ISMT_hogares.Rds"))
