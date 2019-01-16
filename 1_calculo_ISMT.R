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
summary(base_rm$cant_hog)
summary(base_rm$cant_hog)

base_rm <- base_rm %>% 
  # filtrar tipo hogar y ocupacion de la vivienda
  filter(p01 < 8 & p02==1) %>% 
  mutate(
  #Número de piezas usadas exclusivamente como dormitorio
  p04 = case_when(
    p04 == 98 ~ NA_integer_,
    p04 == 99 ~ NA_integer_,
    TRUE~ p04
  ),
  # Cantidad de hogares por vivienda
  cant_hog = case_when(
    cant_hog == 98 ~ NA_integer_,
    cant_hog == 99 ~ NA_integer_,
    TRUE ~ cant_hog
  ),
  nhogar = case_when(
    cant_hog == 98 ~ NA_integer_,
    cant_hog == 99 ~ NA_integer_,
    TRUE ~ cant_hog
  )
)

# Probar codigo con un subset
# subset_rm <- base_rm %>% filter(id_zona_loc==1279)

# seleccionar variables y definir tramos edades
base_rm_hog <- base_rm %>%  select(provincia:cant_per, region) %>% 
  # Se selecciona solo jefe de hogar
  filter(personan == 1) 

# Escolaridad -------------------------------------------------------------

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
  esc_cat7 = if_else(esc_recode == 7, 1L, 0L),
  a_esc_cont = case_when(
    escolaridad == NA ~ NA_integer_,
    escolaridad == 99 ~ NA_integer_,
    escolaridad == 27 ~ NA_integer_,
    TRUE ~ escolaridad)
)  


# Materialidad de la Vivienda ---------------------------------------------

esc_materialidad <- escolaridad %>% mutate(
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
  mat_aceptable   = if_else(cond_muro == 3 & cond_cubierta == 3 & cond_suelo == 3, 1L, 0L),
  mat_irrecup     = if_else(cond_muro == 1 | cond_cubierta == 1 | cond_suelo == 1, 1L, 0L),
  mat_recuperable = if_else(mat_aceptable == 0 & mat_irrecup == 0, 1L, 0L),
  ind_mater = cond_muro + cond_cubierta + cond_suelo
)

# Hacinamiento ------------------------------------------------------------
# Personas / num. dormitorios
esc_mat_hacinam <- esc_materialidad %>% 
 mutate(
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
summary(esc_mat_hacinam$ind_hacinam)
summary(esc_mat_hacinam$ind_mater)


# Agregar allegamiento ----------------------------------------------------

esc_mat_hacinam_all <- esc_mat_hacinam %>% 
  mutate(
    n_hog_alleg = (cant_hog - 1)
  )

summary(esc_mat_hacinam_all$n_hog_alleg)

#Revisar las viviendas con más de 20 hogares - comunas Santiago, Estacion Central, Q Normal, Buin
test <- esc_mat_hacinam_all %>% filter(n_hog_alleg>20) %>% 
  mutate(geocode = (comuna*1000000)+(dc*10000)+(1*1000)+(zc_loc),
         geocode = as.character(geocode)) %>% 
  select(geocode, provincia, comuna, dc, area, zc_loc, cant_hog, cant_per)


# Asignar puntajes  -------------------------------------------------------

# indic_final <- esc_mat_hacinam %>% 
indic_final2 <- esc_mat_hacinam_all %>% 
  mutate(
    # # Calculo puntaje escolaridad
    # ptje_esc = case_when(
    #   esc_recode == 1 ~ 300,
    #   esc_recode == 2 ~ 350,
    #   esc_recode == 3 ~ 400,
    #   esc_recode == 4 ~ 550,
    #   esc_recode == 5 ~ 650,
    #   esc_recode == 6 ~ 790,
    #   esc_recode == 7 ~ 1000
    # ),
    # # Calculo Puntaje hacinamiento
    # ptje_hacin = case_when(
    #   sin_hacin == 1     ~ 1000,
    #   hacin_medio == 1   ~ 400,
    #   hacin_critico == 1 ~ 200
    # ),
    # # Calculo puntaje materialidad
    # ptje_mater = case_when(
    #   mat_aceptable == 1   ~ 1000,
    #   mat_recuperable == 1 ~ 600,
    #   mat_irrecup == 1     ~ 200
    # Indice de hacinamiento y allegamiento pasarlo a valor negativo
    ind_hacinam_ = -1 * ind_hacinam, 
    ind_alleg = -1 * n_hog_alleg, 
    # z scores
    z_ptje_esc = as.numeric(scale(a_esc_cont)),
    z_ptje_hacin = as.numeric(scale(ind_hacinam_)),
    z_ptje_mater = as.numeric(scale(ind_mater)),
    z_ptje_alleg = as.numeric(scale(ind_alleg)),
    # Valores escalados
    ptje_esc = (a_esc_cont-min(a_esc_cont, na.rm = TRUE))/(max(a_esc_cont, na.rm = TRUE)-min(a_esc_cont, na.rm = TRUE)) * 1000,
    ptje_hacin = (ind_hacinam_-min(ind_hacinam_, na.rm = TRUE))/(max(ind_hacinam_, na.rm = TRUE)-min(ind_hacinam_, na.rm = TRUE)) * 1000,
    ptje_mater = (ind_mater-min(ind_mater, na.rm = TRUE))/(max(ind_mater, na.rm = TRUE)-min(ind_mater, na.rm = TRUE)) *1000,
    ptje_alleg = (ind_alleg-min(ind_alleg, na.rm = TRUE))/(max(ind_alleg, na.rm = TRUE)-min(ind_alleg, na.rm = TRUE)) *1000
  ) %>% 
  select(region, provincia, comuna, dc, zc_loc, id_zona_loc, nviv, nhogar, personan,
         esc_recode, esc_cat1, esc_cat2, esc_cat3, esc_cat4, esc_cat5, esc_cat6, esc_cat7,
         cond_muro, cond_cubierta, cond_suelo, 
         mat_aceptable, mat_irrecup, mat_recuperable,  
         sin_hacin, hacin_medio, hacin_critico, a_esc_cont, ind_hacinam_, ind_mater, ind_alleg,
         z_ptje_esc, z_ptje_hacin, z_ptje_mater, z_ptje_alleg,
         ptje_esc, ptje_hacin, ptje_mater, ptje_alleg)

summary(indic_final2$ptje_esc)
summary(indic_final2$ptje_hacin)
summary(indic_final2$ptje_mater)
summary(indic_final2$ptje_alleg)

# summary(indic_final2$ptje_esc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   428.6   571.4   563.2   714.3  1000.0   43126 
# > summary(indic_final2$ptje_hacin)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   979.7   984.2   983.0   988.7  1000.0   77655 
# > summary(indic_final2$ptje_mater)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0  1000.0  1000.0   978.3  1000.0  1000.0   32939 
# > summary(indic_final2$ptje_alleg)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0    1000    1000     999    1000    1000    2324 


indic_final2 %>% write.csv2(glue("{out_dir}/Censo2017_ISMT_hogares_v3.csv"))
indic_final2 %>% saveRDS(glue("{out_dir}/Censo2017_ISMT_hogares_v3.Rds"))
