# Nombre Programa: 4_ISMT_historico.R
# Ubicacion: GitHub/ISMT
# Autor: Monica Flores
# Fecha Creacion: 23/01/2018
# Proyecto: ISMT
# Objetivo: Calcular Indice Sociomaterial 
# Output: C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Historico
# Notas:

# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)
library(sf)


# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
in_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/14_Microdatos/Clean_Personas"
ismt_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT"
auc_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/02_Insumos/Zonas Censales AUC_2017"
hist_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/11_Accesibilidad/Data/Output"

# Aumentar limite memoria - usando pendrive
# memory.limit(16000)

# Leer archivo Censo  vars homogeneas -----------------------------------------

# # Censo rm - filtrar año 2017 
# data_rm <- readRDS(glue("{hist_dir}/Censo1992_2017_Persona_CleanList_R13.Rds"))
# data_rm <- data_rm[['2017']]

# # Censo Regiones 04 y 08
# data_r08 <- readRDS(glue("{in_dir}/Data Requests/Censo2017_r04_r08_ISMT.Rds")) %>% mutate(region=as.integer(region)) %>% 
#   filter(region == 8) 

## Leer shapes Area Urbana Consolidada
# geo_r04 <- st_read(glue("{auc_dir}/R04_LaSerena")) %>% rename_all(tolower) %>% select(geocodigo, geometry)
# geo_r08 <- st_read(glue("{auc_dir}/R08_Concepcion")) %>% rename_all(tolower) %>% select(geocodigo, geometry)
# geo_r13 <- st_read(glue("{auc_dir}/R13_Santiago")) %>% rename_all(tolower) %>% select(geocodigo, geometry)

# # Unir dataframes en lista
# data <- list(data_rm, data_r08) %>% set_names(c("13", "8"))

# # Inner join shape con data censo
# data[['4']] <- data[['4']] %>%  inner_join(as.data.frame(geo_r04), by = c("geocode"="geocodigo")) %>% select(-geometry)
# data[['8']] <- data[['8']] %>%  inner_join(as.data.frame(geo_r08), by = c("geocode"="geocodigo")) %>% select(-geometry)
# data[['13']] <- data[['13']] %>%  inner_join(as.data.frame(geo_r13), by = c("geocode"="geocodigo")) %>% select(-geometry)

# # Censo Region 09
# data_r09 <- readRDS(glue("{in_dir}/Data Requests/Censo2012_R09.Rds")) %>% mutate(region=as.integer(region)) %>% 
#   filter(region == 9)

# Censo 2012 RM
data_r13 <- readRDS(glue("{in_dir}/Data Requests/Censo2012_R13.Rds")) %>% mutate(region=as.integer(region)) %>% 
  filter(region == 13)


# Leer shapes Area Urbana Consolidada
# geo_r09 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/Input/mzn_AUC_temuco.shp") %>% 
geo_r13 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/Input/mzn_AUC_stgo.shp") %>% 
  mutate(
    MANZENT = (CUT*1000000000) + (DISTRITO*10000000) + (1*1000000) + (ZONA*1000) + MANZANA, # Rehacer Manzent
    geocode = (CUT*1000000) + (DISTRITO*10000) + (1*1000) + (ZONA)  # Crear codigo zona
  ) %>% 
  rename_all(tolower) %>% 
  select(manzent, geocode, geometry)

# Inner join shape con data censo
# data <- data_r09 %>%  inner_join(as.data.frame(geo_r09), by =c("geocode", "manzent")) %>% select(-geometry)
data <- data_r13 %>%  inner_join(as.data.frame(geo_r13), by =c("geocode", "manzent")) %>% select(-geometry)

# Calcular numero mayores 60 años por hogar -------------------------------
# 
# mayores60_hog <- data %>% bind_rows() %>% 
#   group_by(year, region, comuna, geocode, folio, nviv, nhogar) %>% 
#   mutate(dummy_pers = 1) %>% 
#   #Mayores de 60 años por hogar
#   summarise(
#   pers_60 = sum(if_else(edad >= 60, dummy_pers ,0), na.rm=TRUE),
#   pers_tot = sum(dummy_pers, na.rm=TRUE)
# ) %>% ungroup()

# Nuevas variables nivel vivienda --------------------------------------------------

# Calculo data nivel hogar
# data_clean <- map2(data, names(data), 
#                     ~dplyr::filter(.x, parentesco == 1)) %>%  
#   map2_df(names(data),
#           ~dplyr::transmute ( .x,
data_clean <- data %>% filter(parentesco == 1) %>% 
  mutate(
      year = year,  
      region = region,
      comuna = comuna,
      geocode = geocode,
      manzent = manzent,
      folio = folio,
      nviv = nviv,
      nhogar = nhogar,
      # Dummy Vivienda
      hogar = 1,
      # Años escolaridad Jefe de Hogar
      esc_jh = escolaridad, 
      # allegamiento
      n_hog_alleg = (cant_hog - 1), # Luego hacer summary por vivienda: nhogar==1
      # Hacinamiento
      ind_hacinam = case_when( # Por vivienda
        n_dormitorios >= 1 ~ cant_per/n_dormitorios,
        # Indice Sin dormitorios se elige de tal manera que: 
        # 1 persona viviendo en un estudio no presenta hacinamiento, 
        # 2 personas hacinamiento medio y más de 3, hacinamiento critico
        n_dormitorios == 0 ~ cant_per*2
      ),
      # Hacinamiento categorias
      hacin_medio = if_else(ind_hacinam >=2.5 & ind_hacinam < 5, 1L, 0L),
      hacin_critico = if_else(ind_hacinam >= 5, 1L, 0L),
      hacin_med_crit = (hacin_medio + hacin_critico), # Total viviendas con hacinamiento medio o critico
      # Indice materialidad
      mat_aceptable   = if_else(cond_muro == 3 & cond_cubierta == 3 & cond_suelo == 3, 1L, 0L),
      mat_irrecup     = if_else(cond_muro == 1 | cond_cubierta == 1 | cond_suelo == 1, 1L, 0L),
      mat_recuperable = if_else(mat_aceptable == 0 & mat_irrecup == 0, 1L, 0L),
      ind_mater = cond_muro + cond_cubierta + cond_suelo
      #)
  ) %>% 
  # Pre-Calculo ISMT
  mutate(
    ind_hacinam_ = -1 * ind_hacinam, 
    ind_alleg = -1 * n_hog_alleg, 
    # z scores
    z_ptje_esc = as.numeric(scale(esc_jh)),
    z_ptje_hacin = as.numeric(scale(ind_hacinam_)),
    z_ptje_mater = as.numeric(scale(ind_mater)),
    z_ptje_alleg = as.numeric(scale(ind_alleg)),
    # Valores escalados
    ptje_esc = (esc_jh-min(esc_jh, na.rm = TRUE))/(max(esc_jh, na.rm = TRUE)-min(esc_jh, na.rm = TRUE)) * 1000,
    ptje_hacin = (ind_hacinam_-min(ind_hacinam_, na.rm = TRUE))/(max(ind_hacinam_, na.rm = TRUE)-min(ind_hacinam_, na.rm = TRUE)) * 1000,
    ptje_mater = (ind_mater-min(ind_mater, na.rm = TRUE))/(max(ind_mater, na.rm = TRUE)-min(ind_mater, na.rm = TRUE)) *1000,
    ptje_alleg = (ind_alleg-min(ind_alleg, na.rm = TRUE))/(max(ind_alleg, na.rm = TRUE)-min(ind_alleg, na.rm = TRUE)) *1000
  ) #%>% 
  ## Unir personas mayores 60 por hogar
  #left_join(mayores60_hog, by = c("year", "region", "comuna", "geocode", "folio", "nviv", "nhogar")) #%>% 
  #split(.$region)

# Guardar Dataset nivel hogar - ISMT
# data_clean %>% saveRDS(glue("{ismt_dir}/Output/Censo2017_Hogar_ISMT_R13_R08.Rds"))
# data_clean %>% saveRDS(glue("{ismt_dir}/Output/Censo2012_Hogar_ISMT_R09.Rds"))
data_clean %>% saveRDS(glue("{ismt_dir}/Output/Censo2012_Hogar_ISMT_R13.Rds"))


####### Calcular homals GitHub/ISMT/2_homals.R #########

# leer archivo si se parte desde aquí 
# data_clean <- readRDS(glue("{ismt_dir}/Output/Censo2012_Hogar_ISMT_R09.Rds")) 
# data_08_04 <- readRDS(glue("{ismt_dir}/Output/Censo2017_Hogar_ISMT_R04_R08.Rds"))
# data_rm <- readRDS(glue("{ismt_dir}/Output/Censo2017_Hogar_ISMT_R13_R08.Rds")) 

# data_clean <- data_08_04 %>% append(data_rm['13'] )
# data_clean <- data_rm[['13']]
data_clean <- readRDS(glue("{ismt_dir}/Output/Censo2012_Hogar_ISMT_R13.Rds"))

# Calculo ISMT ------------------------------------------------------------

# # Coeff homals R04 - ISMT 2017
# Esc4 <- 0.2808857  ### Escolaridad Puntaje
# Viv4 <- 0.2996181  ### Calidad Vivienda
# Hac4 <- 0.3224852  ### Hacinamiento
# All4 <- 0.2520207  ### Allegamiento

# # Coeff homals RM - ISMT 2017
# Esc13 <- 0.2783314  ### Escolaridad Puntaje
# Viv13 <- 0.2867274  ### Calidad Vivienda
# Hac13 <- 0.3271089  ### Hacinamiento
# All13 <- 0.2522210  ### Allegamiento

# Coeff homals RM - ISMT 2012
Esc13 <- 0.3333650  ### Escolaridad Puntaje
Viv13 <- 0.3043160  ### Calidad Vivienda
Hac13 <- 0.3233621  ### Hacinamiento
All13 <- 0.1813503  ### Allegamiento

# # Coeff homals R08 - ISMT 2017
# Esc8 <- 0.3169055  ### Escolaridad Puntaje
# Viv8 <- 0.3298117  ### Calidad Vivienda
# Hac8 <- 0.2881263  ### Hacinamiento
# All8 <- 0.1012507  ### Allegamiento
# 
# # Coeff homals R09 - ISMT 2012
# Esc9 <- 0.34098386  ### Escolaridad Puntaje
# Viv9 <- 0.27572421  ### Calidad Vivienda
# Hac9 <- 0.34196264  ### Hacinamiento
# All9 <- 0.06282354  ### Allegamiento

# Calcular puntaje ISMT RM
# data_clean[['13']] <- data_clean[['13']] %>%
data_clean <- data_clean %>%
  # Calculo ISMT - pesos según homals
  mutate(
    ptje_raw = (ptje_esc*Esc13) + (ptje_hacin*Hac13) + (ptje_mater*Viv13) + (ptje_alleg*All13),
    ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
  )

# # Calcular puntaje ISMT Concepcion
# data_clean[['8']] <- data_clean[['8']] %>%
#   # Calculo ISMT - pesos según homals
#   mutate(
#     ptje_raw = (ptje_esc*Esc8) + (ptje_hacin*Hac8) + (ptje_mater*Viv8) + (ptje_alleg*All8),
#     ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
#   )
# 
# # Calcular puntaje ISMT La Serena
# data_clean[['4']] <- data_clean[['4']] %>%
#   # Calculo ISMT - pesos según homals
#   mutate(
#     ptje_raw = (ptje_esc*Esc4) + (ptje_hacin*Hac4) + (ptje_mater*Viv4) + (ptje_alleg*All4),
#     ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
#   )
# 
# # Calcular puntaje ISMT Temuco  - pesos según homals
# data_clean <- data_clean %>%
#   mutate(
#     ptje_raw = (ptje_esc*Esc9) + (ptje_hacin*Hac9) + (ptje_mater*Viv9) + (ptje_alleg*All9),
#     ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
#   )

# Calcular centiles para GSE ----------------------------------------------

# Calcular centiles
quant_R13 <- quantile(data_clean$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
# quant_R08 <- quantile(data_clean[['8']]$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
# quant_R04 <- quantile(data_clean[['4']]$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
# quant_R09 <- quantile(data_clean$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()

# Guardar quantiles 
# quant_R09 %>% saveRDS("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/ISMT_quant_R09.Rds")
quant_R13 %>% saveRDS("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/ISMT_quant_R13.Rds")

# Determinar tope centil GSE x ciudad ( x <= R013_E) ( x > R013_E & x <= R013_D )
R013_E <- quant_R13["6%",]
R013_D <- quant_R13["36%",]
R013_C3 <- quant_R13["64%",]
R013_C2 <- quant_R13["79%",]
R013_ABC1 <- quant_R13["100%",]

# R08_E <- quant_R08["6%",]
# R08_D <- quant_R08["36%",]
# R08_C3 <- quant_R08["64%",]
# R08_C2 <- quant_R08["79%",]
# R08_ABC1 <- quant_R08["100%",]
# 
# R04_E <- quant_R04["6%",]
# R04_D <- quant_R04["36%",]
# R04_C3 <- quant_R04["64%",]
# R04_C2 <- quant_R04["79%",]
# R04_ABC1 <- quant_R04["100%",]
# 
# E <- quant_R09["6%",]
# D <- quant_R09["36%",]
# C3 <- quant_R09["64%",]
# C2 <- quant_R09["79%",]
# ABC1 <- quant_R09["100%",]



# Unir lista
# data_clean <- map2_df(data_clean, # Map df returns a dataframe
#                       names(data_clean),
#                       ~dplyr::select(.x, year, region, comuna, geocode, manzent, folio, nviv, nhogar, ptje_ISMT))

# Calcular GSE persona
data_clean <- data_clean %>% 
  select(year, region, comuna, geocode, manzent, folio, nviv, nhogar, ptje_ISMT) %>% 
  mutate(
    GSE_ISMT_pers = case_when(
      # # La Serena
      # region == 4 & ptje_ISMT <= R04_E ~ "E",
      # region == 4 & ptje_ISMT > R04_E & ptje_ISMT <= R04_D ~ "D",
      # region == 4 & ptje_ISMT > R04_D & ptje_ISMT <= R04_C3 ~ "C3",
      # region == 4 & ptje_ISMT > R04_C3 & ptje_ISMT  <= R04_C2 ~ "C2",
      # region == 4 & ptje_ISMT > R04_C2 & ptje_ISMT  ~ "ABC1",
      # Santiago
      region == 13 & ptje_ISMT <= R013_E ~ "E",
      region == 13 & ptje_ISMT > R013_E & ptje_ISMT <= R013_D ~ "D",
      region == 13 & ptje_ISMT > R013_D & ptje_ISMT <= R013_C3 ~ "C3",
      region == 13 & ptje_ISMT > R013_C3 & ptje_ISMT  <= R013_C2 ~ "C2",
      region == 13 & ptje_ISMT > R013_C2 & ptje_ISMT  ~ "ABC1",
      # # Concepcion
      # region == 8 & ptje_ISMT <= R08_E ~ "E",
      # region == 8 & ptje_ISMT > R08_E & ptje_ISMT <= R08_D ~ "D",
      # region == 8 & ptje_ISMT > R08_D & ptje_ISMT <= R08_C3 ~ "C3",
      # region == 8 & ptje_ISMT > R08_C3 & ptje_ISMT  <= R08_C2 ~ "C2",
      # region == 8 & ptje_ISMT > R08_C2 & ptje_ISMT  ~ "ABC1"
      # # Temuco
      # region == 9 & ptje_ISMT <= R09_E ~ "E",
      # region == 9 & ptje_ISMT > R09_E & ptje_ISMT <= R09_D ~ "D",
      # region == 9 & ptje_ISMT > R09_D & ptje_ISMT <= R09_C3 ~ "C3",
      # region == 9 & ptje_ISMT > R09_C3 & ptje_ISMT  <= R09_C2 ~ "C2",
      # region == 9 & ptje_ISMT > R09_C2 & ptje_ISMT  <= R09_ABC1 ~ "ABC1",
    )
  ) %>% 
  filter(!is.na(ptje_ISMT))

# Calcular % hogares GSE por comuna ---------------------------------------

# # Calcular porcentaje de hogares por GSE / manzana
# tot_hog <- data_clean %>%   filter(!is.na(ptje_ISMT)) %>% 
#   mutate(hogar = 1) %>% 
#   group_by(year, region, comuna) %>% 
#   summarise(
#     hog_comuna = sum(hogar, na.rm = TRUE)
#   )
# 
# idgeo_comuna <- readRDS(glue("{in_dir}/Censo2017_IDgeo.Rds")) %>% 
#   transmute(
#     comuna = comuna_15r, 
#     nom_comuna = nom_comuna_15r) %>% unique()
# 
# GSE_hog <- data_clean %>% filter(!is.na(ptje_ISMT)) %>% 
#   mutate(hogar = 1) %>% 
#   group_by(year, region, comuna, GSE_ISMT_pers) %>% 
#   summarise(
#    n_hog_gse = sum(hogar, na.rm = TRUE)
#   ) %>% 
#   ungroup() %>% 
#   left_join(tot_hog, by = c("year", "region", "comuna")) %>% 
#   mutate(
#     GSE = GSE_ISMT_pers, 
#     pct_hog_gse = n_hog_gse/hog_comuna
#   ) %>% 
#   left_join(idgeo_comuna, by= "comuna") %>% 
#   select(region, comuna, nom_comuna, GSE, pct_hog_gse, n_hog_gse, hog_comuna)
# 
# GSE_hog %>% write.csv2(glue("{in_dir}/Data Requests/ISMT_comuna_2017.csv"))

# Determinar 40% más vulnerable -------------------------------------------

# ISMT Asignar percentil 40 a variable - por region
pct40 <- data_clean %>% group_by(region) %>% summarise(
  pct40ISMT = quantile(ptje_ISMT, probs = 0.4, na.rm = TRUE) 
)

# Determinacion de 40% mas vulnerable por año
data_hog <- data_clean %>% 
  left_join(pct40, by="region") %>%
  mutate(
    # pctrank_ISMT = percent_rank(ptje_ISMT), # esto tiene ser por año
    hog40vuln_ISMT = if_else(ptje_ISMT <= pct40ISMT, 1L, 0L)#,  determinar si vivienda pertenece a 40pct mas vulnerable
    # Agregar nombre ciudad
    # ciudad = if_else(region == 13, "Santiago", "Concepcion")
    
  )

# head(data_hog)  
# summary(data_hog$ptje_ISMT)

# Test ISMT ---------------------------------------------------------------

# Plotear histograma ISMT
hist1 <- as.data.frame(data_hog) %>% 
  ggplot(aes(ptje_ISMT)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.01) #+
  # facet_grid(. ~ ciudad)
hist1 + labs(x = "Puntaje Normalizado", y = "Frecuencia",
             title ="Histograma ISMT",
             subtitle = "a Nivel de Hogar en la RM",
             caption = NULL)

# Chequear que porcentaje de la poblacionb de hogares está en el 40% mas vulnerable
test40pct <- data_hog %>% group_by(region) %>% 
  summarise(
    hog_40pct = sum(hog40vuln_ISMT, na.rm = TRUE),
    tot_hog = sum(hogar, na.rm = TRUE)
  ) %>% 
  mutate(
    pct_vuln = hog_40pct/tot_hog # Da 40.00% :) 
  )

# Summarise datasets ------------------------------------------------------

# Summary GSE zona censal - Temuco
data_hog_zc <- data_clean %>% 
  group_by(region, geocode) %>% 
  summarise(
    ISMTptj_zc = median(ptje_ISMT, na.rm = TRUE) # cambiar por median
  ) %>% 
  mutate(
    GSE_ISMT_zc = case_when(
      # # Temuco
      # ISMTptj_zc <= R09_E ~ "E",
      # ISMTptj_zc > R09_E & ISMTptj_zc <= R09_D ~ "D",
      # ISMTptj_zc > R09_D & ISMTptj_zc <= R09_C3 ~ "C3",
      # ISMTptj_zc > R09_C3 & ISMTptj_zc  <= R09_C2 ~ "C2",
      # ISMTptj_zc > R09_C2 & ISMTptj_zc  <= R09_ABC1 ~ "ABC1",
      # La Serena
      # region == 4 & ISMTptj <= R04_E ~ "E",
      # region == 4 & ISMTptj > R04_E & ISMTptj <= R04_D ~ "D",
      # region == 4 & ISMTptj > R04_D & ISMTptj <= R04_C3 ~ "C3",
      # region == 4 & ISMTptj > R04_C3 & ISMTptj  <= R04_C2 ~ "C2",
      # region == 4 & ISMTptj > R04_C2 & ISMTptj  <= R04_ABC1 ~ "ABC1",
      # Santiago
      region == 13 & ISMTptj_zc <= R013_E ~ "E",
      region == 13 & ISMTptj_zc > R013_E & ISMTptj_zc <= R013_D ~ "D",
      region == 13 & ISMTptj_zc > R013_D & ISMTptj_zc <= R013_C3 ~ "C3",
      region == 13 & ISMTptj_zc > R013_C3 & ISMTptj_zc  <= R013_C2 ~ "C2",
      region == 13 & ISMTptj_zc > R013_C2 & ISMTptj_zc  <= R013_ABC1 ~ "ABC1",
      # # Concepcion
      # region == 8 & ISMTptj <= R08_E ~ "E",
      # region == 8 & ISMTptj > R08_E & ISMTptj <= R08_D ~ "D",
      # region == 8 & ISMTptj > R08_D & ISMTptj <= R08_C3 ~ "C3",
      # region == 8 & ISMTptj > R08_C3 & ISMTptj  <= R08_C2 ~ "C2",
      # region == 8 & ISMTptj > R08_C2 & ISMTptj  <= R08_ABC1 ~ "ABC1"
    )
  ) %>% ungroup()

# Summary info hogar (ISMT)
data_hog_summ <- data_clean %>% 
  group_by(year, region, comuna, geocode, manzent) %>%
  summarise(
  #   n_hog = sum(hogar),
  #   #pers_60 = sum(pers_60),
  #   #pers_tot = sum(pers_tot),
  #   esc_jh = mean(esc_jh, na.rm = TRUE),
  #   ind_hac = mean(ind_hacinam, na.rm = TRUE),
  #   ind_mat = mean(ind_mater, na.rm = TRUE),
  #   h_all = sum(n_hog_alleg[nhogar == 1], na.rm = TRUE),
  #   h_hac_m = sum(hacin_medio, na.rm = TRUE),
  #   h_haccrit = sum(hacin_critico, na.rm = TRUE),
  #   hac_m_crit = sum(hacin_med_crit, na.rm = TRUE),
  #   mat_acep = sum(mat_aceptable, na.rm = TRUE),
  #   mat_irr = sum(mat_irrecup, na.rm = TRUE),
  #   mat_recp = sum(mat_recuperable, na.rm = TRUE),
  #   # ISMT
  #   h_40pct = sum(hog40vuln_ISMT, na.rm = TRUE), # Suma de viviendas bajo el 40% mas vulnerable
    ISMT_mzn = median(ptje_ISMT, na.rm = TRUE) # cambiar por mediana
  #  # prom_rank = mean(pctrank_ISMT)
  ) %>% ungroup() %>%
  mutate(
    # pct40_vuln = h_40pct/n_hog,
    GSE_ISMT_mzn = case_when(
      # # La Serena
      # region == 4 & ISMTptj <= R04_E ~ "E",
      # region == 4 & ISMTptj > R04_E & ISMTptj <= R04_D ~ "D",
      # region == 4 & ISMTptj > R04_D & ISMTptj <= R04_C3 ~ "C3",
      # region == 4 & ISMTptj > R04_C3 & ISMTptj  <= R04_C2 ~ "C2",
      # region == 4 & ISMTptj > R04_C2 & ISMTptj  <= R04_ABC1 ~ "ABC1",
      # Santiago
      region == 13 & ISMT_mzn <= R013_E ~ "E",
      region == 13 & ISMT_mzn > R013_E & ISMT_mzn <= R013_D ~ "D",
      region == 13 & ISMT_mzn > R013_D & ISMT_mzn <= R013_C3 ~ "C3",
      region == 13 & ISMT_mzn > R013_C3 & ISMT_mzn  <= R013_C2 ~ "C2",
      region == 13 & ISMT_mzn > R013_C2 & ISMT_mzn  <= R013_ABC1 ~ "ABC1",
      # # Concepcion
      # region == 8 & ISMTptj <= R08_E ~ "E",
      # region == 8 & ISMTptj > R08_E & ISMTptj <= R08_D ~ "D",
      # region == 8 & ISMTptj > R08_D & ISMTptj <= R08_C3 ~ "C3",
      # region == 8 & ISMTptj > R08_C3 & ISMTptj  <= R08_C2 ~ "C2",
      # region == 8 & ISMTptj > R08_C2 & ISMTptj  <= R08_ABC1 ~ "ABC1",
      # # Temuco
      # region == 9 & ISMTptj <= R09_E ~ "E",
      # region == 9 & ISMTptj > R09_E & ISMTptj <= R09_D ~ "D",
      # region == 9 & ISMTptj > R09_D & ISMTptj <= R09_C3 ~ "C3",
      # region == 9 & ISMTptj > R09_C3 & ISMTptj  <= R09_C2 ~ "C2",
      # region == 9 & ISMTptj > R09_C2 & ISMTptj  <= R09_ABC1 ~ "ABC1"
    )
  ) %>% ungroup() %>%
  as.data.frame() %>%
  left_join(data_hog_zc, by = "geocode")
  #mutate(pct_60a = pers_60/pers_tot) %>% 
  # split(.$region)


    
# Unir info a shapes
# geo_r13 <- geo_r13 %>% left_join(data_hog_summ[['13']], by = c("geocodigo"="geocode"))
# geo_r08 <- geo_r08 %>% left_join(data_hog_summ[['8']], by = c("geocodigo"="geocode"))
geo_r09 <- geo_r09 %>% left_join(data_hog_summ, by = "manzent") 

# Plotear shapes ----------------------------------------------------------

# Plotear mapa con color por puntaje ISMT

# # Metropolitana
# mapa_ISMT13 <- ggplot(data = geo_r13, aes(fill = ISMTptj)) + 
#   geom_sf(lwd = 0) +
#   scale_fill_gradient(name = "Puntaje ISMT", 
#                       low = "#d53e4f", 
#                       high = "#084594",
#                       labels = scales::percent,
#                       breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
#   ) +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# # Plotearlo con legenda 
# mapa_ISMT13 +  labs(title ="ISMT Santiago",
#                      subtitle = NULL,
#                      caption = NULL) 
# 
# # Concepcion
# mapa_ISMT08 <- ggplot(data = geo_r08, aes(fill = ISMTptj)) + 
#   geom_sf(lwd = 0) +
#   scale_fill_gradient(name = "Puntaje ISMT", 
#                       low = "#d53e4f", 
#                       high = "#084594",
#                       labels = scales::percent,
#                       breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
#   ) +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# # Plotearlo con legenda 
# mapa_ISMT08 +  labs(title ="Concepción",
#                     subtitle = NULL,
#                     caption = NULL) 

# Temuco
mapa_ISMT09 <- ggplot(data = geo_r09, aes(fill = ISMTptj)) + 
  geom_sf(lwd = 0) +
  scale_fill_gradient(name = "Puntaje ISMT", 
                      low = "#d53e4f", 
                      high = "#084594",
                      labels = scales::percent,
                      breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# Plotearlo con legenda m
mapa_ISMT09 +  labs(title ="Temuco",
                    subtitle = NULL,
                    caption = NULL) 


# Unir a base personas ----------------------------------------------------

data_gse <- data_hog_summ %>% select(manzent, ISMT_mzn, ISMTptj_zc, GSE_ISMT_mzn, GSE_ISMT_zc)
data_gse_full <- data_clean %>% left_join(data_gse, by = "manzent")

# Save datasets ----------------------------------------------------

# geo_r13 %>%  st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles_60/ISMT_Santiago/ISMT_Stgo.shp", quiet = TRUE,  delete_layer = TRUE)
# geo_r08 %>%  st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles_60/ISMT_Conce/ISMT_Conce.shp", quiet = TRUE,  delete_layer = TRUE)
#geo_r09 %>%  st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shape_Temuco/ISMT_Temuco.shp", quiet = TRUE,  delete_layer = TRUE)
data_gse_full %>% saveRDS("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/15_TesisRT/Data/ISMT2012_R13.Rds")

# test13 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles_60/ISMT_Santiago")
# test8 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles_60/ISMT_Conce")
# test4 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles/ISMT_Serena")
