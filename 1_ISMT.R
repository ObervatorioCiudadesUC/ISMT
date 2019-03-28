# Nombre Programa: 1_ISMT.R
# Ubicacion: GitHub/ISMT
# Autor: Monica Flores
# Fecha Creacion: 23/01/2018
# Proyecto: ISMT
# Objetivo: Calcular Indice Sociomaterial Territorial
# Output: C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output
# Notas:

# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)
library(sf)


# Instrucciones -----------------------------------------------------------

# Para correr este script se necesitan: 
# RDS de ka base censal procesada en el archivo 0_HomologacionDatosCenso
# Shapefile de zonas censales (o manzanas) dentro del Área Urbana Consolidada (AUC) o ciudad
# en caso de correrlo para una ciudad en específico.

# Se debe correr un área geográfica a la vez, definiendo la region y año censal en el paso 0. 
# Además en el paso 0 se debe definir el nombre y ubicacion del archivo AUC.

# Importante: correr el script hasta el paso 3 y detenerse. 
# Luego correr script 2_homals y redefinir los coeficientes en el paso 4.
# Luego correr el script hasta el final.


# 0. Definiciones iniciales --------------------------------------------------


# Setear directorio local - Cambiar el nombre a la ubicacion de la carpeta de trabajo personal
dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018" # Directorio local
ismt_dir <- glue("{dir_loc}/13_ISMT") # Directorio ISMT
censo_dir <- glue("{dir_loc}/14_Microdatos/Clean_Personas") # Directorio datos censo


# Definir region y año censal a analizar
reg <- 
yyyy <- 2017
AUC <- "AUC_Stgo_2012/AUC_Stgo_2012.shp" #Definir nombre archivo Zonas Censales AUC individual

# 1. Leer datos ------------------------------------------------------------

# Leer archivo Censo variables homogeneas
data <- readRDS(glue("{censo_dir}/Censo{yyyy}_Persona_Clean_R{reg}.Rds")) %>%
  filter(year==yyyy) # Elegir año

# Leer shape Area Urbana Consolidada - acotar el analisis al area definida
geo_r <- st_read(glue("{censo_dir}/Shapefiles/{AUC}")) %>% 
  # Censo 2012 necesita rehacer manzent y geocode - no correr para otros años
  transmute(
    manzent = (CUT*1000000000) + (DISTRITO*10000000) + (1*1000000) + (ZONA*1000) + MANZANA, # Rehacer Manzent
    geocode = (CUT*1000000) + (DISTRITO*10000) + (1*1000) + (ZONA)  # Crear codigo zona
  ) 

# Inner join shape con data censo
data <- data %>%  inner_join(as.data.frame(geo_r), by =c("geocode")) %>% select(-geometry)

# 2. Pre-calculo datos ISMT nivel hogar --------------------------------------------------

data_clean <- data %>% 
  filter(parentesco == 1) %>% # Filtrar solo jefes de hogar
  mutate(
      year = year,  
      region = region,
      comuna = comuna,
      geocode = geocode,
      manzent = manzent,
      folio = folio,
      nviv = nviv,
      nhogar = nhogar,
      hogar = 1, # Dummy Hogar
      esc_jh = escolaridad, # Años escolaridad Jefe de Hogar
      n_hog_alleg = (cant_hog - 1), # allegamiento
      ind_hacinam = case_when( # Indice Hacinamiento (variables a nivel vivienda)
        n_dormitorios >= 1 ~ cant_per/n_dormitorios,
        # Indice para 0 dormitorios se elige de tal manera que: 
        # 1 persona viviendo en un estudio no presenta hacinamiento, 2 personas hacinamiento medio y más de 3, hacinamiento critico
        n_dormitorios == 0 ~ cant_per*2
      ),
      # Indice materialidad
      ind_mater = cond_muro + cond_cubierta + cond_suelo, # Puntaje va de 3 a 9, donde 3 es todo irrecuperable y 9 es todo aceptable 
      # Hacinamiento categorias
      hacin_medio = if_else(ind_hacinam >=2.5 & ind_hacinam < 5, 1L, 0L),
      hacin_critico = if_else(ind_hacinam >= 5, 1L, 0L),
      hacin_med_crit = (hacin_medio + hacin_critico), # Total viviendas con hacinamiento medio o critico
      # Materialidad categorias
      mat_aceptable   = if_else(cond_muro == 3 & cond_cubierta == 3 & cond_suelo == 3, 1L, 0L), # Las 3 condiciones aceptables
      mat_irrecup     = if_else(cond_muro == 1 | cond_cubierta == 1 | cond_suelo == 1, 1L, 0L), # Al menos 1 condicion irrecuperable
      mat_recuperable = if_else(mat_aceptable == 0 & mat_irrecup == 0, 1L, 0L)
  ) %>% 
  # Pre-Calculo ISMT
  mutate(
    # Hacer negativos aquellos índices inversos
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
  ) 

# Guardar Dataset nivel hogar - ISMT
data_clean %>% saveRDS(glue("{ismt_dir}/Output/Censo{yyyy}_Hogar_ISMT_R{reg}.Rds"))


# 3. Calculo Homals (script 2_) ----------------------------------------------

####### Calcular homals GitHub/ISMT/2_homals.R #########

# Descomentar y leer archivo si se parte desde aquí 
# data_clean <- readRDS(glue("{ismt_dir}/Output/Censo{yyyy}_Hogar_ISMT_R{reg}.Rds"))

# 4. Definir coeficientes ------------------------------------------------------------

# Redefinir coeaficientes Homals segun resultado script

# Coeff homals RM - ISMT 2017
Esc <- 0.2783314  ### Escolaridad Puntaje
Viv <- 0.2867274  ### Calidad Vivienda
Hac <- 0.3271089  ### Hacinamiento
All <- 0.2522210  ### Allegamiento

# # Coeff homals RM - ISMT 2012
# Esc <- 0.3333650  ### Escolaridad Puntaje
# Viv <- 0.3043160  ### Calidad Vivienda
# Hac <- 0.3233621  ### Hacinamiento
# All <- 0.1813503  ### Allegamiento

# 5. Calculo ISMT y GSE hogar ------------------------------------------------------------

# Calcular puntaje ISMT - pesos según homals
data_clean <- data_clean %>%
  mutate(
    ptje_raw = (ptje_esc*Esc) + (ptje_hacin*Hac) + (ptje_mater*Viv) + (ptje_alleg*All),
    ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
  )

# Calcular centiles
quant <- quantile(data_clean$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()

# Definir GSE - según cortes INE
E <- quant["6%",]
D <- quant["36%",]
C3 <- quant["64%",]
C2 <- quant["79%",]
ABC1 <- quant["100%",]

# Calcular GSE persona
data_clean <- data_clean %>% 
  filter(!is.na(ptje_ISMT)) %>% 
  mutate(
    GSE_pers = case_when(
      ptje_ISMT <= E ~ "E",
      ptje_ISMT > E & ptje_ISMT <= D ~ "D",
      ptje_ISMT > D & ptje_ISMT <= C3 ~ "C3",
      ptje_ISMT > C3 & ptje_ISMT  <= C2 ~ "C2",
      ptje_ISMT > C2  ~ "ABC1"
    )
  )  

# 6. Summarise datasets ------------------------------------------------------

# Numero de personas por GSE por manzana
data_GSE_mzn <- data_clean %>% 
  group_by(manzent, GSE_pers) %>%
  summarise(n = n()) %>% 
  spread(GSE_pers, n)

# ISMT y GSE medio por manzana
data_hog_summ <- data_clean %>% 
  group_by(year, region, comuna, geocode, manzent) %>% # Eliminar manzent de la agrupación si se necesita por ZC (2017)
  summarise(
    n_hog = n(),
    esc_jh = mean(esc_jh, na.rm = TRUE),
    ind_hac = mean(ind_hacinam, na.rm = TRUE),
    ind_mat = mean(ind_mater, na.rm = TRUE),
    h_all = sum(n_hog_alleg[nhogar == 1], na.rm = TRUE),
    h_hac_m = sum(hacin_medio, na.rm = TRUE),
    h_haccrit = sum(hacin_critico, na.rm = TRUE),
    hac_m_crit = sum(hacin_med_crit, na.rm = TRUE),
    mat_acep = sum(mat_aceptable, na.rm = TRUE),
    mat_irr = sum(mat_irrecup, na.rm = TRUE),
    mat_recp = sum(mat_recuperable, na.rm = TRUE),
    ISMT_mzn = median(ptje_ISMT, na.rm = TRUE) # Calculo mediana ISMT manzana
  ) %>%
  mutate(
    GSE_mzn = case_when(
      ISMT_mzn <= E ~ "E",
      ISMT_mzn > E & ISMT_mzn <= D ~ "D",
      ISMT_mzn > D & ISMT_mzn <= C3 ~ "C3",
      ISMT_mzn > C3 & ISMT_mzn  <= C2 ~ "C2",
      ISMT_mzn > C2  ~ "ABC1"
    )
  ) %>% ungroup() %>% 
  left_join(data_GSE_mzn, by = "manzent")


# Plotear shapes ----------------------------------------------------------

# Unir info a shapes
geo_r <- geo_r %>% left_join(data_hog_summ, by = "manzent") 

# Plotear mapa con color por puntaje ISMT
mapa_ISMT <- ggplot(data = geo_r, aes(fill = ISMTptj)) +
  geom_sf(lwd = 0) +
  scale_fill_gradient(name = "Puntaje ISMT",
                      low = "#d53e4f",
                      high = "#084594",
                      labels = scales::percent,
                      breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# Plotearlo con legenda
mapa_ISMT +  labs(title ="ISMT Santiago",
                     subtitle = NULL,
                     caption = NULL)


# Unir a base personas ----------------------------------------------------

# Seleccionar solo ISMT y GSE manzana 
# eliminar el select() si se necesitan todas las variables
data_gse <- data_hog_summ %>% select(manzent, ISMT_mzn, GSE_mzn) 

# Unir GSE mzn a base personas
data_gse_full <- data_clean %>% left_join(data_gse, by = "manzent")

# Save datasets ----------------------------------------------------

data_gse_full %>% saveRDS(glue("{ismt_dir}/Data/Output/ISMT{yyyy}_R{reg}.Rds"))

geo_r %>% st_write(glue("{ismt_dir}/Output/Shapefiles/shape_ISMT{yyyy}_R{reg}/shape_ISMT{yyyy}_R{reg}.shp")) 

