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
ismt_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Historico/Output"

# aumentar limite memoria - usando pendrive
memory.limit(16000)


# Leer archivo Censo RM vars homogeneas -----------------------------------------

r <- 13

data_rm <- readRDS(glue("{out_dir}/Censo1992_2017_Persona_CleanList_R{r}.Rds")) 

# Nuevas variables nivel vivienda --------------------------------------------------

# Coeff homals - ISMT 2017 (actualizar)
Esc <- 0.2783314  ### Escolaridad Puntaje
Viv <- 0.2867274  ### Calidad Vivienda
Hac <- 0.3271089  ### Hacinamiento
All <- 0.2522210  ### Allegamiento

# Calculo data nivel hogar
data_rm_hog <- map2(data_rm, names(data_rm), 
                    ~dplyr::filter(.x, parentesco == 1)) %>%  
  map2_df(names(data_rm),
          ~dplyr::transmute ( .x,
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
          )
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
  ) %>% 
  # Calculo ISMT - pesos según homals
  mutate(
    ptje_raw = ((ptje_esc*Esc) + (ptje_hacin*Hac) + (ptje_mater*Viv) + (ptje_alleg*All)),
    ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
  ) %>% 
  select(-z_ptje_esc, -z_ptje_hacin, -z_ptje_mater, -z_ptje_alleg, -ptje_raw)

# ISMT Asignar percentil 40 a variable - esto tiene que ser por año
pct40 <- data_rm_hog %>% group_by(year) %>% summarise(
  pct40ISMT = quantile(ptje_ISMT, probs = 0.4, na.rm = TRUE) 
)

# Determinacion de 40% mas vulnerable por año
data_rm_hog <- data_rm_hog %>% 
  left_join(pct40, by="year") %>%
  mutate(
    # pctrank_ISMT = percent_rank(ptje_ISMT), # esto tiene ser por año
    hog40vuln_ISMT = if_else(ptje_ISMT <= pct40ISMT, 1L, 0L) # determinar si vivienda pertenece a quantil 0.4 anual
  )

head(data_rm_hog)  

summary(data_rm_hog$ptje_ISMT)

# Test ISMT ---------------------------------------------------------------

# Plotear histograma ISMT
hist1 <- as.data.frame(data_rm_hog) %>% 
  ggplot(aes(ptje_ISMT)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.01) +
  facet_grid(. ~ year)
hist1 + labs(x = "Puntaje Normalizado", y = "Frecuencia",
             title ="Histograma ISMT",
             subtitle = "a Nivel de Hogar en la RM",
             caption = NULL)

# Chequear que porcentaje de la poblacionb de hogares está en el 40% mas vulnerable
test40pct <- data_rm_hog %>% group_by(year) %>% 
  summarise(
    hog_40pct = sum(hog40vuln_ISMT, na.rm = TRUE),
    tot_hog = sum(hogar, na.rm = TRUE)
  ) %>% 
  mutate(
    pct_vuln = hog_40pct/tot_hog # Da 40.00% :) 
  )

# Summarise datasets ------------------------------------------------------

# Summary info hogar (ISMT)
data_rm_hog_summ <- data_rm_hog %>% 
  group_by(year, region, comuna, geocode, manzent) %>% 
  summarise(
    n_hog = sum(hogar),
    med_esc_jh = median(esc_jh, na.rm = TRUE),
    mean_esc_jh = mean(esc_jh, na.rm = TRUE),
    med_ind_hacinam = median(ind_hacinam, na.rm = TRUE),
    mean_ind_hacinam = mean(ind_hacinam, na.rm = TRUE),
    med_ind_mater = median(ind_mater, na.rm = TRUE),
    mean_ind_mater = mean(ind_mater, na.rm = TRUE),
    hog_alleg = sum(n_hog_alleg[nhogar == 1], na.rm = TRUE),
    hog_hacin_medio = sum(hacin_medio, na.rm = TRUE),
    hog_hacin_critico = sum(hacin_critico, na.rm = TRUE),
    hog_hacin_med_crit = sum(hacin_med_crit, na.rm = TRUE),
    hog_mat_aceptable = sum(mat_aceptable, na.rm = TRUE),
    hog_mat_irrecup = sum(mat_irrecup, na.rm = TRUE),
    hog_recuperable = sum(mat_recuperable, na.rm = TRUE),
    # ISMT
    hog_40pct_ismt = sum(hog40vuln_ISMT, na.rm = TRUE), # Suma de viviendas bajo el 40% mas vulnerable
    prom_ismt = mean(ptje_ISMT, na.rm = TRUE)
    # prom_rank = mean(pctrank_ISMT)
  ) %>% ungroup() %>% 
  mutate(
    pct_hog_vuln = hog_40pct_ismt/n_hog 
  )

# Save datasets ----------------------------------------------------

# Dataset nivel hogar - ISMT
data_rm_hog %>% saveRDS(glue("{ismt_dir}/Censo1992_2017_Hogar_ISMT_R{r}.Rds"))
data_rm_hog_summ %>% saveRDS(glue("{ismt_dir}/Censo1992_2017_Agregado_ISMT_R{r}.Rds"))


# Incorporar shapefiles  --------------------------------------------------


