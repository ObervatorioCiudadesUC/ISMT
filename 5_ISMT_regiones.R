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
ismt_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Output"
auc_dir <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/02_Insumos/Zonas Censales AUC_2017"

# aumentar limite memoria - usando pendrive
# memory.limit(16000)


# Leer archivo Censo  vars homogeneas -----------------------------------------

# Regiones 04 y 08
data <- readRDS(glue("{in_dir}/Data Requests/Censo2017_r04_r08_ISMT.Rds")) %>% mutate(as.integer(region)) %>% 
  split(.$region)

# Leer shapes Area Urbana Consolidada
# geo_r04 <- st_read(glue("{auc_dir}/R04_LaSerena")) %>% rename_all(tolower) %>% select(geocodigo, geometry)
geo_r08 <- st_read(glue("{auc_dir}/R08_Concepcion")) %>% rename_all(tolower) %>% select(geocodigo, geometry)
geo_r08 <- st_read(glue("{auc_dir}/R08_Concepcion")) %>% rename_all(tolower) %>% select(geocodigo, geometry)

# Inner join shape con data censo
data[['4']] <- data[['4']] %>%  inner_join(as.data.frame(geo_r04), by = c("geocode"="geocodigo")) %>% select(-geometry)
data[['8']] <- data[['8']] %>%  inner_join(as.data.frame(geo_r08), by = c("geocode"="geocodigo")) %>% select(-geometry)

# Nuevas variables nivel vivienda --------------------------------------------------

# Calculo data nivel hogar
data_clean <- map2(data, names(data), 
                    ~dplyr::filter(.x, parentesco == 1)) %>%  
  map2_df(names(data),
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
  split(.$region)

# Guardar Dataset nivel hogar - ISMT
data_clean %>% saveRDS(glue("{ismt_dir}/Censo2017_Hogar_ISMT_R04_R08.Rds"))

####### Calcular homals

# leer archivo si se parte desde aquí 
data_clean <- readRDS(glue("{ismt_dir}/Censo2017_Hogar_ISMT_R04_R08.Rds"))

# Calculo ISMT ------------------------------------------------------------

# Coeff homals R04 - ISMT 2017 
Esc4 <- 0.2808857  ### Escolaridad Puntaje
Viv4 <- 0.2996181  ### Calidad Vivienda
Hac4 <- 0.3224852  ### Hacinamiento
All4 <- 0.2520207  ### Allegamiento

# Coeff homals R08 - ISMT 2017 
Esc8 <- 0.3169055  ### Escolaridad Puntaje
Viv8 <- 0.3298117  ### Calidad Vivienda
Hac8 <- 0.2881263  ### Hacinamiento
All8 <- 0.1012507  ### Allegamiento

# Calcular puntaje ISMT Serena
data_clean[['4']] <- data_clean[['4']] %>% 
  # Calculo ISMT - pesos según homals
  mutate(
    ptje_raw = (ptje_esc*Esc4) + (ptje_hacin*Hac4) + (ptje_mater*Viv4) + (ptje_alleg*All4),
    ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
  ) 

# Calcular puntaje ISMT Concepcion
data_clean[['8']] <- data_clean[['8']] %>% 
  # Calculo ISMT - pesos según homals
  mutate(
    ptje_raw = (ptje_esc*Esc8) + (ptje_hacin*Hac8) + (ptje_mater*Viv8) + (ptje_alleg*All8),
    ptje_ISMT = (ptje_raw-min(ptje_raw, na.rm = TRUE))/(max(ptje_raw, na.rm = TRUE)-min(ptje_raw, na.rm = TRUE)) # puntaje ISMT normalizado
  ) 

# Calcular centiles para GSE ----------------------------------------------

# Calcular centiles
quant_R04 <- quantile(data_clean[['4']]$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()
quant_R08 <- quantile(data_clean[['8']]$ptje_ISMT, probs = seq(0, 1, 0.01), na.rm=TRUE) %>% as.data.frame()

rownames(quant_R04)

# Determinar tope centil GSE x ciudad ( x <= R04_E) ( x > R04_E & x <= R04_D )
R04_E <- quant_R04["6%",]
R04_D <- quant_R04["36%",]
R04_C3 <- quant_R04["64%",]
R04_C2 <- quant_R04["79%",]
R04_ABC1 <- quant_R04["100%",]

R08_E <- quant_R08["6%",]
R08_D <- quant_R08["36%",]
R08_C3 <- quant_R08["64%",]
R08_C2 <- quant_R08["79%",]
R08_ABC1 <- quant_R08["100%",]

# Determinar 40% más vulnerable -------------------------------------------

# Unir lista
data_clean <- map2_df(data_clean, names(data_clean), 
                    ~dplyr::select(.x, -z_ptje_esc, -z_ptje_hacin, -z_ptje_mater, -z_ptje_alleg, -ptje_raw))

# ISMT Asignar percentil 40 a variable - por region
pct40 <- data_clean %>% group_by(region) %>% summarise(
  pct40ISMT = quantile(ptje_ISMT, probs = 0.4, na.rm = TRUE) 
)

# Determinacion de 40% mas vulnerable por año
data_hog <- data_clean %>% 
  left_join(pct40, by="region") %>%
  mutate(
    # pctrank_ISMT = percent_rank(ptje_ISMT), # esto tiene ser por año
    hog40vuln_ISMT = if_else(ptje_ISMT <= pct40ISMT, 1L, 0L), # determinar si vivienda pertenece a quantil 0.4 anual
    # Agregar nombre ciudad
    ciudad = if_else(region == 4, "La Serena-Coquimbo", "Concepcion")
  )

head(data_hog)  

summary(data_hog$ptje_ISMT)

# Test ISMT ---------------------------------------------------------------

# Plotear histograma ISMT
hist1 <- as.data.frame(data_hog) %>% 
  ggplot(aes(ptje_ISMT)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.01) +
  facet_grid(. ~ ciudad)
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

# Summary info hogar (ISMT)
data_hog_summ <- data_hog %>% 
  group_by(year, region, comuna, geocode, manzent) %>% 
  summarise(
    n_hog = sum(hogar),
    # med_esc_jh = median(esc_jh, na.rm = TRUE),
    esc_jh = mean(esc_jh, na.rm = TRUE),
    # ind_hac = median(ind_hacinam, na.rm = TRUE),
    ind_hac = mean(ind_hacinam, na.rm = TRUE),
    # ind_mater = median(ind_mater, na.rm = TRUE),
    ind_mat = mean(ind_mater, na.rm = TRUE),
    h_all = sum(n_hog_alleg[nhogar == 1], na.rm = TRUE),
    h_hac_m = sum(hacin_medio, na.rm = TRUE),
    h_haccrit = sum(hacin_critico, na.rm = TRUE),
    hac_m_crit = sum(hacin_med_crit, na.rm = TRUE),
    mat_acep = sum(mat_aceptable, na.rm = TRUE),
    mat_irr = sum(mat_irrecup, na.rm = TRUE),
    mat_recp = sum(mat_recuperable, na.rm = TRUE),
    # ISMT
    h_40pct = sum(hog40vuln_ISMT, na.rm = TRUE), # Suma de viviendas bajo el 40% mas vulnerable
    ISMTptj = mean(ptje_ISMT, na.rm = TRUE)
    # prom_rank = mean(pctrank_ISMT)
  ) %>% ungroup() %>% 
  mutate(
    pct_vuln = h_40pct/n_hog,
    GSE_ave = case_when(
      # La Serena
      region == 4 & ISMTptj <= R04_E ~ "E",
      region == 4 & ISMTptj > R04_E & ISMTptj <= R04_D ~ "D",
      region == 4 & ISMTptj > R04_D & ISMTptj <= R04_C3 ~ "C3",
      region == 4 & ISMTptj > R04_C3 & ISMTptj  <= R04_C2 ~ "C2",
      region == 4 & ISMTptj > R04_C2 & ISMTptj  <= R04_ABC1 ~ "ABC1",
      # Concepcion
      region == 8 & ISMTptj <= R08_E ~ "E",
      region == 8 & ISMTptj > R08_E & ISMTptj <= R08_D ~ "D",
      region == 8 & ISMTptj > R08_D & ISMTptj <= R08_C3 ~ "C3",
      region == 8 & ISMTptj > R08_C3 & ISMTptj  <= R08_C2 ~ "C2",
      region == 8 & ISMTptj > R08_C2 & ISMTptj  <= R08_ABC1 ~ "ABC1"
    )
  ) %>% 
  as.data.frame() %>% 
  split(.$region)


# Unir info a shapes
geo_r04 <- geo_r04 %>% left_join(data_hog_summ[['4']], by = c("geocodigo"="geocode"))
geo_r08 <- geo_r08 %>% left_join(data_hog_summ[['8']], by = c("geocodigo"="geocode"))

# Plotear shapes ----------------------------------------------------------

# Plotear mapa con color por puntaje ISMT

# La Serena
mapa_ISMT04 <- ggplot(data = geo_r04, aes(fill = ISMTptj)) + 
  geom_sf(lwd = 0) +
  scale_fill_gradient(name = "Puntaje ISMT", 
                      low = "#d53e4f", 
                      high = "#084594",
                      labels = scales::percent,
                      breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# Plotearlo con legenda m
mapa_ISMT04 +  labs(title ="ISMT La Serena-Coquimbo",
                     subtitle = NULL,
                     caption = NULL) 

# Concepcion
mapa_ISMT08 <- ggplot(data = geo_r08, aes(fill = ISMTptj)) + 
  geom_sf(lwd = 0) +
  scale_fill_gradient(name = "Puntaje ISMT", 
                      low = "#d53e4f", 
                      high = "#084594",
                      labels = scales::percent,
                      breaks = c(0.7, 0.75, 0.8, 0.85, 0.9)
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# Plotearlo con legenda m
mapa_ISMT08 +  labs(title ="Concepción",
                    subtitle = NULL,
                    caption = NULL) 

# Save datasets ----------------------------------------------------

geo_r04 %>%  st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles/ISMT_Serena/ISMT_Serena.shp", quiet = TRUE,  delete_layer = TRUE)
geo_r08 %>%  st_write("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles/ISMT_Conce/ISMT_Conce.shp", quiet = TRUE,  delete_layer = TRUE)

test4 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles/ISMT_Serena")
test8 <- st_read("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT/Regiones/Output/Shapefiles/ISMT_Conce")
