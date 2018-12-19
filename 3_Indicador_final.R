# Nombre Programa: Ponderacion Homnals
# Ubicacion: GitHub/ISMT
# Autor: Ricardo Truffello
# Fecha Creacion: 19/12/2018
# Proyecto: ISMT
# Objetivo: Calcular Indice Sociomaterial 
# Output: C:/Users/Ricardo Truffello/Desktop  ### LOL :D
# Notas:

# Sacar notacion cientifica
options(scipen=999)

# install.packages (c("tidyverse","glue", "sf"))
library(tidyverse)
library(glue)
library(sf)

dir_loc <- "C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/13_ISMT"
Base_RM <- readRDS(glue("{dir_loc}/Output/Censo2017_ISMT_hogares_v2.Rds"))

#Eigenvalues Homals
# Variable Loadings:
#   D1
# ptje_esc   -0.3999909
# ptje_hacin -0.2686852
# ptje_mater  0.4118807

Esc <- 0.3999909  ### Escolaridad Puntaje
Viv <- 0.4118807  ### Calidad Vivienda
Hac <- 0.2686852  ### Hacinamiento

indic_sel <- Base_RM %>%  
  ### Generacion de Codigo Zona Censal
  mutate(geocode = (comuna*1000000)+(dc*10000)+(1*1000)+(zc_loc),
         geocode = as.character(geocode)) %>% 
  select(geocode, ptje_esc, ptje_hacin, ptje_mater)

#Eliminar NA
indic_sel <- na.omit(indic_sel)

#Ponderacion Puntajes Homals
indic_sel <- indic_sel %>% 
  mutate(
    ptje_final = ((ptje_esc*Esc) + (ptje_hacin*Hac) + (ptje_mater*Viv)),
    ptje_fnorm = (ptje_final-min(ptje_final))/(max(ptje_final)-min(ptje_final))
  )

summary(indic_sel)
# -------------------ptje_esc        ptje_hacin        ptje_mater       ptje_final       ptje_fnorm    
# Length:2091913     Min.   :   0.0   Min.   :  27.03   Min.   :   0.0   Min.   : 257.8   Min.   :0.0000  
# Class :character   1st Qu.: 428.6   1st Qu.: 977.61   1st Qu.:1000.0   1st Qu.: 830.6   1st Qu.:0.6961  
# Mode  :character   Median : 571.4   Median : 982.12   Median :1000.0   Median : 903.7   Median :0.7851  
# -------------------Mean   : 563.3   Mean   : 981.52   Mean   : 978.5   Mean   : 892.1   Mean   :0.7709  
# -------------------3rd Qu.: 714.3   3rd Qu.: 986.62   3rd Qu.:1000.0   3rd Qu.: 962.7   3rd Qu.:0.8567  
# -------------------Max.   :1000.0   Max.   :1000.00   Max.   :1000.0   Max.   :1080.6   Max.   :1.0000 


### Definicion Quantiles
quantile(indic_sel$ptje_fnorm, probs = seq(0, 1, 0.05)) ### Esta dando super mal la distribucion :/ - sigue mal :(
#40% = 0.7784421

# Asignar percentil 40 a variable
pct40 <- quantile(indic_sel$ptje_fnorm, probs = 0.4) 

# Plotear histograma
hist1 <- indic_sel %>% 
  ggplot(aes(ptje_fnorm)) + 
  geom_histogram(color = "grey", fill = "navy", lwd=0.1, binwidth = 0.01)
hist1 + labs(x = "Puntaje Normalizado", y = "Frecuencia",
             title ="Histograma ISMT",
             subtitle = "a Nivel de Hogar en la RM",
             caption = NULL)

### Determinacion de 40% mas vulnerable
indic_sel <- indic_sel %>% mutate(
  # determinar quantil 0.4
  pctrank = percent_rank(ptje_fnorm),
  ptje_min40 = if_else(ptje_fnorm <= pct40, 1, 0),
  hogar = 1
)

sum_hog <- indic_sel %>% summarise(
  hog_40pct = sum(ptje_min40),
  hogares = sum(hogar)
) %>% 
  mutate(
    pct_vuln = hog_40pct/hogares
    # Da 40.5% :) 
  )

# Summary by geocode
indic_ZC <- indic_sel %>% group_by(geocode) %>% summarise(
  hog_40pct = sum(ptje_min40),
  hogares = sum(hogar)
) %>% 
  mutate(
    pct_hog40pvuln = hog_40pct/hogares
  )



# Unir a shapefile --------------------------------------------------------

#RM zonas censales
data_geo_precios <- glue("{dir_loc}/Shapefiles/precios") %>% 
  st_read()  %>%
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(geocodigo)) %>% 
  ## Unir datos escolaridad media ZC - data full   
  left_join(indic_sel, by = "geocode")

#RM zonas censales
data_geo_ZC <- glue("{dir_loc}/Shapefiles/precios") %>% 
  st_read()  %>%
  rename_all(tolower) %>% 
  mutate(
    geocode = as.character(geocodigo)) %>% 
  ## Unir datos escolaridad media ZC   
  left_join(indic_ZC, by = "geocode")

# Plotear mapa con color por pct hogares vulnerables
mapa_vuln <- ggplot(data = data_geo_ZC, aes(fill = pct_hog40pvuln)) + 
  geom_sf(lwd = 0) +
  scale_fill_gradient(name = "Porcentaje Hogares\nVulnerables", 
                      low = "#084594", 
                      high = "#d53e4f",
                      labels = scales::percent
  ) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) 
# Plotearlo con legenda más grande
mapa_vuln + guides(shape = guide_legend(override.aes = list(size = 10)))


### Exportar
write.csv2(Base_RM, file = "C:/Users/Ricardo Truffello/Desktop/Censo2017_ISMT_hogares.csv")
saveRDS(Base_RM, file = "C:/Users/Ricardo Truffello/Desktop/Censo2017_ISMT_hogares.rds",ascii=F,compress=T,refhook=NULL)

# Guardar shape 
data_geo_ZC %>% st_write(glue("{dir_loc}/Output/Shapefiles/shape_ISMT/ISMT_ZC.shp"))
test <-  st_read(glue("{dir_loc}/Output/Shapefiles/shape_ISMT/ISMT_ZC.shp"))
# Exportar bases
data_ZC <- as.data.frame(data_geo_ZC) %>% select(-geometry) 
data_hogar <- as.data.frame(data_geo_precios) %>% select(-geometry) 

data_ZC %>% write.csv2(glue("{dir_loc}/Output/data_ZC.csv"))
data_hogar %>% write.csv2(glue("{dir_loc}/Output/data_hogar_.csv"))

data_geo_precios
data_geo_ZC
