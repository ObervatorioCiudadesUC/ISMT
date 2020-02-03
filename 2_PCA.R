# Código PCA

# Analisis de componentes principales
base <- base_pca_std %>% select(ptje_esc, ptje_hacin, ptje_mater, ptje_alleg)
pca <- prcomp(base)
pca
summary(pca) 

# PCA con valores absolutos coeficientes
pc <- pca$rotation %>% as.data.frame() %>% 
  mutate(
    variable = row.names(pca$rotation)
  ) %>% 
  mutate_at(
    vars(PC1:PC4), abs # Valor absoluto cada coefieciente
  )

row.names(pc) <- pc$variable


# Coeficientes por variable CL 
variables <- colnames(base) # Definir variables 
coefs <-  data.frame() # Definir base coefs

for (var in variables) {
  
  variable <- var
  coef <- (pc[[glue("{var}"), 'PC1']] * 0.8422) + (pc[[glue("{var}"), 'PC2']] * 0.1493) + (pc[[glue("{var}"), 'PC3']] * 0.00598) +
    (pc[[glue("{var}"), 'PC4']] * 0.0253) 
  
  data_merge <- data.frame(variable, coef)
  
  coefs <- bind_rows(coefs, data_merge)
}
