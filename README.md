# ISMT
Índice Socio-Material y Territorial

# Instrucciones -----------------------------------------------------------

Para correr este script se necesitan: 
- RDS de la base censal procesada en el archivo 0_HomologacionDatosCenso (elegir el año de interés).
- Shapefile de zonas censales (o manzanas) dentro del Área Urbana Consolidada (AUC) o ciudad en caso de correrlo para una ciudad en específico.

Se debe correr un área geográfica a la vez, definiendo la region y año censal en el paso 0. Además en el paso 0 se debe definir el nombre y ubicacion del archivo AUC.

Importante: correr el script hasta el paso 3 y detenerse. 
Luego correr script 2_homals y redefinir los coeficientes en el paso 4.
Luego volver a 1_ISMT y correr el script desde el paso 4 hasta el final.
