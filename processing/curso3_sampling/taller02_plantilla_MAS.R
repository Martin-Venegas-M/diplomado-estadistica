library(samplingbook)
library(readxl)
library(dplyr)

# Tamaño de muestra 

# Requerimiento:
# Nos piden estimar el rendimiento con un error no superior a 3 puntos y que 
# la proporción de establecimientos en condición de riesgo (rendimiento en SIMCE < 200)
# estimada presente un error menor o igual al 2%.

# Material:
# Se dispone del listado de los RBD de los 3632 establecimientos

# Antecedentes previos:
# Un 6% de los establecimientos estaban en condición de riesgo.
# Rendimiento fue levemente mayor a 250 puntos con una desviación 
# estándar de 30 puntos.

# Calculamos tamaño de la muestra en ambos casos

sample.size.mean(e = 3, S = 30, N = 3632)
sample.size.prop(e = 0.02, P = 0.06, N = 3632)
n = 542

# Elegimos el mayor numero de muestra entre los dos resultados.

# Muestreo

# Seleccion de la muestra - RBD.xlsx
RBD <- readxl::read_excel("Bases de datos/Establecimientos.xlsx")
head(RBD)
nrow(RBD)
table(RBD$Region)

N <- nrow(RBD)

set.seed(1010)
muestraRBD <- RBD %>% sample_n(n)
table(muestraRBD$Region)

# Tabulacion - uso comandos Smean y Sprop

# Rendimiento SIMCE
# Promedio MAT y su ErrEst
Smean(muestraRBD$MAT, N = N) # Rendimiento SIMCE Prom y ErrEst

# Establecimientos en Riesgo
# Proporcion en riesgo
Sprop(muestraRBD$MAT < 200, N = N) # Establecimientos en riesgo Prop ErrEst

# Estimaciones regionales 

# Rendimiento SIMCE
# Promedio MAT y su ErrEst
muestraRBD %>% 
  group_by(Region) %>% 
  summarise(promedio = mean(MAT), 
            sd = sd(MAT),
            n = n(),
            se = sd/sqrt(n)) # obtenemos errores estándar de las estimaciones 
                             # sd_i/raíz(n_i)

# Establecimientos en Riesgo
# Proporcion en riesgo
muestraRBD <- muestraRBD %>% 
  mutate(riesgo = ifelse(MAT < 200, 1, 0))
head(muestraRBD, 20)

muestraRBD %>% 
  group_by(Region) %>% 
  summarise(promedio = mean(riesgo), 
            sd = sd(riesgo),
            n = n(),
            se = sd/sqrt(n))



