###############
### Clase . ###
###############

library(dplyr)
library(readxl)
library(car)

base = read_excel("input/data/original/01_Actividad_de_regresion_Lineal_Multiple.xlsx")
head(base)
tail(base)
colSums(is.na(base))
# datos completos, no tuvimos la necesidad de eliminar observaciones

## Partamos haciendo un analisis descriptivo ##
## Partamos haciendo boxplots ##
par(bty = "n")
boxplot(HG ~ SEXO, data = base, las = 1,
        border = "darkred")
boxplot(HG ~ TRATAMIENTO, data = base, las = 1,
        border = "darkred")
boxplot(HG ~ DEFORMIDAD, data = base, las = 1,
        border = "darkred")

## Hagamos Graficos de Dispersion ##
par(mfrow = c(1,3))
plot(HG ~ PESO, data = base, bty = "n", las = 1, pch = 20)
plot(HG ~ GRASA, data = base, bty = "n", las = 1, pch = 20)
plot(HG ~ LARGO, data = base, bty = "n", las = 1, pch = 20)
dev.off()

# OJO! Tenemos una observacion al parecer "muy atipica"

# empezamos del modelo seleccion por backward con todas las observaciones
Mod1 <- lm(HG ~ PESO + GRASA,
           data = base)
summary(Mod1)


# Puntos anomalos o atipicos ####
# datos mal ajustados


# prueba de bonferroni
# H0: i-esima observacion no es un outlier
# H1: i-esima observacion es un outlier

# para nuestro caso_ tenemos 200 observaciones -> i = 1,...,200

car::outlierTest(Mod1)

# Rechamos H0, las observaciones 1, 61 y 151 son posibles datos anomalos

# revisaremos nuestros datos
summary(base)

# revisamos los posibles datos anomalos
# tenemos datos completos? si
# -> no, base <- na.omit(base %>% select(HG, PESO, GRASA))
base %>% slice(1, 61, 151)  # -> extraer las filas
base[c(1,61,151), ] # [filas, columnas]

# en 1: tenemos valores aparentemente altos 
# en 61: tenemos valores aparentemente bajos en PESO Y GRASA
# en 151: tenemos valores altos en PESO y no en GRASA

# eliminamos y estudiamos como cambia el modelo

base_filtrada <- base %>% slice(-1, -61, -151)

# vemos cambia el n
nrow(base)
nrow(base_filtrada)

Mod1_1 <- lm(HG ~ PESO + GRASA,
             data = base_filtrada)


# como cambia el modelo
summary(Mod1)

summary(Mod1_1)

rm(base_filtrada, Mod1_1) # rm: borrar


# No perdemos significancia, es mas ganamos en B0
# ganamos en informacion/variabilidad explicada 98.8% -> 99.5%
# ojo# cambiamos direccion en B0, pero en ambos casos cercano a 0
# datos de apalancamiento?


# Observaciones con alto apalancamiento (leverage) ####
# pueden influir en Beta


# distancia de cook
# Si tenemos observaciones presentes Di > 4/(n-p-2) = d se consideran
# datos de apalancamiento

n = nrow(base); n
p = length(Mod1$coefficients); p
d = 4/(n-p-2); d

data.frame(dist_cook = cooks.distance(Mod1),
           d = d,
           mirar = (cooks.distance(Mod1) > d)) %>% 
  arrange(desc(dist_cook)) %>% 
  head(10)

plot(Mod1, 4)
abline(h = d, lty = 2, col = "red")

# con alto apalancamiento posible son las obs.
# 151, 61 y 40

# revisaremos nuestros datos
summary(base)

# revisamos los posibles datos anomalos
base[c(40,61,151), ] # [filas, columnas]

# eliminamos y estudiamos ambos modelos?
# comparar modelos
# seran obs influyentes tambien?


