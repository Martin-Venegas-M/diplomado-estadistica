###############
### Clase 1 ###
###############

library(dplyr)
library(readxl)
library(car)

base = read_excel("input/data/original/01_Actividad_de_regresion_Lineal_Multiple.xlsx")
head(base)
tail(base)

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


# Conceptos

# AIC = 2k-2ln(L): Criterio de información de Akaike
# donde k es el número de parámetros en el modelo estadístico , 
# y L es el máximo valor de la función de verosimilitud para el modelo estimado.

# Es un estimador de la calidad relativa del modelo que tiene en cuenta su complejidad.
# penaliza los modelos complejos en favor del los sencillos para evitar el sobreajuste.
# Cuanto más bajo sea el criterio de información Akaike, mejor.

# BIC: Criterio de información bayesiano, se basa, en parte, por el AIC


# Etapa seleccion de predictores

Mod1 <- lm(HG ~ .,
           data = base) # modelo completo
Mod1

Mod2 <- lm(HG ~ 1,
           data = base) # Modelo nulo
Mod2

# Metricas iniciales
data.frame(R2 = summary(Mod1)$adj.r.squared, AIC = AIC(Mod1), BIC = BIC(Mod1))


# Realizamos seleccion de predictores

Mod1_1 <- step(Mod1, direction = "backward")

Mod2_1 <- step(Mod2, scope = formula(Mod1), direction = "forward")

Mod1_1
Mod2_1

# Etapa Modelamiento

# Analisis del modelo seleccionado, en este caso coincide los resultados
summary(Mod1_1)

data.frame(R2 = summary(Mod1)$adj.r.squared, AIC = AIC(Mod1), BIC = BIC(Mod1))
data.frame(R2 = summary(Mod1_1)$adj.r.squared, AIC = AIC(Mod1_1), BIC = BIC(Mod1_1))
# Mejora levemente el R2

# supuestos
# linealidad
plot(Mod1_1, 1)

# normalidad

plot(Mod1_1, 2)

ks.test(Mod1_1$residuals, "pnorm", mean(Mod1_1$residuals), sd(Mod1_1$residuals))
# no pasamos normalidad

# homoceasticidad
lmtest::bptest(Mod1_1)

# independencia
lmtest::dwtest(Mod1_1)


# Ojo! tenemos datos posiblemente anomalos
outlierTest(Mod1) # libreria car, una opcion para identificar datos anomalos

# revision 
base[151, ]
base[61, ]
summary(base)

# recuerden que nuestra base de datos esta completa
# su tabla de datos no esta completa -> sugiere eliminar NA antes, simplemente
# por la existencia de datos anomalos y queden marcados con el num de la obs.

base <- base %>% slice(-151, -61) # con mucho estudio!!!!!!
base

summary(base)

# que hacemos????


# Etapa Modelamiento sin datos anomalos

Mod1 <- lm(HG ~ .,
           data = base) # modelo completo
Mod1

# Metricas iniciales
data.frame(R2 = summary(Mod1)$adj.r.squared, AIC = AIC(Mod1), BIC = BIC(Mod1))

# Realizamos seleccion de predictores

Mod1_1 <- step(Mod1, direction = "backward")

Mod1_1

# Analisis del modelo seleccionado, en este caso coincide los resultados
summary(Mod1_1)

data.frame(R2 = summary(Mod1)$adj.r.squared, AIC = AIC(Mod1), BIC = BIC(Mod1))
data.frame(R2 = summary(Mod1_1)$adj.r.squared, AIC = AIC(Mod1_1), BIC = BIC(Mod1_1))
# Mejora levemente el R2

# supuestos
# linealidad
plot(Mod1_1, 1)

# normalidad

plot(Mod1_1, 2)

ks.test(Mod1_1$residuals, "pnorm", mean(Mod1_1$residuals), sd(Mod1_1$residuals))
# pasamos normalidad

# homoceasticidad
lmtest::bptest(Mod1_1)

# independencia
lmtest::dwtest(Mod1_1)


# multi colinealidad
vif(Mod1_1) 
# vif cercano a 1 demuestra ausencia de colinealidad
# vif entre 1 y 5 es moderado
# vif mayor a 5 es grave

# Indica tengo que eliminar alguna variable
# cual? debemos elegir
# en este caso eliminamos LARGO por tener la menor significancia

# anova???
anova(Mod1_1)
# indicar a traves del estadistico F, cual es la variable mas importante
# orden de mayor a menor del F -> orden de mayor a menor en "importancia"
# eliminar LARGO

# Es alguno de los caminos

Mod1_3 <- lm(HG ~ PESO + GRASA, 
             data = base)
vif(Mod1_3)

data.frame(R2 = summary(Mod1_3)$adj.r.squared, AIC = AIC(Mod1_3), BIC = BIC(Mod1_3))
# Leve caida en las metricas

# --- > evaluar los supuestos 

# el camino es largo, depende de ustedes que camino llegar y que decisiones tomar



