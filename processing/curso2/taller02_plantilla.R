
# Clase regresion lineal simple

# librerias
library(dplyr)
library(readxl)


base = read_excel("input/data/original/01_Actividad_de_regresion_Lineal_Multiple.xlsx")
head(base)
tail(base)

##################################
### Actividad Regresion Lineal ###
##################################

plot(base$HG, type = "l", bty = "n")
summary(base$HG)

# Antes, vamos a generar un ruido
set.seed(0607)
RUIDO = rnorm(200, 5, 20)
base$RUIDO = RUIDO
head(base)

## Hagamos regresion simple con las tres variables ##
## Ajustemos una recta de regresion con la funcion lm() ##

Mod1 <- lm(HG ~ PESO, data = base)
summary(Mod1) # Reg. simple: HG = B0 + B1*PESO(X1) = 0.05795 + 0.68276*PESO(X1) 

# el Std.Error corresponde a la raiz de la varianza de los estimadores

# el Residual standard error: corresponde a la raiz de la vrianza de sigma

# Prop de informacion explicada por la regresion -> 0.926 -> 92.6%

# H0: No existe regresion vs H1: existe regresion
# -> p-value: < 2.2e-16  -> rechazo H0 -> existe regresion

# test t
# H0: B_i = 0 vs H1: B_i != 0
# -> si es significativa la variable Peso en nuestro modelo

# supuestos
# probamos la semana pasada

Mod2 <- lm(HG ~ GRASA, data = base)
summary(Mod2) # Reg. simple: 


Mod3 <- lm(HG ~ RUIDO, data = base)
summary(Mod3) # Reg. simple: 
# no es significante el RUIDO


# probar si el LARGO es una variable que logre explicar HG

Mod4 <- lm(HG ~ LARGO, data = base)
summary(Mod4) # Reg. simple: 

# existe regresion? -> p-value: < 2.2e-16, a un 95% de confianza -> Si hay regresion
# hay signifcancia del Largo? -> t value y Pr(>|t|): <2e-16 -> Si, lo es
# prop de informacion explicada? Multiple R-squared: 0.7015, el 70.15% de la varianza
# original es explicada por la regresion
# interpretacion del LARGO: por cada 1 cm de aumento en el largo aumenta 0.12 kg HG


Tabla <- data.frame(Modelo = c("PESO","GRASA","RUIDO","LARGO"),
                    R2 = c(summary(Mod1)$r.squared,
                           summary(Mod2)$r.squared,
                           summary(Mod3)$r.squared,
                           summary(Mod4)$r.squared))
Tabla


#############################
# Regresion Lineal Multiple #
#############################

head(base)

# crear una variable a modo de ejemplo, no es necesario realizar:
# que ocurre si una variable categorica es tratada como numerica?
base <- base %>% mutate(TRATAMIENTO_2 = case_when(TRATAMIENTO == "A" ~ 1,
                                                  TRATAMIENTO == "B" ~ 2,
                                                  TRATAMIENTO == "C" ~ 3,
                                                  TRUE ~ 4))
head(base)
tail(base)

# Graficos de caja
par(bty = "n")
boxplot(HG ~ SEXO, data = base, col = "steelblue", las = 1)
boxplot(HG ~ TRATAMIENTO, data = base, col = "steelblue", las = 1)
boxplot(HG ~ DEFORMIDAD, data = base, col = "steelblue", las = 1)

# Graficos de dispersion
par(mfrow = c(1,3))
plot(HG ~ PESO, data = base, col = "steelblue", pch = 19)
plot(HG ~ GRASA, data = base, col = "steelblue", pch = 19)
plot(HG ~ LARGO, data = base, col = "steelblue", pch = 19)

# Vamos a hacer algunos modelos mas

Mod5 <- lm(HG ~ PESO + TRATAMIENTO_2, data = base)
summary(Mod5) # no tiene sentido

# Prop de informacion explicada por la regresion -> 0.926 -> 92.54%

# Dejamos el tratamiento como corresponde, variable categorica
Mod6 <- lm(HG ~ PESO + TRATAMIENTO, data = base)
summary(Mod6)  

# Prop de informacion explicada por la regresion -> 0.9262 -> 92.62%

# supuestos
# linealidad

plot(Mod6, 1)

# Normalidad

# H0: residuos son normales
# H1: residuos no son normales

nortest::lillie.test(Mod6$residuals)
# Si existe normalidad

# Homoceasticidad

# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(Mod6)
# Existe Homoceasticidad

# Independencia

# H0: No hay autocorrelacion
# H1: hay autocorrelacion

lmtest::dwtest(Mod6)
# Si existe Independencia

# podemos cambiar el grupo de referencia? -> usamos factor 

Mod7 <- lm(HG ~ PESO + LARGO, data = base)
summary(Mod7) 


Mod8 <- lm(HG ~ PESO + LARGO + GRASA, data = base)
summary(Mod8)  

## Hacemos un resumen de los modelos con sus R cuadrados ##
Tabla <- data.frame(Modelo = c("PESO", "GRASA", "RUIDO", "LARGO",
                               "PESO+TRATAMIENTO_2","PESO+TRATAMIENTO(factor)",
                               "PESO+LARGO","PESO+LARGO+GRASA"),
                    R2     = c(summary(Mod1)$r.squared, 
                               summary(Mod2)$r.squared,
                               summary(Mod3)$r.squared,
                               summary(Mod4)$r.squared,
                               summary(Mod5)$r.squared,
                               summary(Mod6)$r.squared,
                               summary(Mod7)$r.squared,
                               summary(Mod8)$r.squared),
                    R2_ajd     = c(summary(Mod1)$adj.r.squared,
                                   summary(Mod2)$adj.r.squared,
                                   summary(Mod3)$adj.r.squared,
                                   summary(Mod4)$adj.r.squared,
                                   summary(Mod5)$r.squared,
                                   summary(Mod6)$adj.r.squared,
                                   summary(Mod7)$adj.r.squared,
                                   summary(Mod8)$adj.r.squared))
Tabla

# Elegimos el con mayor R2 ajustad?
# Por que deja de ser significativo el LARGO en el Mod8?
# Tenemos problemas de colinealidad?
# Hay informacion "duplicada"?


