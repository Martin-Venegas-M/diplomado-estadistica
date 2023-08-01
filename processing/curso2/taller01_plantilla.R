# Clase regresion lineal simple

# librerias
library(dplyr)
library(readxl)


base = read_excel("input/data/original/01_Actividad_de_regresion_Lineal_Multiple.xlsx")
head(base)
tail(base)

plot(base$HG, type = "l", bty = "n")
summary(base$HG)


# Antes, vamos a generar un ruido
set.seed(0607)
RUIDO = rnorm(200, 5, 20)
base$RUIDO = RUIDO
head(base)


# Grafico de HG con las demas variables

par(mfrow = c(1,3)) # que se muestre 1 fila y 3 columnas
plot(HG ~ PESO, data = base, type = "p", bty = "n", ylab = "HG", xlab = "PESO",
     pch = 19)
plot(HG ~ GRASA, data = base, type = "p", bty = "n", ylab = "HG", xlab = "GRASA",
     pch = 19)
plot(HG ~ RUIDO, data = base, type = "p", bty = "n", ylab = "HG", xlab = "RUIDO",
     pch = 19)
dev.off()


# Vemos correlacion

cor(base %>% select(HG, PESO, GRASA, RUIDO))

# Deduccion la variable explicativa que nos va a dar mas informacion a 
# priori es el Peso, luego viene GRASA.

# Reg. simple: HG = B0 + B1*PESO(X1) = 0.05795 + 0.68276*PESO(X1) 

Mod1 <- lm(HG ~ PESO, data = base)
summary(Mod1)

# Prop. de variabilidad/informacion explicada por la var. explicativa
# sobre la var. respuesta: 0.926 -> 92.6% (R2)

# Linealidad

plot(Mod1, 1)

# Normalidad

plot(Mod1, 2)

# H0: residuos son Normales
# H1: residuos no son Normales

ks.test(Mod1$residuals, "pnorm", mean(Mod1$residuals), sd(Mod1$residuals))
nortest::lillie.test(Mod1$residuals)

# Pasamos normalidad

# Homocedasticidad

# H0: Homocedasticidad
# H1: Heterocedasticidad

lmtest::bptest(Mod1)

# Tenemos homocedasticidad

# Independencia 

# H0: No hay autocorrelacion
# H1: hay autocorrelacion

lmtest::dwtest(Mod1)

# Si hay independencia


# Modelo 2: Peso ~ GRASA
Mod2 = lm(HG ~ GRASA, data = base)
summary(Mod2)

# Modelo 3: Peso ~ RUIDO
Mod3 = lm(HG ~ RUIDO, data = base)
summary(Mod3)


# R2 -> EL MEJOR MODELO ES AQUEL EN DONDE LA VAR EXPLICATIVA ES PESO

# graficamos

dev.off()
plot(HG ~ PESO, data = base, type = "p", bty = "n", ylab = "HG", xlab = "PESO",
     pch = 19)
abline(Mod1, col = "steelblue", lwd = 2)


base <- base %>% 
  mutate(Yhat = c(Mod1$fitted.values))
head(base)

plot(base$HG, type = "l", bty = "n")
lines(base$Yhat, col = "steelblue")

