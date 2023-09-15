########################################
###    Datos Cajas de Compensacion   ###
########################################

library(readxl)
base <- read_excel("input/data/original/Venta 2016 - 2021.xlsx")
base$tiempo <- base$Año + (base$Mes -1)/12

dev.off()

y0 <- base$Venta
tiempo <- base$tiempo


######################################
###    1. Graficar la Serie        ###
######################################

par(las=1)
plot(y0 ~ tiempo, type="o", pch=19)

# Identificamos el evento del 18 de octubre
# Identificamos el evento COVID

# Crearemos dos variables Dummies

# Estallido social
D1 <- rep(0, length(tiempo))
D1[45:47] <- 1

# Pandemia
D2 <- rep(0, length(tiempo))
D2[52:68] <- 1


###########################################
###    2. Modelar Deterministico        ###
###########################################

mod1 <- lm(y0 ~ tiempo + D1 + D2)
plot(y0 ~ tiempo, type="o", pch=19,
     xlim= c(2016,2022))
lines(mod1$fitted.values ~tiempo, col = "red", type='o', pch=19)




##########################################
###    3. Analisis de Supuestos        ###
##########################################

# Revisaremos normalidad, Homocedasticidad (varianza constante)
# y no correlaci?n o independencia de los residuos

y1 <- mod1$residuals

par(las=1, pch=19)

plot(mod1,1)
plot(y1 ~ tiempo, type = "o")
abline(h=0, lwd=3)


### Normalidad
library(car)
qqPlot(y1)

shapiro.test(y1)
# Valor-p = 0.157 > 0.05
# Existe normalidad

### Homocedasticidad
library(lmtest)
bptest(mod1)
# Valor-p = 0.119 > 0.05
# Hay homocedasticidad

### No correlaci?n de los residuos
par(mfrow = c(1,2))
acf(y1, lwd=2)
pacf(y1,ylim = c(-0.25,1), lwd=2)

library(LSTS)
Box.Ljung.Test(y1)
# existe correlaci?n en los residuos, y por lo tanto, se puede ajustar un modelo de serie de tiempo


################################################
###    5. Proponer modelo ARMA               ###
################################################
library(forecast)


#auto.arima: define orden p y q, y estima par?metros.
mod2 <- auto.arima(y1)
mod2

y2 <- mod2$residuals

par(mfrow = c(1,2))
acf(y2, lwd = 2)
pacf(y2, ylim = c(-0.25, 1), lwd = 2)

Box.Ljung.Test(y2)



## Modelo AR(1)
mod3 <- Arima(y1, order = c(1,0,0)) #(p ,d=0,q)    p: ver con PACF   q: ver con ACF
y3 <- mod3$residuals
par(mfrow = c(1,2))
acf(y3, lwd = 2)
pacf(y3, ylim = c(-0.25, 1), lwd = 2)

Box.Ljung.Test(y3)


## Modelo ARMA(1,1)
mod4 <- Arima(y1, order = c(1,0,1))
y4 <- mod4$residuals
par(mfrow = c(1,2))
acf(y4, lwd = 2)
pacf(y4, ylim = c(-0.25, 1), lwd = 2)

Box.Ljung.Test(y4)


## Modelo ARMA(6,8)
mod5 <- Arima(y1, order = c(6,0,8))
y5 <- mod5$residuals
par(mfrow = c(1,2))
acf(y5, lwd = 2)
pacf(y5, ylim = c(-0.25, 1), lwd = 2)

Box.Ljung.Test(y5)


## Modelo ARMA(6,8)
fixed = c(NA,0,0,0,0,NA,NA,0,0,0,0,0,NA,NA,NA)
mod6 <- Arima(y1, order = c(6,0,8), fixed=fixed)
y6 <- mod6$residuals
par(mfrow = c(1,2))
acf(y6, lwd = 2)
pacf(y6, ylim = c(-0.25, 1), lwd = 2)

Box.Ljung.Test(y6)


## Comparaci?n de modelos
### AIC = Akaike's Information Criterion
###       Criterio de Informacion de Akaike

### Un modelo ajusta mejor a los datos que otro
### si es que el modelo tiene menor AIC que el otro

mod2$aic # 1470.4  MA(1) - auto arima
mod3$aic # 1474.1  AR(1)
mod4$aic # 1474.4  ARMA(1,1)
mod5$aic # 1474.4  ARMA(6,8)
mod6$aic # 1471.6  ARMA(6,8) fixed

############################
###     Prediccion       ###
############################

## 1. Regresion Lineal en tiempo + D1 + D2
## 2. MA(1)

X1 <- model.matrix(mod1) #mod1 es el modelo, X1
head(X1)
X1 <- X1[,-1] # Quitar columna Intercept


Mod <- Arima(y = y0,
             order = c(0,0,1),
             xreg = X1)

AñoP <-2021:2023
MesP <-rep(1:12,3)
tiempoP <- AñoP + (MesP - 1)/12
tiempoP <- tiempoP[9:length(tiempoP)] # Para empezar de Sep 2021

D1P <- rep(0, length(tiempoP))
D2P <- rep(1, length(tiempoP))

XP <- cbind(tiempo = tiempoP, D1 = D1P, D2 = D2P)

Pred <- forecast(Mod,
                 h = length(tiempoP),
                 xreg = XP)
