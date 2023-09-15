############################
## Script Clase 12 - ARMA ##
############################

source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

Y <- LSTS::malleco

## Grafico de Serie de Tiempo
par(bty = "n", las = 1, mfrow = c(1,1))
plot(Y, xlim = c(1200, 2000), ylab = "Width tree ring", xlab = "Year", xaxt = "n", ylim = c(0,2))
axis(1, seq(1200,2000,100))

## Transformación?
tiempo <- time(Y)
par(bty = "n")
aux <- MASS::boxcox(lm(Y ~ tiempo))
lambda <- aux$x[aux$y == max(aux$y)]
axis(1, at = lambda, labels = round(lambda,3))

## Alternativa
lambda <- forecast::BoxCox.lambda(Y, method = "loglik")
lambda

## ARMA(p,q) --> ¿p,q?
## ACF  --> q
## PACF --> p
lag.max <- 10
par(bty = "n", las = 1, mfrow = c(1,2))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
pacf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")

## Estimación
model <- forecast::Arima(Y, order = c(1,0,5), lambda = NULL)
salida.arima(Y, model)

par(bty = "n", las = 1, mfrow = c(1,1))
plot(model)

ar <- model$coef[1]
ma <- model$coef[2:5]
par(bty = "n", las = 1, mfrow = c(1,1))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
ACF <- ARMAacf(ar = ar, ma = ma, lag.max = lag.max)
lines(ACF ~ c(0:lag.max), type = "p", pch = 20, col = "darkblue")

model <- forecast::auto.arima(Y, max.p = 1, max.q = 5)
salida.arima(Y, model)

par(bty = "n", las = 1, mfrow = c(1,1))
plot(model)

ar <- model$coef[1]
ma <- NULL
par(bty = "n", las = 1, mfrow = c(1,1))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
ACF <- ARMAacf(ar = ar, ma = ma, lag.max = lag.max)
lines(ACF ~ c(0:lag.max), type = "p", pch = 20, col = "darkblue")

## Dianostico de Residuos
TS.diag(model$res, col = "darkred", border = "darkblue")


## Predicción
pred <- forecast::forecast(model, h = 10, fan = T)
par(bty = "n", las = 1, mfrow = c(1,1), font.main = 1)
plot(pred, xlim = c(1200, 2000))
lines(pred$fitted, col = "darkorange")
abline(h = mean(Y), col = "darkred", lty = 2)

par(bty = "n", las = 1, mfrow = c(1,1), font.main = 1)
plot(pred, xlim = c(1950, 1990))
abline(h = mean(Y), col = "darkred", lty = 2)

pred <- forecast::forecast(model, h = 10, level = 0.95)
pred
