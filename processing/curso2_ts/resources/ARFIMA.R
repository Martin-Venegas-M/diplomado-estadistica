#####################
## Script Clase 25 ## 
#####################

library(fracdiff)
source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")


## Climatologia
X <- rio::import("shihua.txt")$Value
Y <- log(X)
Year <- rio::import("shihua.txt")$Year

par(bty = "n", mfrow = c(1,1), las = 1, font.main = 1)
plot(Y ~ Year, type = "l")

par(bty = "n", mfrow = c(1,1), las = 1, font.main = 1)
acf(Y, lag.max = 100, main = "")

fit_1 <- forecast::auto.arima(Y, d = 0)
salida.arima(Y, fit_1)

TS.diag(fit_1$res)

salida.arima(Y, fit_1)

ar <- fit_1$coef[1:3]
ma <- fit_1$coef[4]
lag.max <- 100

par(bty = "n", las = 1, mfrow = c(1,1))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
ACF <- ARMAacf(ar = ar, ma = ma, lag.max = lag.max)
lines(ACF ~ c(0:lag.max), type = "p", pch = 20)

fit_2 <- forecast::arfima(Y)
summary(fit_2)
aux <- fracdiff.sim(100000, ar = fit_2$ar, ma = fit_2$ma, d = fit_2$d)$series

par(bty = "n", las = 1, mfrow = c(1,1))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
ACF <- acf(aux, lag.max = lag.max, plot = F)$acf[,,1]
lines(ACF ~ c(0:lag.max), type = "p", pch = 20)

fit_3 <- fracdiff(Y, nar = 5, nma = 2)
summary(fit_3)
aux <- fracdiff.sim(100000, ar = fit_3$ar, ma = fit_3$ma, d = fit_3$d)$series
par(bty = "n", las = 1, mfrow = c(1,1))
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
ACF <- acf(aux, lag.max = lag.max, plot = F)$acf[,,1]
lines(ACF ~ c(0:lag.max), type = "p", pch = 20)

TS.diag(fit_1$res)


par(bty = "n", las = 1, mfrow = c(1,1))
pred <- forecast::forecast(fit_3, h = 1000)
plot(pred)
lines(pred$fitted, col = "darkorange")
