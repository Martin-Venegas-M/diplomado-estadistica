#############################
## Script Clase 12 - SARMA ##
#############################


source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

## Accidental Deaths in the US 1973–1978
## Description: A time series giving the monthly totals of accidental deaths in the USA. 
## The values for the first six months of 1979 are 7798 7406 8363 8460 9217 9316.
Y <- USAccDeaths

par(bty = "n", las = 1, mfrow = c(1,1))
plot(Y, xlim = c(1972, 1980), ylab = "", xlab = "Year", xaxt = "n", ylim = c(6000,12000))
axis(1, 1972:1980)

lag.max <- 60
par(bty = "n", las = 1, mfrow = c(1,2))
Y <- c(Y)
acf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")
pacf(Y, lag.max = lag.max, xlim = c(0,lag.max), ylim = c(-1,+1), main = "", lwd = 2, col = "darkred")

Y <- USAccDeaths
model <- forecast::auto.arima(Y, max.p = 11, max.q = 11, max.P = 1, max.Q = 4, d = 0, D = 0)
salida.arima(Y, fit = model)

par(bty = "n", las = 1, mfrow = c(1,1))
plot(model)

TS.diag(model$res, col = "darkred", border = "darkblue")

pred <- forecast::forecast(model, h = 12, fan = T)
par(bty = "n", las = 1, mfrow = c(1,1), font.main = 1)
plot(pred, xlim = c(1972, 1981), ylab = "", xlab = "Year", xaxt = "n", ylim = c(6000,12000))
axis(1, 1972:1981)
lines(pred$fitted, col = "darkorange")
abline(h = mean(Y), col = "darkred", lty = 2)

pred <- forecast::forecast(model, level = 0.95)
pred