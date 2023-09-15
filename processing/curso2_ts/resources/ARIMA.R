#############################
## Script Clase 12 - ARIMA ##
#############################

source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")


## Aplicación Indice de Precios al Consumidor (IPC)
## Fuente: https://www.bcentral.cl

Data <- rio::import("IPC.xlsx")
Y <- ts(Data$IPC, start = c(Data$YEAR[1],Data$MONTH[1]), frequency = 12)

par(mfrow = c(1,2), bty = "n", las = 1, font.main = 1)
plot(Y)
plot(log(Y))

par(mfrow = c(1,2), bty = "n", las = 1, font.main = 1)
X <- diff(Y)/Y
plot(X, ylab = "", main = "Tasa Mensual")
Z <- diff(Y, lag = 12)/Y
plot(Z, ylab = "", main = "Tasa Anual")
abline(h = 0.1, lty = 2)

## ¿Que orden de integración tiene el IPC?
forecast::ndiffs(Y, test = "adf", type = "level", max.d = 4)
forecast::ndiffs(Y, test = "pp", type = "level", max.d = 4)
forecast::ndiffs(Y, test = "kpss", type = "level", max.d = 4)

## Efecto diferenciación
par(mfrow = c(1,3), bty = "n", las = 1, font.main = 1)
plot(diff(Y, differences = 1), ylim = c(-3,+3))
plot(diff(Y, differences = 2), ylim = c(-3,+3))
plot(diff(Y, differences = 3), ylim = c(-3,+3))

## Propuesta forecast::auto.arima()
forecast::auto.arima(ts(Y), max.d = 3)
fit <- forecast::Arima(Y, order = c(1,2,2))
salida.arima(Y, fit)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)


par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)

fixed = c(NA,0,NA,0,0,0,0,NA,0,0,0,0,0,NA,NA,NA)
fit <- forecast::Arima(Y, order = c(1,2,13), seasonal = c(0,0,2), fixed = fixed)
salida.arima(Y, fit, fixed = fixed)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)


## Forecast IPC
pre <- forecast::forecast(fit, h = 12, level = seq(0.5,0.95,0.05))

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n")
axis(1, seq(1990,2025,5))

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n", xlim = c(2022, 2024), ylim = c(100,140))
axis(1, seq(2022,2024,1))


## Forecast TASA ANUAL
pre <- forecast::forecast(fit, h = 12, level = 0.95)
pre.aux <- pre
Y.aux <- ts(c(pre$x, pre$mean), start = c(1989, 4), frequency = 12)
pre.aux$x <- diff(Y.aux, lag = 12)/Y.aux
Y.aux <- ts(c(pre$x[time(pre$x)>2022 + (6-1)/12], pre$mean), start = c(2022, 7), frequency = 12)
pre.aux$mean <- diff(Y.aux, lag = 12)/Y.aux 
pre.aux$lower = NA
pre.aux$upper = NA

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre.aux, xaxt = "n", xlim = c(1990,2030), main = "")
axis(1, seq(1990,2030,5))
abline(h = 0.1, lty = 2)

M <- cbind(pre$mean,pre$lower,pre$upper,pre.aux$mean)
colnames(M) <- c("Forecast IPC","Lo 95","Hi 95","Forecast Tasa Anual") 
M