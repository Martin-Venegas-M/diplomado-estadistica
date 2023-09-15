#############################
## Script Clase 12 - ARMAX ##
#############################

source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

## Aplicación Contaminación
## Fuente: https://sinca.mma.gob.cl

Data <- rio::import("Contaminacion.xlsx")

Y <- ts(Data$O3, start = c(Data$YEAR[1],Data$MONTH[1]), end = c(2020,12), frequency = 12)
X <- ts(Data$AVG, start = c(Data$YEAR[1],Data$MONTH[1]), end = c(2020,12),frequency = 12)
par(mfrow = c(1,2), bty = "n", las = 1, font.main = 1)
plot(Y, ylab = "O3",xlab = "", xlim = c(2000,2025), ylim = c(0,30))
plot(Y~X, pch = 20, ylim = c(0,30), xlim = c(0,30), ylab = "O3", xlab = "Temperatura media (AVG)")


## Propuesta forecast::auto.arima()
xreg = as.matrix(data.frame(AVG = X))
fit <- forecast::auto.arima(Y, xreg = xreg, d = 0, D = 0)
salida.arima(Y, fit)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)

fixed = c(NA,0,0,NA,0,0,0,0,0,0,0,0,0,0,NA,0,NA,NA,NA,NA)
fit <- forecast::Arima(Y, order = c(4,0,13), seasonal = c(1,0,0), xreg = xreg, fixed = fixed)
salida.arima(Y, fit, fixed = fixed)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)


par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)


## Forecast IPC
Y.h <- ts(dplyr::filter(Data, YEAR == 2021)$O3, start = c(2021,1), frequency = 12)
X.h <- ts(dplyr::filter(Data, YEAR == 2021)$AVG, start = c(2021,1), frequency = 12)
newxreg <- as.matrix(data.frame(AVG = X.h))
pre <- forecast::forecast(fit, xreg = newxreg, level = seq(0.5,0.95,0.05))

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n")
axis(1, 2004:2022)

pre <- forecast::forecast(fit, xreg = newxreg)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n", xlim = c(2020, 2022), ylim = c(0,30))
axis(1, seq(2004,2022,1))
lines(Y <- ts(Data$O3, start = c(Data$YEAR[1],Data$MONTH[1]), frequency = 12))

pre <- forecast::forecast(fit, xreg = newxreg, level = 0.95)
M <- cbind(pre$mean,pre$lower,pre$upper,Y.h)
MAPE <- cumsum(abs(M[,1]/M[,4]-1))*100/(1:10)
M <- cbind(M,MAPE)
colnames(M) <- c("Forecast O3","Lo 95","Hi 95","O3","MAPE") 
M