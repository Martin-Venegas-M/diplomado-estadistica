##############################
## Script Clase 12 - SARIMA ##
##############################

source("TS.diag.R")
source("summary.arima.R")
source("salida.arima.R")

## Mauna Loa Atmospheric CO2 Concentration
## Fuente: https://scrippsco2.ucsd.edu/data/atmospheric_co2

Y <- ts(co2, start = start(co2), end = c(1995,12), frequency = 12)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(Y, ylab = "co2",xlab = "", xlim = c(1955,2000), ylim = c(300,380), main = "Mauna Loa Atmospheric CO2 Concentration", xaxt = "n")
axis(1,seq(1955,2000,5))

## Propuesta forecast::auto.arima()
fit <- forecast::auto.arima(Y)
salida.arima(Y, fit)
fixed <- c(0,NA,NA,NA,NA,0,NA)
fit <- forecast::Arima(Y, order = c(2,1,2), seasonal = c(1,1,2), fixed = fixed)
salida.arima(Y, fit, fixed = fixed)


par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)

fixed <- c(0,NA,NA,NA,0,0,0,0,0,0,NA,NA,0,NA)
fit <- forecast::Arima(Y, order = c(2,1,9), seasonal = c(1,1,2), fixed = fixed)
salida.arima(Y, fit, fixed = fixed)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
TS.diag(fit$res)


par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(fit)


## Forecast IPC
pre <- forecast::forecast(fit, h = 24, level = seq(0.5,0.95,0.05))

pdf(paste(path,"clase12_38.pdf",sep = "/"), 20*k, 10*k)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n", xlim = c(1955, 2000), ylim = c(300,380))
lines(pre$fitted, col = "darkorange")
axis(1, seq(1955,2000,5))
dev.off()

pre <- forecast::forecast(fit, h = 24)
par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(pre, xaxt = "n", xlim = c(1993, 1998), ylim = c(350,380))
axis(1, 1993:1998)
lines(co2, lwd = 2)

pre <- forecast::forecast(fit, h = 24, level = 0.95)
M <- cbind(pre$mean,pre$lower,pre$upper,co2)
M <- na.omit(M)
MAPE <- cumsum(abs(M[,1]/M[,4]-1))*100/(1:12)
M <- cbind(M,MAPE)
colnames(M) <- c("Forecast co2","Lo 95","Hi 95","co2","MAPE") 
M