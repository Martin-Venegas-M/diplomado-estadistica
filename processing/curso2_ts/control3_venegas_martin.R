# 0. Identificación ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Código elaboración del Control 3 del curso 2 del Diplomado de Estadística - UC
#Institución: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejecutivo: El presente documento contiene el código para la realización del tercer control del segundo curso diplomado (series de tiempo).

# 1. Cargar paquetes y funciones propias ------------------------
if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra,
               summarytools,
               forecast,
               MASS,
               uroot
               
)

source("processing/curso2_ts/resources/TS.diag.R")
source("processing/curso2_ts/resources/summary.arima.R")
source("processing/curso2_ts/resources/salida.arima.R")

# 2. Cargar datos -------------------------------------------------------------------------------------------------------------------------------------------

df <- readxl::read_xlsx("input/data/original/IMACEC_PIB.xlsx") %>% 
  rename(annio = AÑO,
         mes = MES,
         imacec = IMACEC_NM
         )

# 3. Preparación --------------------------------------------------------------------------------------------------------------------------

Y <- ts(df$imacec, start = c(df$annio[1],df$mes[1]), end = c(2022,12), frequency = 12) # Crear objeto TS

# 4. Realización del taller ---------------------------------------------------------------------------------------------------------------------------------

#################################### INTRODUCCION ###########################################################################################################

# De acuerdo con la información preliminar, el Imacec de junio de 2023 cayó 1,0% en comparación con igual mes del a noanterior (Gráfico 1). 
# La serie desestacionalizada aumentó 0,5% respecto del mes precedente y cayó 1,1% en doce meses. El mes registró la misma cantidad de días hábiles que junio de 2022. 
# La variación anual del Imacec fue explicada por la caída del comercio y la industria, compensada en parte por el resto de bienes. 
# En tanto, el crecimiento del Imacec desestacionalizado estuvo determinado por el desempeño de la minería. 
# El Imacec no minero presentó una disminución de 1,3% en doce meses, mientras que en términos desestacionalizados, cayó 0,2% respecto del mes anterior.  
# 
# https://www.bcentral.cl  
# 
# Ejercicio  
# (a) Descargue la serie mensual histórica de Imacec no minero hasta diciembre de 2022.  
# (b) Utilizando la función ts(...) genere un objeto con atributos de serie de tiempo  
# (c) Utilizando forecast::BoxCox.lambda(...) y forecast::BoxCox(...) evalúe una transformación.  
# (d) Utilizando forecast::ndiffs(...) y forecast::nsdiffs(...) evalúe los grados de integración de la serie.  
# (e) Ajuste un modelo SARIMA usando forecast::auto.arima(...).  
# (f) Chequee la significancia estadística de coeficientes estimados, blancura, normalidad y homocedasticidad de los residuos.  
# (g) Proponga una mejora al modelo propuesto en (e) utilizando el argumento fixed en la función forecast::Arima(...).  
# (h) Con forecast::forecast(...) genere pronósticos hasta diciembre de 2027.  
# (i) Compare el desempeño de sus pronostico con respecto a los valores reales observados hasta junio de 2023.

#################################### Desarrollo ###########################################################################################################

# (a) Descargue la serie mensual histórica de Imacec no minero hasta diciembre de 2022. ----
# LISTO

# (b) Utilizando la función ts(...) genere un objeto con atributos de serie de tiempo ----
# LISTO

plot.ts(Y, ylab = "IMACEC No Minero", xlab = "Tiempo") # Veamos la serie

# COMENTARIO: A simple vista, se observa que la serie no cumple con estacionaridad. Primero, se observa una tendencia al alza,
# probablemente debido a que es un fenomeno integrado. Segundo, se observa un quiebre por el tiempo de la pandemia. Tercero, 
# se observa una potencial estacionalidad. Cuarto, se observa que posterior al 2010 la varianza de la serie se amplía.

# De hecho, si se corren los diagnosticos con la serie tal y como está, se puede observar que no hay estacionaridad.

TS.diag(Y)

# Por ende, se hace nesario evaluar potenciales trasnformaciones a los datos para obtener estacionaridad.

# (c) Utilizando forecast::BoxCox.lambda(...) y forecast::BoxCox(...) evalúe una transformación. ----

forecast::BoxCox.lambda(Y, method = "loglik") # Da un lambda de 1.2
Y_v <- forecast::BoxCox(Y, lambda = 1.2) # Da un lambda de 1.2

## Revisar plots
plot.ts(
  cbind(
    "IMACEC NM" = Y,
    "IMACEC NM (Transforamdo)" = Y_v
    )
  )

## Tests Shapiro Wilk

shapiro.test(Y) # No distribuye normalmente
shapiro.test(Y_v) # No distribuye normalmente

# COMENTARIO: A priori, la transformación no parece hacer mucho.

# d) Utilizando forecast::ndiffs(...) y forecast::nsdiffs(...) evalúe los grados de integración de la serie. ----

##### NDIFFS SERIE ESTACIONARIA ####
# kpss
forecast::ndiffs(Y, test = "kpss")
forecast::ndiffs(Y_v, test = "kpss")

# adf
forecast::ndiffs(Y, test = "adf")
forecast::ndiffs(Y_v, test = "adf")

# pp
forecast::ndiffs(Y, test = "pp")
forecast::ndiffs(Y_v, test = "pp")

##### NDIFFS SERIE NO ESTACIONARIA (SEASONAL) ####
# seas
forecast::nsdiffs(Y, test = "seas")
forecast::nsdiffs(Y_v, test = "seas")

# ocsb
forecast::nsdiffs(Y, test = "ocsb")
forecast::nsdiffs(Y_v, test = "ocsb")

# hegy
forecast::nsdiffs(Y, test = "hegy")
forecast::nsdiffs(Y_v, test = "hegy")

# ch
forecast::nsdiffs(Y, test = "ch")
forecast::nsdiffs(Y_v, test = "ch")

# COMENTARIO: La mayoría de los tests sugieren una diferenciación.

Y_d <- diff(Y, lag = 1)
Y_vd <- diff(Y_v, lag = 1)

## Revisar plots (Original y diferenciado)
plot.ts(
  cbind(
    "IMACEC NM" = Y, # Original
    "IMACEC NM (Dif = 1)" = Y_d # Diferenciado
  )
)

## Revisar plots (Transformado y Transformado diferenciado)
plot.ts(
  cbind(
    "IMACEC NM (Transformado)" = Y_v, # Transforamado
    "IMACEC NM (Transforamdo y Dif = 1)" = Y_vd # Transformado y diferenciado
  )
)

## Revisar plots
plot.ts(
  cbind(
    "IMACEC NM (Dif = 1)" = Y_d, # Diferenciado
    "IMACEC NM (Transforamdo y Dif = 1)" = Y_vd # Transformado diferenciado
  )
)

# COMENTARIO: La diferenciación aun nivel parece darle más estacionalidad a la serie.
# Además, se identifica mucho más la estacionalidad. Sin embargo, sigue sin pasar el test de blancura.

TS.diag(Y_d)
TS.diag(Y_vd)

# (e) Ajuste un modelo SARIMA usando forecast::auto.arima(...). ----

m_auto <- auto.arima(Y) # Ajustar modelo automatico
m_auto # Ver modelo

# (f) Chequee la significancia estadística de coeficientes estimados, blancura, normalidad y homocedasticidad de los residuos. ----

salida.arima(Y, m_auto) # Ver salida y ajuste
summary.arima(m_auto) # Ver parametros

# El auto.arima() entrega los siguientes parámetros:
# - AR(2) = p = 2
# - MA(1) = q = 1
# - SAR(2) = P = 2
# - D = 1 (una diferenciación seasonal)
# - SMA(1) = Q = 2
# drift (no sé que es)

# COMENTARIO: No todos los parametros son estadísticamente significativos al 95% de confianza. Solo AR(2), MA(1), SAR(2) y el drift.

TS.diag(m_auto$residuals) # Ver diagnosticos

# Veamos ACF y PACF COMPARADOS
par(mfrow = c(1, 2))
acf(m_auto$residuals, ylim = c(-0.25, 1), lwd = 2); pacf(m_auto$residuals, lwd = 2)
shapiro.test(m_auto$residuals)

plot.ts(m_auto$residuals)

# Con ACF y PACF vemos que aun queda correlación por extraer en los componentes AR y MA. Esto se confirma al evaluar el test de blancura, en tanto
# existen puntos que están por debajo del umbral. Por lo demás, se observa en el QQ-plot que los residuos no se distribuyen normalmente, lo que se 
# confirma con el test de Shapiro-Wilk. Además, a partir del test de Breusch-Pagan se observa que no se cumple homocedasticidad.

# (g) Proponga una mejora al modelo propuesto en (e) utilizando el argumento fixed en la función forecast::Arima(...).  ----

f <- c(
  rep(NA, 6), 0, rep(NA, 2), 0, rep(NA, 3), 0, # Fijar valores 7, 10 y 14 para los parámetros φ (AR) y θ (MA)
  rep(NA, 5), 0, rep(NA, 2), 0, NA, 0, NA, 0, rep(NA, 6), 0, # Fijar valores 6, 9 , 11, 13 y 20 para los parámetros φ (AR) y θ (MA)
  
  rep(NA, 6), 0, rep(NA, 2), 0, rep(NA, 3), 0, # Fijar valores 7, 10 y 14 para los parámetros estacionales Φ (SAR) y Θ (SMA)
  rep(NA, 5), 0, rep(NA, 2), 0, NA, 0, NA, 0, rep(NA, 6), 0 # Fijar valores 6, 9 , 11, 13 y 20 para los parámetros estacionales Φ (SAR) y Θ (SMA)
  )

m_fixed <- forecast::Arima(Y,
  order = c(14, 0, 20),
  seasonal = c(14, 1, 20),
  fixed = f
  )
