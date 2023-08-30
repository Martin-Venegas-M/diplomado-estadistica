# 0. Identificacion ----------------------------------
#Título: Codigo elaboración de la control 1 del segundo curso del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del primer control del segundo curso del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra,
               nortest,
               car,
               lmtest)

# d cook function

dcook <- function(data, model){
  n <- nobs(model) #n de observaciones
  k <- length(coef(model)) # n de parametros
  dcook <- 4 / (n - k - 1) #punt de corte
  
  final <- broom::augment_columns(model, data = data)
  final$id <- as.numeric(row.names(final))
  # identify obs with Cook's D above cutoff
  ggplot(final, aes(id, .cooksd)) +
    geom_bar(stat = "identity", position = "identity") +
    xlab("Obs. Number") + ylab("Cook's distance") +
    geom_hline(yintercept = dcook) +
    geom_text(aes(label = ifelse((.cooksd > dcook), id, "")),
              vjust = -0.2, hjust = 0.5)
}

dffits_mar <- function(model){
  n <- nobs(model) #n de observaciones
  k <- length(coef(model)) # n de parametros
  corte <- 2*sqrt(k/n) #punt de corte
  data.frame(yhat = dffits(model)) %>% filter(yhat > corte)
}


# 2. Cargar datos --------------------

df <- read_excel("input/data/original/Contam.xlsx")

# 3. Realización del taller --------------------

# Objetivo: Exokucar el comportamiento de los niveles de contaminación del aire en la RM
# Variable dependiente: PM2.5
# Variables independientes: Dos tipos -> metereologicas y contaminantes atmosfericos


# 1.	Obtenga el mejor modelo de regresión lineal simple basado en las variables meteorológicas. --------------------

par(mfrow = c(1,5))
plot(PM2.5 ~ Viento   , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TProm, data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TMin    , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ TMax   , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ Humed, data = df, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

m_viento <- lm(PM2.5 ~ Viento, data = df)
m_tprom <- lm(PM2.5 ~ TProm, data = df)
m_tmin <- lm(PM2.5 ~ TMin, data = df)
m_tmax <- lm(PM2.5 ~ TMax, data = df)
m_humed <- lm(PM2.5 ~ Humed, data = df)

data.frame(
  Variable = c("Viento", "TProm", "TMin", "TMax", "Humed"),
  RCuadrado = c(
    summary(m_viento)$r.squared * 100,
    summary(m_tprom)$r.squared * 100,
    summary(m_tmin)$r.squared * 100,
    summary(m_tmax)$r.squared * 100,
    summary(m_humed)$r.squared * 100
  )
) %>% # Eliminar columnas innecesarias
  kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

plot(m_tmin, 1)
nortest::lillie.test(m_tmin$residuals)
lmtest::bptest(m_tmin)
lmtest::dwtest(m_tmin)

#################### LOGS #####################
log_m_viento <- lm(PM2.5 ~ log(Viento ), data = df)
log_m_tprom <- lm(PM2.5 ~  log(TProm  ), data = df)
log_m_tmin <- lm(PM2.5 ~   log(TMin   ), data = df)
log_m_tmax <- lm(PM2.5 ~   log(TMax   ), data = df)
log_m_humed <- lm(PM2.5 ~  log(Humed  ), data = df)

data.frame(
  Variable = c("log(Viento)", "log(TProm)", "log(TMin)", "log(TMax)", "log(Humed)"),
  RCuadrado = c(
    summary(log_m_viento)$r.squared * 100,
    summary(log_m_tprom)$r.squared * 100,
    summary(log_m_tmin)$r.squared * 100,
    summary(log_m_tmax)$r.squared * 100,
    summary(log_m_humed)$r.squared * 100
  )
) %>% # Eliminar columnas innecesarias
  kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

plot(log_m_tmin, 1)
nortest::lillie.test(log_m_tmin$residuals)
lmtest::bptest(log_m_tmin)
lmtest::dwtest(log_m_tmin)
###############################################

# 2.	Obtenga el mejor modelo de regresión lineal simple basado en los contaminantes atmosféricos. --------------------
# Puede recurrir a transformar las variables de ser necesario. Revise los supuestos y explique el modelo escogido en cuanto a sus resultados e interpretación. --------------------

#par(mfrow = c(1,5))
plot(PM2.5 ~ NO   , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ NO2, data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ CO    , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(PM2.5 ~ O3   , data = df, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

m_no <- lm(PM2.5 ~ NO, data = df)
m_no2 <- lm(PM2.5 ~ NO2, data = df)
m_co <- lm(PM2.5 ~ CO, data = df)
m_o3 <- lm(PM2.5 ~ O3, data = df)

data.frame(
  Variable = c("NO", "NO2", "CO", "O3"),
  RCuadrado = c(
    summary(m_no)$r.squared * 100,
    summary(m_no2)$r.squared * 100,
    summary(m_co)$r.squared * 100,
    summary(m_o3)$r.squared * 100
  )
) %>% # Eliminar columnas innecesarias
  kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
 
plot(m_no2,1)
nortest::lillie.test(m_no2$residuals)
lmtest::bptest(m_no2)
lmtest::dwtest(m_no2)

############## LOG #################

log_m_no <- lm(PM2.5 ~  log(NO  ), data = df)
log_m_no2 <- lm(PM2.5 ~ log(NO2 ), data = df)
log_m_co <- lm(PM2.5 ~  log(CO  ), data = df)
log_m_o3 <- lm(PM2.5 ~  log(O3  ), data = df)

data.frame(
  Variable = c("log(NO)", "log(NO2)", "log(CO)", "log(O3)"),
  RCuadrado = c(
    summary(log_m_no)$r.squared * 100,
    summary(log_m_no2)$r.squared * 100,
    summary(log_m_co)$r.squared * 100,
    summary(log_m_o3)$r.squared * 100
  )
) %>% # Eliminar columnas innecesarias
  kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
####################################

# 3.	Con base a todas las variables (meteorológicas y contaminantes), mediante una técnica iterativa (forward o backward) seleccione el mejor modelo predictivo. Indique para cada paso qué variable entra/sale del modelo, indicando el aumento/disminución del R2-corregido del modelo final vs inicial. --------------------

## Variables metereologicas

step(
  lm(PM2.5 ~ Viento + TProm + TMin + TMax + Humed, data = df),
  direction = "backward"
)

## Variables contamianción:

step(
  lm(PM2.5 ~ NO + NO2 + CO + O3, data = df),
  direction = "backward"
)

# Para todas

step(
  lm(PM2.5 ~ ., data = df),
  direction = "backward"
)

summary(lm(PM2.5 ~ ., data = df))$adj.r.squared
summary(lm(PM2.5 ~ Viento + TMin + TMax + Humed + NO + NO2 + CO + O3, data = df))$adj.r.squared
summary(lm(PM2.5 ~ Viento + TMin + Humed + NO + NO2 + CO + O3, data = df))$adj.r.squared


# 4.	Proponga un modelo con tres predictores (debe incluir una variable meteorológica y dos contaminantes), revise supuestos y evalúe con especial énfasis el problema de multicolinealidad, corregir de ser necesario. Apóyese de tablas de correlación, gráficos y métricas respectivas. --------------------

m_completo <- lm(PM2.5 ~ ., data = df)
m_best <- lm(PM2.5 ~ Viento + TMin + Humed + NO + NO2 + CO + O3, data = df)
m_propuesta1 <- lm(PM2.5 ~ TMin + NO2 + CO, data = df)
m_propuesta2 <- lm(PM2.5 ~ TMin + CO + O3, data = df)


vif(m_best)
vif(lm(PM2.5 ~ Viento + TMin + Humed + CO + O3, data = df))
vif(lm(PM2.5 ~ TMin + CO + O3, data = df))



sjPlot::tab_model(list(m_completo, m_best, m_propuesta1, m_propuesta2), # los modelos estimados
                  show.ci=FALSE, # no mostrar intervalo de confianza (por defecto lo hace)
                  p.style = "stars", # asteriscos de significación estadística
                  dv.labels = c("Modelo Completo", "Modelo Backward", "Modelo Propuesta 1", "Modelo Propuesta 2"), # etiquetas de modelos o variables dep.
                  string.pred = "Predictores", string.est = "β") # nombre predictores y símbolo beta en tabla

par(mfrow = c(1,2))
plot(m_propuesta1, 1)
plot(m_propuesta2, 1)

nortest::lillie.test(m_propuesta1$residuals)
lmtest::bptest(m_propuesta1)
lmtest::dwtest(m_propuesta1)
vif(m_propuesta1)

nortest::lillie.test(m_propuesta2$residuals)
lmtest::bptest(m_propuesta2)
lmtest::dwtest(m_propuesta2)
vif(m_propuesta2)


# 5.	Para el mejor modelo obtenido, ya sea en 3) o en 4), ¿Puede realizar una predicción para la primera semana de agosto 2023? Justifique (sea sí o no su respuesta, no es necesario realizar la predicción). --------------------
