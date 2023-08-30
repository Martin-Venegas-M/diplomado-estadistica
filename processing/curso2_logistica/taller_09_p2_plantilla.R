
# librerias
library(dplyr)
library(readxl)

# actividad FUGA
fuga <- readxl::read_excel("input/data/original/FUGA.xlsx")
glimpse(fuga)

fuga <- fuga %>% mutate(Internauta = ifelse(Internauta==1,"Si","No"))
fuga <- fuga %>% mutate(Target = as.factor(Target))


# (g)  Modelo automatico

set.seed(20230822)
filas_validacion <- sample(1:nrow(fuga), 0.2*nrow(fuga), replace = FALSE)
## dejaremos un 20% de la data para validacion

fuga$FILTRO <- "ENTRENAMIENTO"
fuga$FILTRO[filas_validacion] <- "VALIDACION"
  
glimpse(fuga)
table(fuga$FILTRO)

data_entrenamiento <- fuga %>% filter(FILTRO == "ENTRENAMIENTO")
data_entrenamiento <- data_entrenamiento %>% select(-FILTRO) ## Quitamos la variable FILTRO ##
nrow(data_entrenamiento)

data_validacion <- fuga %>% filter(FILTRO == "VALIDACION")
data_validacion <- data_validacion %>% select(-FILTRO) ## Quitamos la variable FILTRO ##
nrow(data_validacion)

glimpse(data_entrenamiento)
glimpse(data_validacion)


# Vamos a obtener nuestro modelo automatico

ModCompleto <- glm(Target ~ .,
                   data = data_entrenamiento,
                   family = "binomial")

ModAuto <- step(ModCompleto, method = "backward")

# Solo tenemos 1 paso
# eliminamos solo TC: numerode tarjetas de credito

# revisamos si hay problemas multicolinealidad
car::vif(ModAuto)

# que deciden?
# no tenemos problemas graves

# revision del modelo
summary(ModAuto)
# demasiados coeficientes por interpretar
# el n nos estara provocando que todo sea significativo y tener mucha
# informacion lo hace redundante?

# interpretacion
summary(ModAuto)
  
# factores de riesgo: 
# Edad
# Antiguedad
# N_meses_avances
# prom_uso_TC

# factores protectores:
# Ser mujer
# utilizar servicioes web
# Cupo promedio de la tc
# Numero de meses activo
# Numero de transacciones
# promedio facturado
# promedio de pago

exp(ModAuto$coefficients) %>% as.data.frame() # OR

# Ser mujer disminuye la chance de fugarse en un 15.68%
# Utilizar los servicios web disminuye la chance de fugarse en un 46.83%
# Al aumentar en 1 mes los meses activos del cliente disminuye la chance de
# fugarse en un 11.31% 
# Al aumentar en un mes la cantidad de meses de avances del cliente
# aumenta la chance de fugarse en un 16.61%
# falta mas y mas

# (h) predecir y obtener la probabilidad
aux <- predict(ModAuto, fuga)
p <- exp(aux)/(1+exp(aux))

fuga$p <- p
glimpse(fuga)
summary(fuga$p)

par(bty = "n", pch = 20, las = 1)
boxplot(p ~ FILTRO, data = fuga, 
        outline = FALSE, ylab = "probabilidad")
  
  
  
