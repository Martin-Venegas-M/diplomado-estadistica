

# librerias
library(dplyr)
library(readxl)

# actividad FUGA
fuga <- readxl::read_excel("Bases de datos/FUGA.xlsx")
glimpse(fuga)

fuga <- fuga %>% mutate(Internauta = ifelse(Internauta==1,"Si","No"))
fuga <- fuga %>% mutate(Target = as.factor(Target))


glimpse(fuga)


# estudiemos el comportamiento de la fuga bajo la utilizacion de servicios web

table(fuga$Target, fuga$Internauta)
# fuga es la izquierda, arriba internauta

# P(Fuga    | Si utiliza servicios web) =    1785/26002 = 0.06864857
# P(No Fuga | Si utiliza servicios web) =   24217/26002 = 0.9313514
# P(Fuga    | No utiliza servicios web) =    1310/9506 =  0.1378077
# P(No Fuga | No utiliza servicios web) =    8196/9506 =  0.8621923

addmargins(table(fuga$Target, fuga$Internauta), margin = 1)

# RR1 = P(Fuga    | Si utiliza servicios web)/P(No Fuga | Si utiliza servicios web) 
# RR1 =  (1785/26002) / (24217/26002 )
# RR2 = P(Fuga    | No utiliza servicios web)/P(No Fuga | No utiliza servicios web)
# RR2 =  (1310/9506)  / (8196/9506)
# OR = RR1/RR2 =  ((1785/26002) / (24217/26002 )) / ((1310/9506)  / (8196/9506))
# OR = 0.4611567

# Si utilizar servicios web es un factor protector
# Si utilizar servicios web disminuye la chance de fugarme en un 53.88%

# modelo simple con reg. log.
# Recordemos que en un reg logistica modelamos log(p/(1-p)) = B0 + B1*X

Mod1 <- glm(Target ~ Internauta, data = fuga, family = "binomial")
summary(Mod1)

# OR
exp(Mod1$coefficients) 

# Recordemos, como obtener una probabilidad
# logito: log(p/(1-p)) = B0 + B1*X
# p = exp(B0 + B1*X) / (1 + exp(B0 + B1*X) )

# En nuestro caso,
# p = exp(-1.83362 - 0.77402*X) / (1 + exp(-1.83362 - 0.77402*X) )

# P(Fuga    | Si utiliza servicios web) = 
# X, obtiene valores Si y No, en un Si soy 1, NO soy 0
# exp(-1.83362 - 0.77402*X) / (1 + exp(-1.83362 - 0.77402*X) )
# exp(-1.83362 - 0.77402*1) / (1 + exp(-1.83362 - 0.77402*1) )
# = 0.06864834


# X: TRATAMIENTO
# logito = B0 + TRATAMIENTOB*X + TRATAMIENTOC*X + TRATAMIENTOD*X


# P(Fuga    | No utiliza servicios web) = 
# exp(-1.83362 - 0.77402*0) / (1 + exp(-1.83362 - 0.77402*0) )
# 0.1378076


# se puede hacer con predict
NEWDATA = data.frame(Internauta = "Si")
aux = predict(Mod1, NEWDATA)
exp(aux)/(1+exp(aux))

NEWDATA = data.frame(Internauta = "No")
aux = predict(Mod1, NEWDATA)
exp(aux)/(1+exp(aux))


