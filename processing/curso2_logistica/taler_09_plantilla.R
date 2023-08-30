
# librerias
library(dplyr)
library(readxl)

# actividad FUGA

fuga <- readxl::read_excel("input/data/original/FUGA.xlsx")
head(fuga)
head(as.data.frame(fuga))

glimpse(fuga)

# internauta esta como numerico...
table(fuga$Internauta)
# esta como 0 y 1, basta con eso
# igualmente vamos a recodificar
fuga <- fuga %>% mutate(Internauta = ifelse(Internauta==1,"Si","No"))

# nuestra variable respuesta tambien esta como 0 y 1
# basta con eso, igualmente vamos a transformar
fuga <- fuga %>% mutate(Target = as.factor(Target))

glimpse(fuga)
str(fuga) 

#  vamos bien, revisamos escalas para ver errores de tipeo
summary(fuga)

# datos faltantes

colSums(is.na(fuga))
nrow(fuga)

# no tenemos problemas de datos daltantes

# (d) modelo de estudio ####

# Modelo preliminar

Modelo1 <- glm(Target ~ Sexo + Edad,
               data = fuga,
               family = "binomial")
summary(Modelo1)

# Vemos que SexoM tiene beta negativo, es decir, es un factor protector
# Vemos que Edad tiene beta positivo, es decir, es un factor de riesgo

table(fuga$Target, fuga$Sexo)

# (e) interpretacion ####

exp(Modelo1$coefficients[2]) # Ser mujer disminuye en un 12.5% la chance de fuga
exp(Modelo1$coefficients[3]*1) # En un aumento de 1 año del cliente aumentamos 
# en un -+1% la chance de fuga

# (f)

exp(Modelo1$coefficients[3]*10) # una decada de diferencia entre clientes hace
# que aumente la chance de fuga en un 11.3%
# Esta comparacion es valida para cualquier comparacion de clientes
# que tengan 10 años de diferencia ##

