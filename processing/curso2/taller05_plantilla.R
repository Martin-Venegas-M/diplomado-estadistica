library(dplyr)
library(car)

base <- read_excel("input/data/original/Abalon.xlsx")
head(base)
nrow(base)

# Pregunta 1
# a ####
# ¿Qué variable explica mejor linealmente el peso cuerpo (pesocu): 
# largo, diámetro o alto? Revise los supuestos y explique el modelo escogido.

par(mfrow = c(1,3))
plot(pesocu ~ largo   , data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(pesocu ~ diametro, data = base, type = "p", pch = 20, bty = "n", las = 1)
plot(pesocu ~ alto    , data = base, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

Mod0 <- lm(pesocu ~ 1       , data = base)
Mod1 <- lm(pesocu ~ largo   , data = base)
Mod2 <- lm(pesocu ~ diametro, data = base)
Mod3 <- lm(pesocu ~ alto    , data = base)

## Mod1, Mod2 y Mod3 se comparan por defecto con el Mod0 ##
## A partir del estadistico F o el t value de la pendiente ##

summary(Mod1) ## Mejor modelo, por mayor R2 (0.8287) ##
summary(Mod2)
summary(Mod3)

# Largo explica mejor linealmente el peso de cuerpo

# Supuestos
# linealidad

plot(Mod1, 1)
# no pasa

# Normalidad

plot(Mod1, 2)

# H0: residuos son normales
# H1: residuos no son normales

nortest::lillie.test(Mod1$residuals)
ks.test(Mod1$residuals, "pnorm", mean(Mod1$residuals), sd(Mod1$residuals))
# No existe normalidad

# Homoceasticidad

# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(Mod1)
# No existe Homoceasticidad

# si bien el largo el explica mejor el peso de cuerpo, el modelo
# no logra cumplir los supuestos satisfactoriamente

# Explicacion

summary(Mod1)

# El largo explica un 82.87% de la variabilidad de peso de cuerpo del abalon
# Tenemos que por cada aumento en una unidad del largo aumenta el peso de
# cuerpo en 15.4682 unidades.

# b ####
# ¿En cuántas unidades aumenta o decrece el peso cuerpo, por cada 5 unidades 
# que aumenta el mejor regresor? 

# Al aumentar en 5 unidades el largo aumenta el peso de cuerpo en 
# 5*15.4682 = 77.341 unidades

# c ####
# Mediante una técnica iterativa (forward o backward) seleccione el mejor 
# modelo predictivo. Indique en cada paso que variable ingresa/sale. 
# Interprete el modelo final y sus resultados.

# para backward
Mod1 <- lm(pesocu ~ ., 
           data = base) # Modelo completo

Mod1_1 <- step(Mod1, direction = "backward") 

# solo tenemos un pasos 
# en el primero se elimina la variable centro, luego no se eliminan 
# mas variables

summary(Mod1_1)

# al aumentar en 1 unidad el largo aumenta 4.16 uni el peso del cuerpo
# al aumentar en 1 unidad el diametro disminuye 2.77 uni el peso del cuerpo
# al aumentar en 1 unidad el alto aumenta 5.47 uni el peso del cuerpo
# al aumentar en 1 unidad el peso total aumenta 0.21 uni el peso del cuerpo
# al aumentar en 1 unidad el peso de la concha disminuye 0.11 uni el peso del cuerpo
# al aumentar en 1 unidad el num de anillos disminuye 0.45 uni el peso del cuerpo

# Por ende, el largo, alto y peso total logran tener una relacion
# positiva con el peso del cuerpo, las demas tienen una relacion
# negativa

# Por otro lado, el diametro y numero de anillos no son significativos
# dentro del modelo

# El modelo completo logra explicar en un 93.01% la variabilidad 
# del peso del cuerpo


# para forward
Mod2 <- lm(pesocu ~ 1, 
           data = base) # Modelo nulo

Mod2_1 <- step(Mod2, scope = formula(Mod1), direction = "forward") 

# tenemos 6 pasos
# en el primero se agrega el peso total
# en el segundo se agrega el largo
# en el tercero se agrega el peso de la concha
# en el cuarto se agrega el alto
# en el quinto se agrega el numero de anillos
# en el sexto se agrega el diametro

summary(Mod2_1)

# lo mismo que el anterior...

# Si comparamos con el modelo elegido en (a)

summary(Mod1)

summary(Mod1_1)

# ganamos en variabilidad explicada un 0.05% 

# d ####
# Revise supuestos del modelo anterior y evalúe multicolinealidad.

# Supuestos
# linealidad

plot(Mod1_1, 1)
# pasa

# Normalidad

plot(Mod1_1, 2)

# H0: residuos son normales
# H1: residuos no son normales

nortest::lillie.test(Mod1_1$residuals)
ks.test(Mod1_1$residuals, "pnorm", mean(Mod1_1$residuals), sd(Mod1_1$residuals))
# No existe normalidad

# Homoceasticidad

# H0: Si existe Homoceasticidad
# H1: No existe Homoceasticidad

lmtest::bptest(Mod1_1)
# No existe Homoceasticidad

# Seguimos no cumpliendos los supuestos de normalidad y homoceasticidad
# aunque se nota mayor linealidad

# multicolinealidad

# estudiamos correlacion

base %>% select(pesocu, largo, diametro, alto, pesot, pesoco, anillos) %>% 
  cor %>% round(3)

base %>% select(pesocu, largo, diametro, alto, pesot, pesoco, anillos) %>% 
  cor %>% corrplot::corrplot(.)

# tenemos problemas con varios pares
# largo, diametro, alto, peso total y peso cuerpo entre ellos

car::vif(Mod1_1)

# En general, si el VIF es mayor a 5 ó 10 la variable 
# no debe ser considerada en el modelo.
# si tenemos problemas de multicolinealidad!!!


# e ####
# En caso de existir multicolinealidad proponga que variables dejar/eliminar,
# respalde su eleccion y vuelva a evaluar

# para el modelo final se debe considerar alto y anillos, quizas pesoco
# ya que estos tienen VIF menor a 10, y el ultimo es muy cercano a el

Mod1_3 <- lm(pesocu ~ alto + anillos + pesoco,
             data = base)

# volvemos a evaluar

car::vif(Mod1_3)

# luego de los cambios mejoramos los problemas de multicolinealidad

# f ####
# Para el mejor modelo obtenido en e) �Puede realizar una prediccion para una pieza 
# de abalon que tiene de largo 13, diametro 10, alto 3.5, peso total 380, 
# peso de la concha 110 y numero de anillos 9?

formula(Mod1_3)

new_data <- data.frame(alto = 3.5, anillos = 9, pesoco = 110)

predict(Mod1_3, newdata = new_data)

# dada las caracteristicas se espera un peso de cuerpo igual a 85.3944 u. 