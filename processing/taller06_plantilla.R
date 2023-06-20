# Base de datos ####

library(dplyr)
library(readxl)
Base <- read_excel("Bases de datos/TCM2020.xlsx")
head(Base)

# Bajo uso ####

# P = proporción de clientes que usaron la tarjeta en el último año (mayo 2019 a mayo 2020)

# H0: P >= 0.62
# H1: P < 0.62

uso <- Base$Uso2020    # Selecciona solo la variable uso 2020   

total_uso <- sum(uso) # Total de personas que si la usaron el 2020
n_uso <- length(uso) # Total de personas

prop.test(total_uso, n_uso, p = 0.62, alternative = "less", conf.level = 0.95) # Hace el test de proporciones

# Con alternativa = less para probar menor que
# Prueba con un valor de proporcion de 0.62 de referencia

# Valor p mayor a 0.05 no rechaza H0
# Por lo tanto: La proporcion de el uso de tarjeta del ultimo año no es menor a la proporcion de hasta mayo 2019


# ¿Que pasa si nos interesa un i.c. para el parametro de la poblacion, 
# proporcion de personas que si usaron la tarjeta el 2020

test <- prop.test(total_uso, n_uso, alternative = "two.sided", conf.level = 0.95) # 
test$conf.int

# Y si lo queremos manual?

p_est <- total_uso/n_uso
q <- qnorm(1-0.05/2)
error_prima <- sqrt( (p_est*(1-p_est))/n_uso )

# ic
c(p_est - q*error_prima, p_est + q*error_prima)



# Montos ###

# mu: monto medio de compra de mayo 2020

# monto medio de compra en mayo 2019 fue de m$ 400

# H0: mu <= 400
# H1: mu > 400

uso_mayo <- Base %>% filter(UsoMayo == 1) # solo clientes que utilizan la TCM
# en mayo 2020
nrow(uso_mayo)
monto_mayo <- uso_mayo$MontoMayo


t.test(monto_mayo, mu = 400, alternative = "greater", conf.level = 0.95) # test de media

# t = 3.1324
# g.l = 167
# valor-p = 0.001024

# valor-p es menor a 0.05, se rechaza H0
# Existe evidencia para concluir que el monto promedio de compra de mayo 2020
# es mayor a los m$400 de mayo 2019

# ¿Que pasa si nos interesa un i.c. para el parametro de la poblacion, 

test <- t.test(monto_mayo, alternative = "two.sided", conf.level = 0.95) 
test$conf.int

test <- t.test(monto_mayo, alternative = "two.sided", conf.level = 0.99) 
test$conf.int


# Y si lo queremos manual?

n <- length(monto_mayo)
mu_est <- mean(monto_mayo)
error_prima = sd(monto_mayo)/sqrt(n)
q <- qt(1-0.05/2, n - 1)

# i.c
c(mu_est - q*error_prima, mu_est + q*error_prima)



# Antiguos ####

# mua : monto medio de compra para clientes antiguos
# mun :monto medio de compra para clientes nuevos

# mu_0 = m$50

# H0: mun - mua <= 50
# H1: mun - mua > 50            <-> mun > mua + 50

si_uso_2020 <- Base %>% filter(Uso2020 == 1) # ocupa la TCM entre may 2019 
# y may 2020


antiguos <- si_uso_2020 %>% filter(Cliente <= 250000)
monto_antiguos <- antiguos$MontoAcum

# lo mismo para los nuevos


nuevos <- si_uso_2020 %>% filter(Cliente > 250000)
monto_nuevos <- nuevos$MontoAcum


# H0 sigma_n = sigma_a 
# H1: sigma_n != sigma_a


var.test(monto_antiguos, monto_nuevos, alternative = "two.sided", conf.level = 0.95)

# valor.p 0.04633
# se rechaza H0
# varianza distintas

t.test(monto_nuevos, monto_antiguos, mu = 50, alternative = "greater", 
       conf.level = 0.95, var.equal = FALSE)

# valor.p 0.1346
# no se rechaza H0
# No hay suf evidencia para concluir que los nuevos gastan mas que los antiguos
# en m$50


# Segmento Joven ####


# mu1: monto medio de compra para hombres jovenes de regiones
# mu2: monto medio de compra para hombres jovenes de RM

# H0: mu1 - mu2 <= 0
# H1: mu1 - mu2 > 0         <-> mu1 > mu2

si_uso_2020$`Sex(1=Fem)`

si_uso_hombres <- si_uso_2020 %>% filter(`Sex(1=Fem)` == 0)
hombres_jovenes <- si_uso_hombres %>% filter(Edad <= 35)


hombres_rm <- hombres_jovenes %>% filter(`Reg(1=RM)` == 1)
monto_hombres_rm <- hombres_rm$MontoAcum


hombres_regiones <- hombres_jovenes %>% filter(`Reg(1=RM)` == 0)
monto_hombres_regiones <- hombres_regiones$MontoAcum


# H0 sigma_n = sigma_a 
# H1: sigma_n != sigma_a

var.test(monto_hombres_rm, monto_hombres_regiones, alternative = "two.sided", conf.level = 0.95)

# valor.p 0.2892 < 0.05 # FALSE
# NO se rechaza H0
# LAS varianza NO SON distintas

t.test(monto_hombres_regiones, monto_hombres_rm, mu = 0, alternative = "greater", 
       conf.level = 0.95, var.equal = TRUE)

# valor.p 0.02648
# se rechaza H0
# Concluimos que los hombres jovenes de region si gastan mas que 
# los hombres jovenes de la RM


###################################
# Se rechaza H0 si pvalue < alpha #
###################################
