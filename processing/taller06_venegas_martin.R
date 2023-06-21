# 0. Identificacion ----------------------------------
#Título: Codigo elaboración del Taller 6 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del sexto taller del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               TeachingDemos
               )

# 2. Cargar datos --------------------

tcm <- readxl::read_xlsx("input/data/original/TCM2020.xlsx")

# 3. Instrucciones taller -----------------

# TALLER DE INTERVALOS Y TEST

# Usted, como futuro Analista (con conocimiento de Estadística) podría desempeñarse en una gran empresa del
# Retail donde, como experto, tenga como labor determinar las áreas débiles y fuertes a identificar y proponer
# mejoras. Suponga que le realizan el siguiente requerimiento, el cual se resume en una línea: 
# Problemas con el uso de nuestra querida Tarjeta de Compra Más - TCM

# En la reunión de trabajo se determina que el uso de nuestra tarjeta presenta áreas de mejora en varios aspectos.
# Sin embargo, es necesario verificar o refutar las apreciaciones que, durante la reunión, se plantearon sobre la
# TCM. Estás se pueden resumir como sigue:

# 1. Bajo uso: dado que hasta mayo 2019 el 62% utilizó la tarjeta al menos una vez, y ahora (hasta mayo
# 2020) se cree que ese porcentaje ha bajado significativamente.

# 2. Montos: El comité de promociones discute que ellos han focalizado apropiadamente las ofertas, de tal
# forma que han incrementado los montos de compras mensuales, y para comprobar indican que en
# Mayo2019 fue de m$400, aseguran que este mes fue superior.

# 3. Antiguos: El área de fidelización de clientes (renegociación) es acusada de impedir que antiguos clientes
# incrementen sus compras al limitar sus niveles de endeudamiento. En otras palabras, entre los que usan
# la tarjeta, los clientes antiguos tiene montos medios M$50 inferiores a los clientes nuevos (las tarjetas 
# NumCliente ≤ 250.000 fueron emitidas antes de enero2010 -> antiguos).

# 4. Segmento joven ¿Hay evidencia que permita afirmar que los hombres jóvenes (grupo ≤ 35 años) de
# regiones gastan más que los hombres jóvenes de la RM?

# Para el cumplimiento de lo anterior, el Departamento de Informática les hace llegar una pequeña muestra
# aleatoria de clientes con información histórica y los atributos solicitados (ver TCM2020.xlsx)

# Para cada uno de los 4 test de hipótesis indique exactamente lo siguiente:

# 1. Defina el/los parámetros (por ejemplo, Mu = gasto medio en clientes con menos de 50 años)
# 2. Plantear las hipótesis H0 y H1
# 3. Entregue el valor del estadístico y el correspondiente valor-p
# 4. Redacte la conclusión de su decisión en el contexto de lo planteado

# Para todo evento, asuma normalidad y utilice 𝜶 = 𝟎, 𝟎𝟓

# Recuerde que tiene las herramientas necesarias en R para los realizar los test correspondientes y que le
# permiten responder de manera rápida y precisa lo que se le pregunta. (No haga cálculos manuales).

# 4. Desarrollo taller -----------------

################################################# EJERCICIO 1 ###################################################################################################

# Veamos el porcentaje de quienes usaron la tarjeta en 2020
sjmisc::frq(tcm$Uso2020)

# OJO: Aquí asumimos que Uso2020 representa el uso hasta mayo de 2020. Esto debería ser especificado de mejor manera.

# Es necesario plantear las hipótesis. Las hipótesis son las siguientes:

# Parametro: P = porcentaje de gente que usa la tarjeta hasta mayo 2020

# H0: P ≥ .62
# H1: P < .62

# Elijamos el test. Considerando que es test de proporciones, corresponde hacer un prop.test en base a una normal (aunque ojo que R lo hace con chi cuadrado).

prop.test(sum(tcm$Uso2020), # Count of sucess: total de personas que si usaron la tarjeta el 2020 (sum(Uso2020 == 1))
          NROW(tcm$Uso2020), # Count of trials: total de personas con tarjeta, independiente de si la usaron o no.
          0.62, # Probability of sucess: probabilidad que se busca testear
          alternative = "less") # alternative hypothesis: menor a la probabilidad dada (.62)

# Conclusión: No existe evidencia suficiente para sostener que la proporción de personas que utilizaron TCM hasta mayo 2020
# fue significativamente menor que la proporción de personas que utilizaron TCM hasta mayo 2019 (.62) (p-valor > .05)

################################################################################################################################################################

################################################# EJERCICIO 2 ##################################################################################################

# OJO: El planteamiento es poco preciso, se debería establecer que significa el m$400. Asumiremos que es la media.

mean(tcm %>% filter(UsoMayo == 1) %>% pull(MontoMayo)) # veamos la media

# μ: media de los montos del mes de 2020

# H0: μ ≤ 400
# H1: μ > 400

t.test(
  tcm %>% filter(UsoMayo == 1) %>% pull(MontoMayo), # sample mean: monto medio del uso en mayo 2020
  alternative = "greater", # alternative hypothesis: el monto medio de mayo 2020 es mayor a 400 (que fue el monto medio de mayo 2019)
  mu = 400 # mean for test: monto medio de mayo 2019 (m$400)
)

# Conclusión: Existe evidencia suficiente para sostener que el monto medio del mes de mayo del 2020 es significativamente 
# mayor al monto medio de mayo del año pasado (valor-p < 0.05)

################################################################################################################################################################

################################################# EJERCICIO 3 ##################################################################################################

# μ1: monto medio cliente antiguo
# μ2: monto medio cliente nuevo

# Los clientes antiguos tiene montos medios M$50 inferiores a los clientes nuevos: μ2 > μ1 + m$50
# Los clientes nuevos tienen montos medios M$50 mayores a los clientes antiguos: μ2 < μ1 - m$50

# μ2 - μ1 > 50

# H0: μ2 - μ1 ≤ 50
# H1: μ2 - μ1 > 50

# RECORDEMOS, HAY QUE COMPARAR LAS VARIANZAS PRIMERO

# σ1 = varianza de montos cliente antiguo
# σ2 = varianza de montos cliente nuevo

# H0: σ1 = σ2 
# H1: σ1 ≠ σ2 

var.test(
  tcm %>% filter(Cliente >= 250000 & Uso2020 == 1) %>% pull(MontoAcum), # Nuevos
  tcm %>% filter(Cliente <= 250000 & Uso2020 == 1) %>% pull(MontoAcum), # Antiguos
)

# Conclusión homocedeasticidad: Existe evidencia suficiente para asumir diferencia de varianzas

t.test(
  tcm %>% filter(Cliente >= 250000 & Uso2020 == 1) %>% pull(MontoAcum), # Nuevos
  tcm %>% filter(Cliente <= 250000 & Uso2020 == 1) %>% pull(MontoAcum), # Antiguos
  alternative = "greater",
  mu = 50,
  var.equal = F
)

# Conclusión: No hay evidencia suficiente para sustentar la afirmación de que la diferencia de medias entre los 
# clientes nuevos y antiguos es mayor a m$50 (valor-p > .05). O dicho de otra forma, no hay evidencia suficiente para
# plantear que el monto medio de los clientes nuevos es mayor que el monto medio de los clientes antiguos más m$50.

################################################################################################################################################################

################################################# EJERCICIO 4 ##################################################################################################

# Asumimos que están hablando del gasto medio.

# μ1: gasto medio jovenes hombres regiones
# μ2: gasto medio jovenes hombres RM

# H0: μ1 ≤ μ2 
# H1: μ1 > μ2 

# RECORDEMOS, HAY QUE COMPARAR LAS VARIANZAS PRIMERO

# σ1 = varianza de gasto jovenes hombres regiones
# σ2 = varianza de gasto jovenes hombres RM

# H0: σ1 = σ2 
# H1: σ1 ≠ σ2 

var.test(
  tcm %>% filter(Uso2020 == 1 & # Usan la tarjeta
                   Edad <= 35 & # Joven
                   `Sex(1=Fem)` == 0 & # Hombre
                   `Reg(1=RM)` == 0) %>% pull(MontoAcum), # Region
  
  tcm %>% filter(Uso2020 == 1 &
                   Edad <= 35 &
                   `Sex(1=Fem)` == 0 &
                   `Reg(1=RM)` == 1) %>% pull(MontoAcum),
  
)

# Conslusión: No hay suficiente evidencia para sostener que las varianzas en ambos grupos son distintas (valor-p > .05)

t.test(
  tcm %>% filter(Uso2020 == 1 & # Usan la tarjeta
                   Edad <= 35 & # Joven
                   `Sex(1=Fem)` == 0 & # Hombre
                   `Reg(1=RM)` == 0) %>% pull(MontoAcum), # Region

  tcm %>% filter(Uso2020 == 1 &
                   Edad <= 35 &
                   `Sex(1=Fem)` == 0 &
                   `Reg(1=RM)` == 1) %>% pull(MontoAcum),
  alternative = "greater",
  var.equal = T
)

# Conclusión: Hay suficiente evidencia para sostener que los hombres jovenes de regiones gastan significativamente más que los
# hombres jovenes de la RM.

################################################################################################################################################################