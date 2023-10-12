# Script clase 01 muestreo M.A.S.

## Cargar librerias ##
library(tidyverse)
library(readxl)
library(samplingbook)

# directorio
getwd()

base <- read_excel("input/data/original/Precenso-comunas.xlsx")
head(base)
tail(base)
# Tenemos una fila demas, con un supuesto titulo

base <- read_excel("input/data/original/Precenso-comunas.xlsx", skip = 1) # skip elimina filas
head(base)
tail(base)

names(base) <- str_replace_all(names(base), "-", "_")
head(base)

# base simulara una poblacion, que en la mayoria de los casos no se tiene.
# poblacion: manzanas de la comuna de La Reina, Las Condes y Peñalolén 
# de la cual podemos obtener la poblacion de mujeres y hombres

## ---------------------------------------------------------------------------##
## Recordar que vamos a muestrear manzanas no personas
## Nos interesa la proporcion de mujeres en el territorio

## parametro: proporcion
## Podriamos apostar que la proporcion de mujeres es de 0.6
## y queremos de error a lo mas del 5%

## Tamaño poblacional
## Parentesis(
# supongamos que hay 350 manzanas
# -> N = 350, e = 0.05, P = 0.6

sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = 350)
## Sample size needed: 180

# Que pasaria si queremos un error menos, mas pequeño: del 1% (0.01)
sample.size.prop(e = 0.01,
                 P = 0.6,
                 N = 350)
## Sample size needed: 338
## Cerramos Parentesis)

## Alguien nos sopla que la poblacion es de 3569
# -> N = 3569, e = 0.05, P = 0.6
sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = 3569)
## Sample size needed: 335

## que pasa cuando sabemos que es grande el N, pero no exactamente cuanto es
sample.size.prop(e = 0.05,
                 P = 0.6,
                 N = Inf)
## Sample size needed: 369

## Ahora, si desconocemos totalmente P, proporcion de mujeres
## Metodo conservador
## Asumir que P es 0.5 (50%)
sample.size.prop(e = 0.05,
                 P = 0.5,
                 N = Inf)
## Sample size needed: 385

## Decidimos entrevistar a 385... asi tenemos a lo mas un 5% de error para 
## una poblacion grande
## FIN

## -------------------------------------------------------------------------- ##
## Si nos interesa el total de mujeres en el territorio
## Se nos viene a la mente para el total: media*N total de manzanas

## Conocimiento previo o experto S = 100
## Error cercano a 12

sample.size.mean(e = 12,
                 S = 100,
                 N = Inf)
# Sample size needed: 267

## si aumentamos el error a 50
sample.size.mean(e = 50,
                 S = 100,
                 N = Inf)
# Sample size needed: 16

## si disminuye el error a 5
sample.size.mean(e = 5,
                 S = 100,
                 N = Inf)
# Sample size needed: 1537

## En caso de no disponer de la desviacion o tenemos muy mala informacion:

## Metodo conservador
## minimo?  <-- 1 persona
## maximo?  <-- 500 personas
## Una estimacion conservador para S seria de la forma
## S = (max - min)/6
## si es que yo creo, que estos valores cubren el 95% de los casos
## entonces ahi tendriamos que ocupar S = (max - min)/4

max = 500; min = 1
s1 = (max-min)/6; s2 = (max-min)/4
c(s1, s2)

## tamaños muestrales minimos
sample.size.mean(e = 12,
                 S = s1,
                 N = Inf)
# Sample size needed: 185

sample.size.mean(e = 12,
                 S = s2,
                 N = Inf)
# Sample size needed: 416

## en este caso elegimos muestrear 416 manzanas, con un error de a lo mas
## 12 personas, una poblacion grande y utilizando un metodo conservador 
## para S.


## tenemos dos tamaños muestreales minimos a priori:
## para la proporciones nos piden 385
## para la media nos piden        416

## Debemos elegir muestrear 416 (unicamente) si queremos el estudio de 
## ambos parametros


## ------------------------------------------------------------- ##

## Seleccion de muestra
n <- 416
N <- 3569

## semilla - con fines academicos
set.seed(20231005)

## Seleccionamos manzanas sin repetirlas
muestra <- sample(1:N, size = n, replace = FALSE)
muestra

## Separamos la muestra (alias salimos a muestrear manzanas identifacas
## en la comuna de La Reina, Las Condes y Peñalolén)
base_muestra <- base[muestra, ]

## Calcumos nuestras estimaciones
## total de mujeres por manzana (media * N)
N * mean(base_muestra$Población_M)
# o
N * Smean(base_muestra$Población_M, N = N)$mean

## Intervalos de confianza
N * Smean(base_muestra$Población_M, N = N)$ci # Recordar que es para 
# el total (no media) en este ejercicio

## (parentesis
## comparemos con la poblacion (cosa que no deberiamos tener)
sum(base$Población_M)
mean(base$Población_M)
## cierre parentesis)

## proporcion de mujeres por manzana
## proporcion: casos favorables / casos posibles
sum(base_muestra$Población_M)/sum(base_muestra$Población_T) -> p_est; p_est

## por qué no utilizamos Sprop? solo porque la variable a medir
## no es binaria de 1 y 0, entonces debemos recurrir al calculo 
## convencional bajo un M.A.S.

## intervalos de confianza
error = qnorm(0.975)*sqrt(p_est*(1-p_est)/(n-1)*(1-n/N))
error
# i.c.
c(p_est - error, p_est + error)

## (parentesis
## comparemos con la poblacion (cosa que no deberiamos tener)
sum(base$Población_M)/sum(base$Población_T)
## cierre parentesis)









