## Librerias
library(samplingbook)
library(readxl)
library(dplyr)

## Requerimiento:
## Nos piden estimar el rendimiento de MAT con un error no superior a 3 puntos y que 
## la proporción de establecimientos en condición de riesgo (rendimiento en SIMCE MAT < 200)
## estimada presente un error menor o igual al 2%.

## Material:
## Se dispone del listado de los RBD de los 3632 establecimientos

## Informacion:

## Región	           Rendimiento SIMCE     	    Establecimientos  en riesgo %    N
##                   Promedio	Desv. Estándar	
## Biobío	             249	      35	                        10.2%              1.058
## Metropolitana	     255	      30	                         3.8%              1.808
## Valparaíso	         240	      25	                         5.4%                766

## Tamaño de muestra 

## Para la media, queremos un error no mayor a 3 puntos
## necesitamos Nh y Sh

Sh = c(35, 30, 25)
Nh = c(1058, 1808, 766)
stratasize(e = 3, Nh = Nh, Sh = Sh, type = "prop")
## total sample size determinated: 361

## Para la proporcion, queremos un error no mayor a 0.02 (2%)
PQ = c(0.102*0.898, 0.038*0.962, 0.054*0.946) # p*(1-p)
Sh_prop = sqrt(PQ)
Nh = c(1058, 1808, 766)
stratasize(e = 0.02, Nh = Nh, Sh = Sh_prop, type = "prop")
## total sample size determinated: 467

## Con cual nos quedamos?
max(361, 467) # 467

## Este tamaño de muestra es global y necesitamos separar
stratasamp(n = 467, Sh = Sh_prop, Nh = Nh, type = "prop")

## Muestras por region
RBD <- readxl::read_excel("Bases de datos/Establecimientos.xlsx")
head(RBD)
nrow(RBD)
table(RBD$Region)
Nh = table(RBD$Region)

## Muestreo
set.seed(1010)

n1 = 136
muestra1 <- RBD %>% filter(Region == "DEL BIOB?O") %>% 
  sample_n(n1)

n2 = 232
muestra2 <- RBD %>% filter(Region == "METROPOLITANA DE SANTIAGO") %>% 
  sample_n(n2)

n3 = 98
muestra3 <- RBD %>% filter(Region == "DE VALPARA?SO") %>% 
  sample_n(n3)

muestraR <- rbind(muestra1, muestra2, muestra3)
nrow(muestraR)

nh <- table(muestraR$Region)
Nh; nh

## Recoleccion de la informacion
head(muestraR)

## Establecimientos en riesgo
muestraR$riesgo <- ifelse(muestraR$MAT < 200, 1, 0)
head(muestraR, 20)

## estimaciones
stratamean(muestraR$MAT, muestraR$Region, Nh, eae = TRUE)
stratamean(muestraR$riesgo, muestraR$Region, Nh, eae = TRUE)

## error estandar: desviacion estandar / sqrt(n)
