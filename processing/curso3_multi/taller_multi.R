
# Taller Multivariado

# La base de datos VRegion.xlsx, contiene información respecto a diversas 
# variablesasociadas a la administración de cada una de las comunas de la 
# región de Valparaíso.

library(readxl)
library(dplyr)
library(factoextra)

normalizar <- function(x){
        aux <- (x - min(x))/(max(x)-min(x))
        return(aux)
}

##### 1 #####

valparaiso <- read_excel("Bases de datos/VRegionT.xlsx")
head(valparaiso) 
glimpse(valparaiso)

# corregimos nombres (opcional)
names(valparaiso) <- stringr::str_replace_all(names(valparaiso), "\\.", "_")
glimpse(valparaiso)

# resumimos los datos
summary(valparaiso)  # opcion 1
psych::describe(valparaiso) # opcion 2 (mas opcional que nunca)
skimr::skim(valparaiso) # opcion 3 (mas opcional que nunca)

# al parecer tenemos algunos datos atipicos en pc_pmb y Def_Habitac, entre otras
# tambien se observan escalas muy distintas entre variables

# definimos nombre de filas
valparaiso <- data.frame(valparaiso[, -1], row.names = valparaiso$COMUNA)

# revisamos comportamiento de variables
cor_valparaiso <- cor(valparaiso)
round(cor_valparaiso, 2)

library(corrplot) 
corrplot(cor_valparaiso, order = "hclust", method = "ellipse") # (opcional)
ggcorrplot::ggcorrplot(cor_valparaiso)

##### 2.a #####

pca <- princomp(valparaiso, cor = TRUE)

## resumen de pca: contribucion var
summary(pca)

## composicion de dimensiones
loadings(pca)[, c(1,2)]

## Grafico de contribucion var por componentes
fviz_contrib(pca,
             choice = "var",
             axes = 1, top = 10, ylim = c(0,100)) # (mas opcional que nunca)

# R: 
## Componente 1: fondo comun municipal, Deficit habitacional,
##               programa de mejoramiento de barrios y presupuesto
## plus (score): explicar el resultado en cuanto a su signo 
## Componente 1: "Presupuesto y deficit habitacional" (no necesariamente es claro)
## 2 componentes explican un 49% de la variabilida total


##### 2.b #####

fviz_pca_var(pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal())

## score de de los paises por dimension 1 y 2
fviz_pca_ind(pca, col.ind = "cos2", 
             axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())


##### 3.a #####
psych::KMO(valparaiso) # KMO

# 1.0 >= KMO >= 0.9 muy bueno
# 0.9 >= KMO >= 0.8 meritorio
# 0.8 >= KMO >= 0.7 mediano
# 0.7 >= KMO >= 0.6 mediocre
# 0.6 >= KMO >= 0.5 bajo
#        KMO <= 0.5 inaceptable

# variables como Ingreso Promedio, Viv. buenas o Tasa de no pobreza
# convendria quitarlas

## H0: es suficiente k factores vs H1: son insuficientes k factores
afl1 <- factanal(valparaiso, factors = 1)
afl1 
afl2 <- factanal(valparaiso, factors = 2)
afl2
afl3 <- factanal(valparaiso, factors = 3)
afl3
afl4 <- factanal(valparaiso, factors = 4)
afl4

# con 1 factor ya es suficiente, quizas 2 sea mejor para interpretar

afl2_2 <- factanal(valparaiso, factors = 2, rotation = "varimax", scores = "regression")
afl2_2

## uniquenesses: singularidades: varianza de cada variable que "no es comun".
## se recomienda quitar variable cuando es superior a 0.7
# como Ingreso Promedio, Viv. buenas o Tasa de no pobreza

afl2_2$loadings
print(afl2_2$loadings, digits = 3, cutoff = 0.26, sort = TRUE) # ayuda a visualizacio 

# R:
# Se proponen 2 factores para facilitar interpretacion
## Factor 1: fondo comun municipal, Deficit habitacional, Gini
##          -> 

##### 3.b #####
afl2_2$loadings
print(afl2_2$loadings, digits = 3, cutoff = 0.26, sort = TRUE) # Ordenamos cada factor  

## Factor 2: Presupuesto, programa de mejoramiento de barrios, tasa no pobreza
##          -> 

##### 3.c #####

# Grafico manual scores
plot(afl2_2$scores[,1], 
     afl2_2$scores[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-3,3),
     xlim = c(-3,3))
text(afl2_2$scores[,1]-0.03,
     afl2_2$scores[,2]+0.03,
     rownames(valparaiso),
     col="blue",
     cex=0.6)
abline(h = 0, v = 0, lty = 2)


##### 4.a #####

# normalizamos por idh o estandarizacion
valparaiso_ihd <- valparaiso %>% mutate_if(is.numeric, normalizar)
valparaiso_scale <- scale(valparaiso) # resta la media y divide por sd # (mas opcional que nunca)

# Número de cluster
fviz_nbclust(x = valparaiso, 
             FUNcluster = kmeans) 

# Cluster - no jerarquico
clust1.1 <- kmeans(x = valparaiso, centers = 3)

# representacion grafica
fviz_cluster(object = clust1.1,
             data = valparaiso_scale,
             ellipse.type = "euclid",
             repel = TRUE)


# Cluster - jerarquico
EUCL <- dist(valparaiso, method = "euclidean") 
CluJe1.1 <- hclust(EUCL, method = "complete")
fviz_dend(x = CluJe1.1, k = 3, cex = 0.6) 
