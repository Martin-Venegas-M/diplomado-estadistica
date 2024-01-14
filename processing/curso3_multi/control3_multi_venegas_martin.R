# 0. Identificacion ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Codigo elaboración del Control 2 del curso 3 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del segundo control del tercer curso del diplomado.

# 1. Cargar paquetes ------------------------
if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
if (!require("InformationValue")) devtools::install_github("selva86/InformationValue")  #si falta InformationValue, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra,
               summarytools,
               psych,
               factoextra
)

# Cargar funcion
normalizar <- function(x){
  aux <- (x - min(x))/(max(x)-min(x))
  return(aux)
}

# 2. Cargar datos -------------------------------------------------------------------------------------------------------------------------------------------

df <- read_excel("input/data/original/proteinas.xlsx")
df <- data.frame(df[, -1], row.names = df$Pais)
df <- normalizar(df)

# 3. Desarrollo ---------------------------------------------------------------------------------------------------------------------------------------------

# 3.1.	Describir las variables. Comente (comportamiento, anomalías, etc.) ----------------------------------------------------------------------------------

view(dfSummary(df))
psych::describe(df) # opcion 2 (mas opcional que nunca)
skimr::skim(df) # opcion 3 (mas opcional que nunca)

# 3.2.	ACP: Obtenga DOS Componentes Principales y grafique -------------------------------------------------------------------------------------------------

pca <- princomp(df, cor = TRUE)
pca$scores[, c(1,2)]
## resumen de pca: contribucion var
summary(pca)

## composicion de dimensiones
loadings(pca)
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

fviz_pca_var(pca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             ggtheme = theme_minimal())

## score de de los paises por dimension 1 y 2
fviz_pca_ind(pca, col.ind = "cos2", 
             axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_pca_biplot(pca, col.ind = "gray",
             axes = c(1,2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             col.var = "cos2",
             pointsize = 3,
             ggtheme = theme_minimal())

# a.	¿Qué representa el primer componente? ¿retención de varianza con 2 CP?
# b.	¿Es posible construir una “agregación” que permita identificar regiones?

# 3.3.	AFAC: Lleve a cabo un Análisis Factorial. -----------------------------------------------------------------------------------------------------------

sjPlot::tab_corr(df, triangle = "lower")

KMO(df)
cortest.bartlett(df)
plot(scree(df))

set.seed(1) # Resultado reproducible
fa.parallel(df, 
            fm = 'ml', # método máxima verosimilitud
            fa = 'fa') # tipo de autovalores: análisis factorial

# a.	¿Qué representa el primer factor? ¿cuántos factores proponen?

f1 <- fa(df, nfactors = 1, 
   fm = "ml", 
   rotate = "varimax") # sin rotación

f2 <- fa(df, nfactors = 2, 
   fm = "ml", 
   rotate = "varimax") # sin rotación

# b.	Con la solución de dos factores, realice una rotación varimax y grafique. 

plot(f2$scores[,1], 
     f2$scores[,2],
     xlab = "Factor 2: Dieta ovolacto", 
     ylab = "Factor 1: Dieta pescatariana",
     ylim = c(-3,3),
     xlim = c(-3,3))
text(f2$scores[,1]-0.03,
     f2$scores[,2]+0.03,
     rownames(df),
     col="turquoise",
     cex=0.8)
abline(h = 0, v = 0, lty = 2)

# 3.4.	Clúster: Obtenga un número apropiados de “Cluster”, ¿cómo se agrupan los países? --------------------------------------------------------------------

# Número de cluster
fviz_nbclust(x = df, 
             FUNcluster = kmeans) 

# Cluster - no jerarquico
clust1.1 <- kmeans(x = df, centers = 2)

# representacion grafica
fviz_cluster(object = clust1.1,
             data = df,
             ellipse.type = "euclid",
             repel = TRUE)


# Cluster - jerarquico
EUCL <- dist(df, method = "euclidean") 
CluJe1.1 <- hclust(EUCL, method = "complete")
fviz_dend(x = CluJe1.1, k = 2, cex = 0.6) 

# 3.5.	En no más de dos párrafos, contraste y discuta los resultados obtenidos por cada uno de los métodos -------------------------------------------------
