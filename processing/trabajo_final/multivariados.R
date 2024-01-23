
###########################################################################
# 0. Identificación -------------------------------------------------------
#Título: Código de multivariados de EPSOC 2018
#Institución: PUC
#Encargado: Martín Venegas - Estudiante

# Resumen ejecutivo: El presente documento contiene el código para calcular los
# los multivariados de EPSOC 2018 procesada
# El objetivo es poder contar con tablas y gráficos para la realización
# del trabajo final
# El producto de este script serán distintas tablas y gráficos de multivariados
###########################################################################

# 1. Cargar librerías -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar

pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               summarytools,
               sjlabelled,
               car,
               haven,
               psych,
               lavaan,
               semTable,
               semPlot
               )

# 2. Cargar datos ---------------------------------------------------------

epsoc <- readRDS("input/data/proc/epsoc_proc.RDS")


# 3. EFA ------------------------------------------------------------------

bsjo <- epsoc %>% select(
  IGUAL1 = bsjo1,
  IGUAL2 = bsjo2,
  IGUAL3 = bsjo3,
  NEC1 = bsjo4,
  NEC2 = bsjo5,
  NEC3 = bsjo6,
  EQUI1 = bsjo7,
  EQUI2 = bsjo8,
  EQUI3 = bsjo9,
  EQUI4 = bsjo13,
  EQUI5 = bsjo14,
  DER1 = bsjo10,
  DER2 = bsjo11,
  DER3 = bsjo12
  )

sjPlot::tab_corr(bsjo, triangle = "lower")

# KMO
KMO(bsjo)

# Prueba de esfericidad de Bartlett
cortest.bartlett(bsjo)

# Gráfico de sedimentación con autovalores
plot(scree(bsjo))

# Análisis paralelo
set.seed(06051997) # Resultado reproducible
fa.parallel(bsjo, 
            fm = 'ml', # método máxima verosimilitud
            fa = 'fa') # tipo de autovalores: análisis factorial

# Ver comunalidades en solución de 4 factores

fa(bsjo, nfactors = 4, 
   fm = "ml", 
   rotate = "varimax") # rotación varimax

fa(bsjo, nfactors = 3, 
   fm = "ml", 
   rotate = "varimax") # rotación varimax

fa(bsjo, nfactors = 2, 
   fm = "ml", 
   rotate = "varimax") # rotación varimax

# 4. CFA ------------------------------------------------------------------

# Definir modelo de medición

mod1 <- '
igualdad =~  IGUAL1 + IGUAL2 + IGUAL3
necesidad =~ NEC1 + NEC2 + NEC3
equidad =~   EQUI1 + EQUI2 + EQUI3
derecho =~   DER1 + DER2 + DER3
'

mod2 <- '
igualdad =~  IGUAL1 + IGUAL2 
necesidad =~ NEC1 + NEC2
equidad =~   EQUI1 + EQUI2
derecho =~   DER1 + DER2
'

mod3 <- '
igualdad =~  IGUAL1 + IGUAL2 + IGUAL3
necesidad =~ NEC1 + NEC2 + NEC3
equidad =~   EQUI1 + EQUI2 + EQUI3 + EQUI4 + EQUI5
derecho =~   DER1 + DER2 + DER3
'

#bsjo <- bsjo %>% add_column(epsoc_proc %>% select(sexo_rec, hijos_rec, clase_perc_rec))

# Ajustar modelo CFA
mod1_cfa <- cfa(mod1, data = bsjo)
mod2_cfa <- cfa(mod2, data = bsjo)
mod3_cfa <- cfa(mod3, data = bsjo)


# Resultados
## Salida general
# summary(mod1_cfa,
#         standardized = TRUE, # mostrar cargas estandarizadas
#         fit.measures = TRUE) # mostrar índices de ajuste extendidos
# 
# summary(mod2_cfa,
#         standardized = TRUE, # mostrar cargas estandarizadas
#         fit.measures = TRUE) # mostrar índices de ajuste extendidos
# 
# summary(mod3_cfa,
#         standardized = TRUE, # mostrar cargas estandarizadas
#         fit.measures = TRUE) # mostrar índices de ajuste extendidos

# Ver solo índices de ajuste
fitmeasures(mod1_cfa, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea"))
fitmeasures(mod2_cfa, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea"))
fitmeasures(mod3_cfa, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea"))

semPaths(mod1_cfa, 
         whatLabels="std", 
         layout="tree2", 
         title = FALSE, # Elimina el título
         rotation = 4,
         sizeMan=5, sizeLat=15, # Ajusta el tamaño de los nodos
         edge.label.cex = 1, # Ajusta el tamaño del texto de las etiquetas de las aristas
         nCharNodes = 0, # Muestra todos los caracteres de los nombres de los nodos
         curvature = 0.5, # Ajusta la curvatura de las aristas
         residuals = FALSE, # Muestra las varianzas de los errore
         exoCov = F,
         shapeLat = "ellipse",
         shapeMan = "rectangle"
)

semPaths(mod2_cfa, 
         whatLabels="std", 
         layout="tree2", 
         title = FALSE, # Elimina el título
         rotation = 4,
         sizeMan=5, sizeLat=15, # Ajusta el tamaño de los nodos
         edge.label.cex = 1, # Ajusta el tamaño del texto de las etiquetas de las aristas
         nCharNodes = 0, # Muestra todos los caracteres de los nombres de los nodos
         curvature = 0.5, # Ajusta la curvatura de las aristas
         residuals = FALSE, # Muestra las varianzas de los errore
         exoCov = F,
         shapeLat = "ellipse",
         shapeMan = "rectangle"
)

# 5. SEM ------------------------------------------------------------------

## Especificar el modelo: medición y estructural
m_sem1 <- '
# Modelo medición
igualdad =~  IGUAL1 + IGUAL2 + IGUAL3
necesidad =~ NEC1 + NEC2 + NEC3
equidad =~   EQUI1 + EQUI2 + EQUI3 + EQUI4 + EQUI5
derecho =~   DER1 + DER2 + DER3

  # Modelo estructural
igualdad ~  sexo_rec
necesidad ~ sexo_rec
equidad ~   sexo_rec
derecho ~   sexo_rec
'

## Ajustar modelo
f_sem1 <- sem(m_sem1, data = bsjo)

## Ver resultados completos
summary(f_sem1, fit.measures = T, standardized = T, 
        rsquare = T, modindices = T)

## Exportar tablas
### Ajustar versión estandarizada
f_sem1_std <- sem(m_sem1, data = datos, std.lv = T, std.ov = T)
semTable(f_sem1_std, type = "html", 
         paramSets = c("loadings", "slopes", "fits", "latentcovariances"),
         file = "resultados_actividad7")
