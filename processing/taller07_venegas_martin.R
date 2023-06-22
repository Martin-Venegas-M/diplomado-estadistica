# 0. Identificacion ----------------------------------
#Título: Codigo elaboración del Taller 7 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del septimo taller del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               nortest,
               lmtest,
               car
               )

# 2. Cargar datos --------------------

psu <- read_excel("input/data/original/Base PSU (muestra).xlsx")

psu <- psu %>% mutate(across(
  c(Lenguaje,
    Matematicas,
    Historia,
    Ciencias,
    Ranking,
    NEM_ceros),
  as.numeric
))

# 3. Instrucciones taller ------------------------

# TALLER ANOVA y otros test

# Los datos de la base PSU contiene una meustra de 1200 resultados de diferentes
# pruebas de alumnos de la RM. Las variables a considerar:

# - Sexo
# - Puntaje NEM
# - Promedio notas EM
# - Financiamiento (1 = Pagado, 2 = compartido, 3 = gratuito)
# - Régimen (1 = Particular, 2 = Municipal)
# - Prueba Lenguaje
# - Prueba Matemáticas
# - Prueba Historia
# - Prueba Ciencias
# - Ranking

# 1. Describa variables: sexo, régimen, financiamiento y estadísticas de las pruebas (✓)

# 2. Codificar ranking (usar función SI): Bajo = <= 450, Medio <= 650, Alto > 650 y tabular (por sexo, tipo..) (✓)

# 3. ¿Hay diferencia significativa en puntaje ranking, por sexo? (✓)
   
# 4. En la prueba de matemática, se piensa que los hombres de colegios particulares obtienen
# puntajes mayores que los hombres de colegios municipales. (✓)
 
# 5. Proporción de mujeres y hombres que obtienen sobre 600 puntos en Matemáticas ¿Es mayor en los
# hombres que en las mujeres (✓)
 
# 6. ¿Una mayor proporción de hombres que rinda la prueba de ciencias en relación a la proporción 
# de mujeres? (✓)
  
# 7. ¿Hay diferencia significativa en rangkin, según tipo de colegio? (✓)
   
# 8. ¿Hay diferencia significativa en puntaje prometio (Matem-Lenguaje). según fincanciamiento? (✓)
   
# 9. ¿Cuál es la correlación entre los puntajes de las 4 pruebas? (✓)
   
# 10. ¿Hay diferencia significativa en puintajes de Matem. y Lenguaje en mujeres de colegios municipales?
   
# 11. ¿Hay asociación entre tipo y ranking_cod?
  
# 4. Desarrollo taller

################################################# EJERCICIO 1 ###################################################################################################

summarytools::stview(summarytools::dfSummary(
  psu %>% select(
    sexo,
    regimen,
    financiam,
    Lenguaje,
    Matematicas,
    Historia,
    Ciencias,
    Ranking
  )
))

################################################# EJERCICIO 2 ###################################################################################################

psu <- psu %>% mutate(
  ranking_cod = case_when(
    Ranking <= 450 ~ "Bajo",
    Ranking <= 650 ~ "Medio",
    Ranking > 450 ~ "Alto",
  )
)

################################################# EJERCICIO 3 ###################################################################################################

# μ1: media de ranking en hombres
# μ2: media de ranking en mujeres

# H0: μ1 = μ2 
# H1: μ1 ≠ μ2 

# RECORDEMOS, HAY QUE COMPARAR LAS VARIANZAS PRIMERO

# σ1 = varianza de ranking en hombres
# σ2 = varianza de ranking

# H0: σ1 = σ2 
# H1: σ1 ≠ σ2 

var.test(
  psu %>% filter(sexo == "M") %>% pull(Ranking),
  psu %>% filter(sexo == "F") %>% pull(Ranking)
)

# Conclusión: No hay evidencia suficiente para rechazar el supuesto de homocedeasticidad (valor-p < .05)

t.test(
  psu %>% filter(sexo == "M") %>% pull(Ranking),
  psu %>% filter(sexo == "F") %>% pull(Ranking),
  alternative = "two.sided",
  var.equal = T
)

# Conclusión: Hay evidencia suficiente para afirmar que existen diferencias estadísticamente significativa en los ranking medios entre
# hombres y mujeres (valor-p < .05)

################################################# EJERCICIO 4 ###################################################################################################

# μ1: medias de psu matematicas de hombres colegio particular pagado
# μ2: medias de psu matematicas de hombres colegio particular comp
# μ3: medias de psu de matematicas de hombres colegio publico

# H0: μ1 = μ2 = μ3
# H1: alguna media es significativamente distinta de otra

frq(psu$Tipo_colegio)

anova_ej4 <- aov(Matematicas ~ Tipo_colegio,
                 data = psu %>% filter(sexo == "M")) 

anova_ej4 %>% summary()

# Conclusión: existen diferencias estadísticamente significativas en al menos una comparación en las medias de la psu de matematicas de acuerdo al tipo de colegio.

# Comparaciones multiples

anova_ej4 %>% TukeyHSD() %>% plot()

# Conclusión post hoc: efectivamente existe evidencia para afirmar que existen diferencias estadísticamente significativas en
# los puntajes de matematicas de hombres entre jovenes de colegios particulares y municipales, teniendo los hombres de colegios particulares
# pagados una media mayor que los de colegios municipales.

# Robustez

anova_ej4 %>% plot()

# Test normalidad
ks.test(anova_ej4$residual, "pnorm", mean(anova_ej4$residuals), sd(anova_ej4$residuals)) #OJO DA UNA ADVERTENCIA
shapiro.test(anova_ej4$residual)
nortest::ad.test(anova_ej4$residuals)

# Conclusión: no hay evidencia suficiente para rechazar la hipótesis de normalidad

# Test homocedasticidad

lmtest::bptest(anova_ej4)
car::leveneTest(anova_ej4)

# Conclusión: hay evidencia suficiente para rechazar la hipótesis de homocedasticidad

################################################# EJERCICIO 5 ###################################################################################################

# P1: Proporción de hombres que obtienen sobre 600 puntos en Matemáticas
# P2: Proporción de mujeres y hombres que obtienen sobre 600 puntos en Matemáticas

# H0: P1 ≤ P2 
# H1: P1 > P2 

prop.test(c(
  NROW(psu %>% filter(sexo == "M" & Matematicas > 600)), # Sucess first group
  NROW(psu %>% filter(sexo == "F" & Matematicas > 600)) # Succes second group
),
c(
  NROW(psu %>% filter(sexo == "M" & !is.na(Matematicas))), # Total first group
  NROW(psu %>% filter(sexo == "F" & !is.na(Matematicas))) # Total second group
),
alternative = "greater") # alternative hypothesis

# Conclusión: No hay evidencia suficiente para sostener la afirmación de que la proporción de hombres con más de 600 puntos
# en la prueba de matemáticas es significativamente mayor que la proporción de mujeres con más de 600 puntos en la prueba
# de matemáticas.

################################################# EJERCICIO 6 ###################################################################################################

# P1: Proporción de hombres que rinden prueba de ciencias
# P2: Proporción de mujeres y hombres que rinden prueba de ciencias

# H0: P1 ≤ P2 
# H1: P1 > P2 

prop.test(c(
  NROW(psu %>% filter(sexo == "M" & !is.na(Ciencias))), # Sucess first group
  NROW(psu %>% filter(sexo == "F" & !is.na(Ciencias))) # Succes second group
),
c(
  NROW(psu %>% filter(sexo == "M")), # Total first group
  NROW(psu %>% filter(sexo == "F")) # Total second group
),
alternative = "greater") # alternative hypothesis

# Conclusión: No hay evidencia suficiente para sostener la afirmación de que la proporción de hombres que rindió la prueba de ciencias
# es significativamente mayor que la proporción de mujeres que rindió la prueba de ciencias

################################################# EJERCICIO 7 ###################################################################################################

# μ1: medias de ranking colegio particular pagado
# μ2: medias de ranking colegio particular comp
# μ3: medias de ranking colegio publico

# H0: μ1 = μ2 = μ3
# H1: alguna media es significativamente distinta de otra

frq(psu$Tipo_colegio)

anova_ej5 <- aov(Ranking ~ Tipo_colegio,
                 data = psu) 

anova_ej5 %>% summary()

# Conclusión: existen diferencias estadísticamente significativas en al 
# menos una comparación en las medias del ranking de acuerdo al tipo de colegio.

# Comparaciones multiples

anova_ej5 %>% TukeyHSD() %>% plot()

# Conclusión post hoc: efectivamente existe evidencia para afirmar que existen diferencias estadísticamente significativas en
# los puntajes de matematicas de hombres entre jovenes de colegios particulares y municipales, teniendo los hombres de colegios particulares
# pagados una media mayor que los de colegios municipales.

# Robustez

anova_ej5 %>% plot()

# Test normalidad
shapiro.test(anova_ej5$residual)
nortest::ad.test(anova_ej5$residuals)

# Conclusión: hay evidencia suficiente para rechazar la hipótesis de normalidad

# Test homocedasticidad

lmtest::bptest(anova_ej5)
car::leveneTest(anova_ej5)

# Conclusión: no hay evidencia suficiente para rechazar la hipótesis de homocedasticidad

# Test no parametrico (por no normalidad)

kruskal.test(Ranking ~ Tipo_colegio, data = psu)

# Conclusión: evidencia suficiente para sosntener que existe al menos una media distinta en relación al tipo de colegio

# Comparaciones múltiples para no parametrico (por no normalidad)

pairwise.wilcox.test(psu$Ranking, psu$Tipo_colegio,
                     p.adjust.method = "BH") 

# Conclusión: idem a la de anova

################################################# EJERCICIO 8 ###################################################################################################


# μ1: medias de promedio prueba leguaje-mate compartido
# μ2: medias de promedio prueba leguaje-mate gratuito
# μ3: medias de promedio prueba leguaje-mate pagado

# H0: μ1 = μ2 = μ3
# H1: alguna media es significativamente distinta de otra

frq(psu$financiam)

psu <- psu %>%  mutate(promedio_mat_leng = (Matematicas + Lenguaje)/2)

anova_ej8 <- aov(promedio_mat_leng ~ financiam,
                 data = psu)

anova_ej8 %>% summary()

# Conclusión: existen diferencias estadísticamente significativas en al 
# menos una comparación en las medias del promedio de mate/leng de acuerdo al tipo de financiamiento/

# Comparaciones multiples

anova_ej8 %>% TukeyHSD() %>% plot()

# Conclusión post hoc: efectivamente existe evidencia para afirmar que existen diferencias estadísticamente significativas en
# el promedio de puntajes leng/mat, teniendo aquellos con financiamiento pagado una media mayor que quienes tienen financiamiento gratuito.

# Robustez

anova_ej8 %>% plot()

# Test normalidad
shapiro.test(anova_ej8$residual)
nortest::ad.test(anova_ej8$residuals)

# Conclusión: hay evidencia suficiente para rechazar la hipótesis de normalidad

# Test homocedasticidad

lmtest::bptest(anova_ej8)
car::leveneTest(anova_ej8)

# Conclusión: hay evidencia suficiente para rechazar la hipótesis de homocedasticidad

# Test no parametrico (por no normalidad)

kruskal.test(promedio_mat_leng ~ financiam, data = psu)

# Test comparaciones multiples no parametrico (por no normalidad)

pairwise.wilcox.test(psu$promedio_mat_leng, psu$financiam,
                     p.adjust.method = "BH") 

# Conclusión: idem a la de anova

################################################# EJERCICIO 9 ###################################################################################################

sjPlot::tab_corr(psu %>% select(Lenguaje, Matematicas, Ciencias, Historia),
                 triangle = "lower")

# Conclusión: Hay una relación lineal positiva estadísticamente significativa entre todas las pruebas. El par de pruebas con menor
# asociación lineal es entre matemáticas y ciencias (r = .569; p < .05) y matemáticas e historia (r = .574; p < .05)

################################################# EJERCICIO 10 ###################################################################################################

# μ1: medias de psu matematicas de mujeres colegio municipal
# μ2: medias de psu lenguaje de mujeres colegio municipal

# H0: μ1 = μ2 
# H1: μ1 ≠ μ2

t.test(
  psu %>% filter(sexo == "F" & Tipo_colegio == "Muni_Gratis") %>% pull(Matematicas),
  psu %>% filter(sexo == "F" &  Tipo_colegio == "Muni_Gratis") %>% pull(Lenguaje),
  alternative = "two.sided",
  paired = T
)

# Conclusión: no hay evidencia suficiente para sostener que los puntajes medios de lenguaje y matematicas de las mujeres de colegios municipales
# difieren significativamente

################################################# EJERCICIO 11 ###################################################################################################

