# 0. Identificacion ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Codigo elaboración del Control 2 del curso 2 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del segundo control del segundo curso diplomado.

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
               InformationValue,
               caret,
               pROC
)

# 2. Cargar datos -------------------------------------------------------------------------------------------------------------------------------------------

df <- read_excel("input/data/original/fumaedad.xlsx") %>% rename(fuma = FUMA, sexo = sex)

# 3. Recodificaciones y etiquetado --------------------------------------------------------------------------------------------------------------------------

## Primero veamos frecuencias

frq(df$fuma) # Se ha usado FUMA=1 implica “SI” y FUMA=2 implica “NO”. 
frq(df$sexo) # SEX=1 cuando es MASC y SEX=0 en caso contrario (es decir, Femen). 
frq(df$edad)

str(df) # ojo que todas son numericas

## Recodifiquemos para que sea más facil hacer el control

# FUMAR
df$fuma <- car::recode(df$fuma, 
                       "
                       2 = 0;
                       1 = 1
                       ", as.factor = TRUE)

# SEXO
df$sexo <- as.factor(df$sexo)

## Ver frecuencias post rec
frq(df$fuma) 
frq(df$sexo) 

## Etiquetemos

df$fuma <- sjlabelled::set_labels(df$fuma, labels = c("No fuma", "Fuma"))
df$sexo <- sjlabelled::set_labels(df$sexo, labels = c("Femenino", "Masculino")) 


# 4. Realización del taller ---------------------------------------------------------------------------------------------------------------------------------

#################################### INTRODUCCION ###########################################################################################################

# 1 - {3 puntos} Cálculo de OR y ajuste de regresión logística. ¿Sexo implica Fumar?-------------
#
# 1.1 Se recoge información respecto al fumar mediante entrevistas a 100 alumnos. ----------------------
# Parte de los resultados se presentan en la siguiente tabla (los datos están en fumaedad.xlsx):

# Sexo		No Fuma Si Fuma
# Femen    26	     24	 
# Masc     16	     34	 

sjt.xtab(df$sexo, df$fuma, show.row.prc = T) # OJO QUE EL CHI2 AQUI ESTÁ CONSIDERANDO LA CORRECCIÓN DE YATES

# P(Fuma | Masc)    = _______68%____ {0.1 puntos}.
# P(Fuma | Fem)     = _______48%___ {0.1 puntos}.
# P(No Fuma | Masc) = _______32%___ {0.1 puntos}.
# P(No Fuma | Fem)  = _______52%___ {0.1 puntos}.

# 1.2 ¿Cuál es la hipótesis de interés? ------------

# H0: El fumar es independiente del sexo 
# H1: Fumar depende del sexo {0.1 pts}. 

chisq.test(df$sexo, df$fuma, correct = F) # OJO! no se considera corrección de Yates

# INTERPRETACIÓN: En conclusión, con un nivel de confianza del 95%, existe evidencia suficiente para rechazar la hipótesis nula de independencia entre sexo y fumar (valor p = .043 < .05). Es decir, existe una relación estadísticamente significativa entre el sexo y el fumar.

# 1.3 Con el objeto de cuantificar esa asociación se obtiene de la tabla -----------

m_sexo <- glm(fuma ~ sexo, data = df, family = "binomial") # Estimar modelo

summary(m_sexo) # Ver summary

sjPlot::tab_model(m_sexo) # Ver OR

# INTERPRETACIÓN: Los hombres tienen 2.3 veces más chances de fumar que las mujeres. Dicho de otra forma, los hombres tienen un 130% más de chances de fumar que las mujeres.

# 1.4 ¿Qué otras variables son relevantes incluir en el análisis (indique al menos dos)? -------

# PENDIENTE

# 1.5 Ahora, verifique el OR obtenido a través de una regresión logística con R (o software), revise bien los datos: ------

# P(Fumar | Femen) = exp(Intercepto = -0.08004) / {1 + exp(Intercepto = -0.08004)} = ______________, siendo a = Intercepto = -0.08004.

p_fumar_femen = exp(m_sexo$coefficients[1])/ (1 + exp(m_sexo$coefficients[1]))

# P(Fumar | Masc)  = exp(Intercepto = -0.08004 + Logodds_masculino = 0.83381) / {1 + exp(Intercepto = -0.08004 + Logodds_masculino = 0.83381)} = _______0.68_______, siendo b = ________Log odds para masculino____. {0.3 puntos}

p_fumar_masc = exp(m_sexo$coefficients[1] + m_sexo$coefficients[2])/ (1 + exp(m_sexo$coefficients[1] + m_sexo$coefficients[2]))


# 2. {3 puntos} Como usted determinó, un factor relevante y ausente del análisis podría ser la edad. 
# Así, que los datos anteriores son presentados, incluyendo la edad, en formato “por caso - línea”, y se han recodificado para efecto de ajustar e interpretar la regresión logística. 
# Se ha usado FUMA=1 implica “SI” y FUMA=2 implica “NO”. De igual forma, se recodifico SEX=1 cuando es MASC y SEX=0 en caso contrario (es decir, Femen). 
# Así, se interpreta la presencia (Sex=1) con respecto a la ausencia (sex=0).

# Los datos de acuerdo con esta estructura se encuentran en fumaedad.xlsx – hoja fumar (ver encabezado)

# La idea es que en esta segunda etapa incluya en los análisis EDAD, dado que se piensa que es un factor relevante. 
# A realizar. Se propone realizar los siguientes ajustes 

# a)	Fuma  Sexo 
# b)	Fuma  Edad			                                           ¿edad en forma continua o categórica? ____________________________.
# c)	Fuma  sexo + edad	

# Para a), b) y c) resuma e interprete los resultados obtenidos. 
# Indicando si sexo y/o edad es factor de riesgo o protector {0.3 puntos}, 
# evalué el OR y su interpretación {0.3 puntos} e 
# indique significancia {0.3 puntos}. 

# Finalmente ¿qué modelo prefiere? {0.3 puntos}

m_sexo <- glm(fuma ~ sexo, data = df, family = "binomial") # Estimar modelo
m_edad <- glm(fuma ~ edad, data = df, family = "binomial") # Estimar modelo
m_sexo_edad <- glm(fuma ~ sexo + edad, data = df, family = "binomial") # Estimar modelo

summary(m_sexo)
summary(m_edad)
summary(m_sexo_edad)

sjPlot::tab_model(
  m_sexo, m_edad, m_sexo_edad, 
  #transform = NULL,
  show.ci = FALSE,
  pred.labels = c("(Intercepto)", "Sexo: Masculino (ref = Femenino)", "Edad"),
  dv.labels = c("Modelo sexo", "Modelo edad", "Modelo completo"),
  p.style = "stars",
  show.aic = TRUE,
  #show.dev = TRUE,
  #show.loglik = TRUE,
  show.obs = TRUE,
  show.r2 = TRUE
  )

performance::r2_nagelkerke(m_sexo)
performance::r2_nagelkerke(m_edad)
performance::r2_nagelkerke(m_sexo_edad)

# Las interpretaciones son las siguientes:

# En el modelo que solo incluye sexo, se observa una relación estadísticamente significativa al 95% de confianza entre sexo y fumar, en tanto los hombres tienen 2.3 veces más chances de fumar que las mujeres.
# En el modelo que solo incluye edad, también se observa una relación estadísticamente significativa al 99.9% entre edad y fumar, en donde por cada año las chances de fumar aumentan en 67%
# En el modelo completo que la significancia de ambas variables se mantiene, sin embargo las magnitudes de los OR aumentan. En el caso de sexo, los hombres tienen 3.51 veces más chances de fumar que las mujeres, asumiendo la edad constante. 
# Asimismo, por cada año de edad las chances de fumar aumentan en 75%, controlando por el sexo de los estudiantes.

# En suma, tanto el sexo (ser hombre) como la edad son factores de riesgo para fumar

set.seed(1)
filas_validacion <- sample(1:nrow(df), # del total de filas
                           0.3 * nrow(df), # saca una muestra aleatoria del 20%
                           replace = FALSE # sin reemplazo (no se pueden repetir filas)
) ## dejaremos un 20% de la data para validacion

df$FILTRO <- "ENTRENAMIENTO" # crear variable con la etiqueta de "ENTRENAMIENTO"
df$FILTRO[filas_validacion] <- "VALIDACION" # al 20% muestreado anteriormente, cambia la etiqueta por "VALIDACION"

# Ver frecuencias
frq(df$FILTRO) # todo bien

# Filtrar bases
## DATA DE ENTRENAMIENTO
dfe <- df %>% filter(FILTRO == "ENTRENAMIENTO") 

## DATA DE VALIDACION
dfv <- df %>% filter(FILTRO == "VALIDACION")

# Estimar modelos
m1 <- glm(fuma ~ sexo, data = dfe, family = "binomial") 
m2 <- glm(fuma ~ edad, data = dfe, family = "binomial") 
m_comp <- glm(fuma ~ sexo + edad, data = dfe, family = "binomial") 


# Predecir valores
# DATA DE ENTRENAMIENTO
dfe <- dfe %>% mutate(
  fuma_p_m1 = m1$fitted.values,
  fuma_p_m2 = m2$fitted.values,
  fuma_p_mcomp = m_comp$fitted.values
)

# DATA DE VALIDACION
dfv <- dfv %>% mutate(
  fuma_p_m1 =  predict(m1, dfv, type = "response"),
  fuma_p_m2 =  predict(m2, dfv, type = "response"),
  fuma_p_mcomp =  predict(m_comp, dfv, type = "response")
)


# AUROC
# Recordemos...
# - Si AUC = 0.5, entonces, no hay discriminación.
# - Si 0.7 =< AUC < 0.8, la discriminación se considera aceptable
# - Si 0.8 =< AUC < 0.9, la discriminación se considera excelente
# - Si AUC => 0.9, la discriminación se considera sobresaliente

##### InformationValue package

# PLOT ROC CURVE
InformationValue::AUROC(actuals = dfe$fuma,
                          predictedScores = dfe$fuma_p_m1)

InformationValue::AUROC(actuals = dfe$fuma,
                        predictedScores = dfe$fuma_p_m2)

InformationValue::AUROC(actuals = dfe$fuma,
                        predictedScores = dfe$fuma_p_mcomp)

#### DATA ENTRENAMIENTO
par(pty = "s", mfrow = c(1, 2)) # short y dos graficos

roc(dfe$fuma, dfe$fuma_p_m1, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="Tasa de Falsos Positivos (TFP)", ylab="Tasa de Verdaderos Positivos (TVP)", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.y=60)

plot.roc(dfe$fuma, dfe$fuma_p_m2, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=50)

plot.roc(dfe$fuma, dfe$fuma_p_mcomp, percent=TRUE, col="#596777", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Modelo Sexo", "Modelo Edad", "Modelo Completo"), col=c("#377eb8", "#4daf4a", "#596777"), lwd=4, cex=0.8)
title(main = "Curva ROC - Data entrenamiento")

# DATA VALIDACION

roc(dfv$fuma, dfv$fuma_p_m1, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="Tasa de Falsos Positivos (TFP)", ylab="Tasa de Verdaderos Positivos (TVP)", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.y=60)

plot.roc(dfv$fuma, dfv$fuma_p_m2, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=50)

plot.roc(dfv$fuma, dfv$fuma_p_mcomp, percent=TRUE, col="#596777", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Modelo Sexo", "Modelo Edad", "Modelo Completo"), col=c("#377eb8", "#4daf4a", "#596777"), lwd=4, cex=0.8)
title(main = "Curva ROC - Data validación")

