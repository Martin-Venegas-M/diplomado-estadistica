# 0. Identificacion ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Codigo elaboración del Taller 9 del curso 2 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del noveno taller del segundo curso diplomado.

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
               caret
               )

# 2. Cargar datos -------------------------------------------------------------------------------------------------------------------------------------------

df <- read_excel("input/data/original/FUGA.xlsx")

# 3. Realización del taller ---------------------------------------------------------------------------------------------------------------------------------

#################################### INTRODUCCION ###########################################################################################################

# Cuando un cliente abandona la relación con cierta empresa o entidad, se le denomina fuga. En este taller,
# se desea construir un modelo de regresión logística para predecir la fuga de clientes de una entidad bancaria. 
# Los datos se encuentran en la base de datos FUGA.xlsx y la variable target se define como 1 si el cliente se fugó
# en el período de estudio y 0 si no. En el caso de que el cliente se haya fugado, se dispone de información de los 
# últimos 12 meses antes de que se fugara, en el caso de que no se haya fugado, se dispone de información de los últimos 
# 12 meses hasta el momento de la extracción de los datos. Las variables son las siguientes:

# • Target: Indica si un cliente se ha fugado (1: fugado, 0: no fugado).
# • Sexo: Indica el sexo del cliente (M: mujer, H: hombre).
# • Edad: Edad del cliente (en años).
# • Antigüedad: Meses de antigüedad del cliente en la institución.
# • Internauta: Indica si un cliente utiliza los servicios web (1: sí, 0: no).
# • TC: Número de tarjetas de crédito del cliente.
# • Cupo_TC: Cupo promedio en tarjetas de crédito del cliente.
# • N_meses_activo: Número de meses en los que el cliente realizó alguna transacción (últimos 12 meses).
# • N_meses_avances: Número de meses en los que el cliente realizó un avance (últimos 12 meses).
# • N_transacciones: Número de transacciones que realizó (últimos 12 meses).
# • Prom_facturado: Promedio monto facturado (últimos 12 meses)
# • Prom_pagos: Promedio monto pagado mensualmente (últimos 12 meses).
# • Prom_uso_TC: Promedio de uso de la Tarjeta de crédito (últimos 12 meses).

#################################### LIMPIEZA ################################################################################################################

# a) Cargue los datos en R y revise los formatos de cada variable, recuerde codificar las variables como numéricas o factores según corresponda ----

str(df) # Todo está en númeric

df <- df %>% dplyr::mutate_at(c("Target", "Internauta"), factor)

str(df)

# b) Determine si existen o no observaciones faltantes, en el caso de existir tome la decisión de omitirlas del estudio u omitir la variable. ----

any(is.na(df)) # No hay NA's

# c) Estudie la existencia de incongruencias en la fuente de datos, por ejemplo, clientes con edades negativas. ----

view(dfSummary(df)) # Todo bien

#################################### ANALISIS PRELIMINAR #####################################################################################################

# d) Realice análisis de cómo se relacionan las siguientes variables sexo y edad con la variable target. ----

m1 <- glm(Target ~ Edad + Sexo, data = df, family = "binomial")

summary(m1)

#################################### INTERPRETACIÓN DEL MODELO ###############################################################################################

# e) Calcule e interprete los OR correspondientes al modelo, ¿son estos factores protectores o agravantes de la fuga del cliente? ----

OR_edad <- m1$coefficients[2] %>% exp() %>% round(.,2) # OR Edad
OR_edad_10 <- exp(m1$coefficients[2]*10) %>% round(.,2) # OR Edad multiplicado por 10 (la interpretación pasa de cada año a cada 10 años)

OR_mujer <- m1$coefficients[3] %>% exp() %>% round(.,2) # OR Sexo Mujer vs Hombre

# INTERPRETACION EDAD
print(glue::glue("Por cada año de edad, las chances de fugarse aumentan en {(OR_edad-1)*100}%. Por tanto, el aumento de la edad es un factor de riesgo para la fuga de clientes."))

# INTERPRETACION SEXO
print(glue::glue("Las chances de que una mujer se fuge son {OR_mujer} menores a las chances de que se fuge un hombre. Por tanto, ser mujer es un factor protector ante la fuga de clientes."))
print(glue::glue("Las mujeres tienen un {abs((OR_mujer-1)*100)}% menos chances de fugarse que los hombres. Por tanto, ser mujer es un factor protector ante la fuga de clientes."))

# f) En base al modelo obtenido, ¿en cuánto cambia el riesgo de fugarse un cliente de 34 años en relación con uno de 24 años? ----

# INTERPRETACION EDAD
print(glue::glue("Por cada diez años de edad, las chances de fugarse aumentan en {(OR_edad_10-1)*100}%. Por tanto, el aumento de la edad es un factor de riesgo para la fuga de clientes. Esto se aplica para todos los rangos de edad, como por ejemplo entre un cliente con 24 años y uno con 34."))

#################################### VARIANTES DE UN MODELO ##################################################################################################

# g) Utilizando un método automatizado, encuentre el modelo óptimo usando como criterio el criterio de información de Akaike (AIC). La función step( ) puede ser de utilidad. ----

# DIVIDIR BASE DE DATOS
set.seed(20230822)
filas_validacion <- sample(1:nrow(df), # del total de filas (35.508)
                           0.2 * nrow(df), # saca una muestra aleatoria del 20% (7102)
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

# Estimación del modelo completo
m_comp <- glm(Target ~ .,
              data = dfe %>% select(-FILTRO), # ¡OJO! Se elimina FILTRO de la base de datos ya que sino no se puede hacer el modelo utilizando el atajo del punto (.)
              family = "binomial") # ¡OJO! SE UTILIZA SOLO LA DATA DE ENTRENAMIENTO

# Ver modelo
summary(m_comp)

# Estimación modelo backaward
m_back <- step(m_comp, method = "backward") # Un paso, se elimina TC

# Ver modelo
summary(m_back)

#################################### VALIDACION DE UN MODELO #################################################################################################

# h) Utilizando la base de validación y el modelo obtenido en base al método backward, calcule las probabilidades de fugarse del banco. ----

# DATA ENTRENAMIENTO
dfe <- dfe %>% mutate(Target_p = m_back$fitted.values) # guardar valores predichos

# DATA VALIDACION
dfv <- dfv %>% mutate(Target_p = predict(m_back, # Utiliza el modelo backaward para hacer las predicciones
                                         dfv, # Y crea valores predichos para el dataset de validación
                                         type = "response")) # Y que las predicciones vengan directamente en probabilidades (no en logodds)

dfp <- rbind(dfe,dfv) # Unir bases de entrenamiento y validacion

par(bty = "n", pch = 20, las = 1)
boxplot(Target_p ~ FILTRO, data = dfp, 
        outline = FALSE, ylab = "probabilidad")

# No parecen haber diferencias en la distribución de las probabilidades predichas entre ambos datasets

# Emplear test de hipótesis (H0: las probabilidades distribuyen igual)
ks.test(dfe$Target_p,
        dfv$Target_p)

# INTERPRETACION: No hay suficiente evidencia para rechazar la hipótesis nula de que la distribución de las probabilidades de ambos dataset son iguales

# i) Se quiere aplicar una campaña a clientes probables de fugarse (ofrecer productos personalizados, mejores tasas de interés, aumento de cupo, etcétera). Si este cliente tiene las siguientes características en los últimos 12 meses: ----
# • Sexo: Masculino
# • Edad: 49 Años
# • Antigüedad: 33 meses
# • Internauta: No
# • Tarjetas de crédito: 2
# • Cupo_TC: 2.000.000
# • N_meses_activo: 7
# • N_meses_avances: 0
# • N_transacciones: 7
# • Prom_facturado: 27.510
# • Prom_pagos: 47.000
# • Prom_uso_TC: 50.500

# Crear dataframe con la persona señalada
df_test_i <- data.frame(
  Sexo = "H",
  Edad = 49,
  Antiguedad = 33,
  Internauta = 0,
  TC = 2,
  Cupo_TC = 2000000,
  N_meses_activo = 7,
  N_meses_avances = 0,
  N_transacciones = 7,
  Prom_facturado = 27510,
  Prom_pagos = 47000,
  Prom_uso_TC = 50500
) %>% mutate(Internauta = as.factor(Internauta)) # Convertir Internauta a factor
             
# Elaborar predicción
df_test_i <- df_test_i %>% mutate(
  Target_p = predict(m_back, df_test_i, type = "response")
)

print(glue::glue("Una persona con las caracteristicas señaladas tiene una probabilidad de fugarse del {round(df_test_i$Target_p, 2)*100}%"))

# ¿Si el punto de corte fuese una probabilidad del 50%, usted le aplicaría la campaña anti-fuga a este cliente?

# RESPUESTA: No, ya que no se fugaría bajo ese punto de corte.

# j) Evalúe el modelo, para ello obtenga e interprete los siguientes estadísticos: Curva ROC y KS. ----

# AUROC
# Recordemos...
# - Si AUC = 0.5, entonces, no hay discriminación.
# - Si 0.7 =< AUC < 0.8, la discriminación se considera aceptable
# - Si 0.8 =< AUC < 0.9, la discriminación se considera excelente
# - Si AUC => 0.9, la discriminación se considera sobresaliente

InformationValue::AUROC(actuals = dfe$Target,
                        predictedScores = dfe$Target_p)

InformationValue::AUROC(actuals = dfv$Target,
                        predictedScores = dfv$Target_p)

# INTERPRETACIÓN: En base a los criterior anteriormente señalados, se puede afirmar que el modelo es aceptable.

# KS Stat
InformationValue::ks_stat(actuals = dfe$Target,
                          predictedScores = dfe$Target_p)

InformationValue::ks_stat(actuals = dfv$Target,
                          predictedScores = dfv$Target_p)
# entre mas cercano a 1 mejor

# k) El banco desea generar una campaña de retención de clientes. Para ello, usted debe identificar un punto de corte el cual optimice la sensibilidad del modelo, pero que cometa como máximo una tasa de falsos positivos (1 - Especificidad) de a lo más un 25%. ¿Qué punto de corte propone? Reporte la sensibilidad y especificidad. ----

# PUNTO DE CORTE
cutoff_opt <- InformationValue::optimalCutoff(actuals = dfe$Target,
                                               predictedScores = dfe$Target_p,
                                               optimiseFor = "Both")

# El punto de corte óptimo es 0.09479657

##################################### PUNTO DE CORTE ÓPTIMO ##########################################################################################################

# MATRIZ CONDUSION

InformationValue::confusionMatrix(actuals = dfe$Target,
                                  predictedScores = dfe$Target_p,
                                  threshold = cutoff_opt)

# INTERPRETACIÓN: Tenemos...
# 1.697 casos verdaderos correctamente clasificados como positivos (VP). Es decir, 1.697 clientes que se fugaron el modelo predijo que se fugarían.
# 774 casos verdaderos incorrectamente clasificados como negativos (VN). Es decir, 774 clientes que se fugaron, el modelo predijo que no se fugarían.

# 17.931 casos falsos correctamente clasificados como negativos (FN). Es decir, 17.931 clientes que no se fugaron, el modelo predijo que no se fugarían
# 8.005 casos falsos incorrectamente clasificados como positivos (FP). Es decir, 8.005 clientes que no se fugaron, el modelo predijo que se fugarían.

# SENSIBILIDAD: ¿qué tan bien lo hago con lo que se fugan?
InformationValue::sensitivity(actuals = dfe$Target,
                              predictedScores = dfe$Target_p,
                              threshold = cutoff_opt)

# INTERPRETACIÓN: El 68.6% de los clientes que se fugaron fueron correctamente clasificados por el modelo.

# ESPECIFICIDAD: ¿qué tan bien lo hago con lo que no se fugan?
InformationValue::specificity(actuals = dfe$Target,
                              predictedScores = dfe$Target_p,
                              threshold = cutoff_opt)

# INTERPRETACIÓN: El 69.1% de los clientes que no se fugaron fue correctamente clasificados por el modelo.

# PLOT ROC CURVE
InformationValue::plotROC(actuals = dfe$Target,
                                 predictedScores = dfe$Target_p)


# TASA DE FALSOS POSITIVOS
# Proporción de falsos incorrectamente clasificados como positivos.

1-InformationValue::specificity(actuals = dfe$Target,
                                predictedScores = dfe$Target_p,
                                threshold = cutoff_opt)

# INTERPRETACIÓN: El 30.8% de los clientes que no se fugaron fueron incorrectamente clasificados por el modelo (como falsos positivos).
# ¡¡¡¡¡¡¡¡¡OJOOOOOO!!!!!!!!!! Para este taller en particular no se utiliza este punto de corte ya que sobrepasa el error que se solicita para la campaña (25%).

####################### PUNTO DE CORTE MANUAL ######################################################################################################################
# DESCRIPCIÓN: Mejor especificidad (detección correcta de la no fuga), peor sensibilidad (detección correcta de la fuga)
# ¡OJO! Esto se hizo visualizando la ROC CURVE. Por ende, si por el fenómeno de estudio nos interesa mejor una de las dos métricas, puede que el optimal cutoff no sea la mejor opción.

# MATRIZ CONFUSION

InformationValue::confusionMatrix(actuals = dfe$Target,
                                  predictedScores = dfe$Target_p,
                                  threshold = 0.108)


# SENSIBILIDAD 
InformationValue::sensitivity(actuals = dfe$Target,
                              predictedScores = dfe$Target_p,
                              threshold = 0.108)

# INTERPRETACIÓN: El 61.7% de los clientes que se fugaron fueron correctamente clasificados por el modelo.

# ESPECIFICDAD
InformationValue::specificity(actuals = dfe$Target,
                              predictedScores = dfe$Target_p,
                              threshold = 0.108) 

# INTERPRETACIÓN: El 75.3% de los clientes que no se fugaron fue correctamente clasificados por el modelo.

# TASA DE FALSOS POSITIVOS
1-InformationValue::specificity(actuals = dfe$Target,
                                predictedScores = dfe$Target_p,
                                threshold = 0.108) 

# INTERPRETACIÓN: El 24.6% de los clientes que no se fugaron fueron incorrectamente clasificados por el modelo (como falsos positivos). 
# ¡OJO!: Este es el punto de corte que optimiza la sensibiidad y al mismo tiempo cumple el criterio pedido para la campaña de que la TFP sea menor a 25%.

##################### REPLIQUEMOS CON OTRO PAQUETE PARA CORROBORAR ###################################################################################

# ¡OJO! la función confusionMatrix() pide la matriz esté de la siguiente manera: 

# FORMA A:

#           Reference	
# Predicted	Event	No Event
#     Event	  A	     B
#  No Event	  C	     D

# Esto es al revés de como lo estábamos usando antes. Anteriormente estábamos haciendo esto:

# FORMA B:

#           Reference	
# Predicted	No event Event
#  No Event	  D	     C
#     Event	  B	     A

# De hecho, si creamos la matriz de confusión de forma manual, vemos que queda como la FORMA B. 

dfe$Target_clasificado <- ifelse(dfe$Target_p >= 0.108, 1, 0) # Crear variable de clasificación en base al punto de corte elegido
table(dfe$Target_clasificado, dfe$Target) # Ver matriz de confusión creada de forma manual

# Esto es porque R ordena primero los 0 y luego los 1. Sin embargo, conceptualmente se suele ordenar la matriz como la FORMA A, por eso es 
# que asi lo implementa el paquete.

# Para usar el paquete caret sin cometer errores, tenemos dos opciones:

# PRIMERA OPCIÓN: Reordenar los factores para que esté primero el 1 y luego el 0. Esto forzará a que la matriz se vea como la FORMA A.

table(
  dfe %>% mutate(Target_clasificado = factor(Target_clasificado, levels = c("1", "0"))) %>% pull(Target_clasificado), 
  dfe %>% mutate(Target = factor(Target, levels = c("1", "0"))) %>% pull(Target)
  ) %>% caret::confusionMatrix(.) # Hacer estimación de la matriz y los indicadores asociados

# SEGUNDA OPCIÓN: Mantener la matriz como la FORMA B, pero incluir el argumento (positive = "1") en la funcíon confusionMatrix(). 
# Esto especificará que los éxitos son los 1, independiente del orden de la matriz.

table(
  dfe %>% pull(Target_clasificado), 
  dfe %>% pull(Target)
) %>% caret::confusionMatrix(., positive = "1") # Hacer estimación de la matriz y los indicadores asociados

# CONCLUSIÓN: Notamos que con ambas soluciones obtenemos los mismos indicadores que obteníamos con el paquete InformationValue.
# !!!!!OJO!!!!!!: Si se usa este paquete, es importante considerar este detalle, ya que si no se considera, se podrían invertir las metricas de sensibilidad y especificidad.

# l) Se quiere aplicar una campaña a clientes probables de fugarse (ofrecer productos personalizados, mejores tasas de interés, aumento de cupo, etcétera). Si este cliente tiene las siguientes características en los últimos 12 meses: ----
# • Sexo: Masculino
# • Edad: 49 Años
# • Antigüedad: 33 meses
# • Internauta: No
# • Tarjetas de crédito: 2
# • Cupo_TC: 2.000.000
# • N_meses_activo: 7
# • N_meses_avances: 0
# • N_transacciones: 7
# • Prom_facturado: 27.510
# • Prom_pagos: 47.000
# • Prom_uso_TC: 50.500
# ¿Usted le aplicaría la campaña anti-fuga a este cliente?

# Crear dataframe con la persona señalada
df_test_l <- data.frame(
  Sexo = "H",
  Edad = 49,
  Antiguedad = 33,
  Internauta = 0,
  TC = 2,
  Cupo_TC = 2000000,
  N_meses_activo = 7,
  N_meses_avances = 0,
  N_transacciones = 7,
  Prom_facturado = 27510,
  Prom_pagos = 47000,
  Prom_uso_TC = 50500
) %>% mutate(Internauta = as.factor(Internauta)) # Convertir Internauta a factor

# Elaborar predicción
df_test_l <- df_test_l %>% mutate(
  Target_p = predict(m_back, df_test_l, type = "response")
)

print(glue::glue("Una persona con las caracteristicas señaladas tiene una probabilidad de fugarse del {round(df_test_l$Target_p, 2)*100}%"))

# RESPUESTA: Considerando que el punto de corte escogido posterior a los analisis es de 10.8%, si se aplicaría la campaña antiga a este cliente, ya que sería clasificado como una potencial fuga.