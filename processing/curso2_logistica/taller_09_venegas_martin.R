# 0. Identificacion ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Codigo elaboración del Taller 9 del curso 2 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del noveno taller del segundo curso diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra,
               summarytools)

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
OR_edad_10 <- exp(m1$coefficients[2]*10) %>% round(.,2) # OR Edad

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
# 
# set.seed(20230822)
# filas_validacion <- sample(1:nrow(fuga), 0.2*nrow(fuga), replace = FALSE)
# ## dejaremos un 20% de la data para validacion
# 
# fuga$FILTRO <- "ENTRENAMIENTO"
# fuga$FILTRO[filas_validacion] <- "VALIDACION"
# 
# glimpse(fuga)
# table(fuga$FILTRO)
# 
# data_entrenamiento <- fuga %>% filter(FILTRO == "ENTRENAMIENTO")
# data_entrenamiento <- data_entrenamiento %>% select(-FILTRO) ## Quitamos la variable FILTRO ##
# nrow(data_entrenamiento)
# 
# data_validacion <- fuga %>% filter(FILTRO == "VALIDACION")
# data_validacion <- data_validacion %>% select(-FILTRO) ## Quitamos la variable FILTRO ##
# nrow(data_validacion)
# 
# glimpse(data_entrenamiento)
# glimpse(data_validacion)

m_comp <- glm(Target ~ ., data = df, family = "binomial")

summary(m_comp)

m_back <- step(m_comp, method = "backward") # Un paso, se elimina TC

#################################### VALIDACION DE UN MODELO #################################################################################################

# h) Utilizando la base de validación y el modelo obtenido en base al método backward, calcule las probabilidades de fugarse del banco. ----

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

# ¿Si el punto de corte fuese una probabilidad del 50%, usted le aplicaría la campaña anti-fuga a este cliente?

# j) Evalúe el modelo, para ello obtenga e interprete los siguientes estadísticos: Curva ROC y KS. ----

# k) El banco desea generar una campaña de retención de clientes. Para ello, usted debe identificar un punto de corte el cual optimice la sensibilidad del modelo, pero que cometa como máximo una tasa de falsos positivos (1 - Especificidad) de a lo más un 25%. ¿Qué punto de corte propone? Reporte la sensibilidad y especificidad. ----

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