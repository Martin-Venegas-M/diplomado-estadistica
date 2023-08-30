

# (h) predecir y obtener la probabilidad
aux <- predict(ModAuto, fuga)
p <- exp(aux)/(1+exp(aux))

fuga$p <- p
glimpse(fuga)
summary(fuga$p)

par(bty = "n", pch = 20, las = 1)
boxplot(p ~ FILTRO, data = fuga, 
        outline = FALSE, ylab = "probabilidad")


data_entrenamiento <- fuga %>% filter(FILTRO == "ENTRENAMIENTO")
data_entrenamiento <- data_entrenamiento %>% select(-FILTRO) ## Quitamos la variable FILTRO ##
nrow(data_entrenamiento)

data_validacion <- fuga %>% filter(FILTRO == "VALIDACION")
data_validacion <- data_validacion %>% select(-FILTRO) ## Quitamos la variable FILTRO ##

# H0: las probabilidades distribuyen igual
ks.test(data_entrenamiento$p,
        data_validacion$p)

# (j) Evaluemos
# devtools::install_github("selva86/InformationValue")
library(InformationValue)

# Curva ROC
InformationValue::AUROC(actuals = data_entrenamiento$Target,
                        predictedScores = data_entrenamiento$p)

InformationValue::AUROC(actuals = data_validacion$Target,
                        predictedScores = data_validacion$p)

# KS Stat
InformationValue::ks_stat(actuals = data_entrenamiento$Target,
                          predictedScores = data_entrenamiento$p)

InformationValue::ks_stat(actuals = data_validacion$Target,
                          predictedScores = data_validacion$p)
# entre mas cercano a 1 mejor

# (k) Punto de corte

punto_corte <- InformationValue::optimalCutoff(actuals = data_entrenamiento$Target,
                                               predictedScores = data_entrenamiento$p,
                                               optimiseFor = "Both")
punto_corte

# matriz de confusion

InformationValue::confusionMatrix(actuals = data_entrenamiento$Target,
                                  predictedScores = data_entrenamiento$p,
                                  threshold = punto_corte)

# especificidad: que tan bien lo hago con lo que no se fugan?
InformationValue::specificity(actuals = data_entrenamiento$Target,
                              predictedScores = data_entrenamiento$p,
                              threshold = punto_corte)

# sensibilidad: que tan bien lo hago con lo que se fugan?
InformationValue::sensitivity(actuals = data_entrenamiento$Target,
                              predictedScores = data_entrenamiento$p,
                              threshold = punto_corte)

aux <- InformationValue::plotROC(actuals = data_entrenamiento$Target,
                                 predictedScores = data_entrenamiento$p)

data_entrenamiento$Target_modelo <- ifelse(data_entrenamiento$p >= punto_corte, 1, 0)
table(data_entrenamiento$Target_modelo, data_entrenamiento$Target)
# izquierda: prediccion, arriba: real

# respondiendo a la pregunta ...
# tasa de falsos positivos
1-InformationValue::specificity(actuals = data_entrenamiento$Target,
                                predictedScores = data_entrenamiento$p,
                                threshold = punto_corte)
# 30.86%

# tasa de falsos positivos
1-InformationValue::specificity(actuals = data_entrenamiento$Target,
                                predictedScores = data_entrenamiento$p,
                                threshold = 0.108) 

# especificidad
InformationValue::specificity(actuals = data_entrenamiento$Target,
                              predictedScores = data_entrenamiento$p,
                              threshold = 0.108) 

# sensibilidad
InformationValue::sensitivity(actuals = data_entrenamiento$Target,
                              predictedScores = data_entrenamiento$p,
                              threshold = 0.108)

data_entrenamiento$Target_modelo <- ifelse(data_entrenamiento$p >= 0.108, 1, 0)
table(data_entrenamiento$Target_modelo, data_entrenamiento$Target)

# install.packages("caret")
library(caret)

# confusionMatrix pide 
#           Reference	
# Predicted	Event	No Event
#     Event	  A	     B
#  No Event	  C	     D

# preferible no hacer para este ejemplo, o cambiar los ordenes con as.factor
data_entrenamiento <- data_entrenamiento %>% 
  mutate(Target_modelo = factor(Target_modelo, levels = c("1","0")),
         Target = factor(Target, levels = c("1","0")),)

table(data_entrenamiento$Target_modelo, data_entrenamiento$Target)
caret::confusionMatrix(table(data_entrenamiento$Target_modelo, data_entrenamiento$Target))


# (l) 
base_k <- fuga[1, ]
base_k$Sexo <- "H"
base_k$Edad <- 49
base_k$Antiguedad <- 33
base_k$Internauta <- "No"
base_k$Cupo_TC <- 2000000
base_k$N_meses_activo <- 7
base_k$N_meses_avances <- 0
base_k$N_transacciones <- 7
base_k$Prom_facturado <- 27510
base_k$Prom_pagos <- 47000
base_k$Prom_uso_TC <- 50500

aux = predict(ModAuto, base_k)
exp(aux)/(1+exp(aux)) # p
## Es decir, este cliente no se queda (se fuga) ##
## ya que 0.1355273 es mayor a 0.108


