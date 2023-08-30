# 0. Identificacion ----------------------------------
#Título: Codigo elaboración del Taller 5 del segundo curso del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del quinto taller del segundo curso del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra,
               nortest,
               car,
               lmtest)

# d cook function

dcook <- function(data, model){
  n <- nobs(model) #n de observaciones
  k <- length(coef(model)) # n de parametros
  dcook <- 4 / (n - k - 1) #punt de corte
  
  final <- broom::augment_columns(model, data = data)
  final$id <- as.numeric(row.names(final))
  # identify obs with Cook's D above cutoff
  ggplot(final, aes(id, .cooksd)) +
    geom_bar(stat = "identity", position = "identity") +
    xlab("Obs. Number") + ylab("Cook's distance") +
    geom_hline(yintercept = dcook) +
    geom_text(aes(label = ifelse((.cooksd > dcook), id, "")),
              vjust = -0.2, hjust = 0.5)
}

dffits_mar <- function(model){
  n <- nobs(model) #n de observaciones
  k <- length(coef(model)) # n de parametros
  corte <- 2*sqrt(k/n) #punt de corte
  data.frame(yhat = dffits(model)) %>% filter(yhat > corte)
}


# 2. Cargar datos --------------------

df <- read_excel("input/data/original/Abalon.xlsx")

# 3. Realización del taller --------------------

##########################################################################
########################## INTRODUCCION ##################################
##########################################################################

# El objetivo es predecir el momento exacto de "cosecha", la cual puede estar relacionada con la edad del abalón (anillos)
# que solo puede obtenerse (postmorten) a partir de otras características más fáciles de evaluar (como peso total, largo o diámetro,
# que permitirá elegir los apropiados).
# 
# Pero más que la edad, el interés real es "maximizar" la carne a obtener (dado que se envía congelado o enlatado al sudeste asiático
# - China, Taiwan, Japón. Es decir, sin concha).La base Abalon.xlsx tiene información recolectada en 4 centros a lo largo de Chile 
# (Caldera, Coquimbo, Puerto Montt y Chiloé). Son 400 datos y es relevante lograr un buen modelo a fin de optimizar el manejo, cultivo 
# y cosecha de este molusco. Las variables son: largo, diámetro, alto, peso total, peso del cuerpo, peso de la concha, número de anillos.

# (a) ¿Qué variable explica mejor linealmente el peso cuerpo (pesocu): largo, diámetro o alto? Revise los supuestos y explique el modelo escogido. --------------------

# a1. Descriptivos -------------------
par(mfrow = c(1,3))
plot(pesocu ~ largo   , data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(pesocu ~ diametro, data = df, type = "p", pch = 20, bty = "n", las = 1)
plot(pesocu ~ alto    , data = df, type = "p", pch = 20, bty = "n", las = 1)
dev.off()

# COMENTARIO: Visualmente, todos parecen ajustar bien, quizás el alto es el que ajusta peor.

# a2. Multivariados --------------------

m_largo <- lm(pesocu ~ largo, data = df)
m_diametro <- lm(pesocu ~ diametro, data = df)
m_alto <- lm(pesocu ~ alto, data = df)

summary(m_largo)$r.squared
summary(m_diametro)$r.squared
summary(m_alto)$r.squared

# REPSUESTA: El modelo que mejor ajusta según el R2 es el que incluye al largo como predictor. En detalle, el 82.9% de la variación del peso es explicada por el largo del abalón.
# Probemos ahora los supuestos.

# Supuestos:
# Linealidad # plot(modelo, 1)
# Normalidad # nortest::lillie.test(modelo$residuals) # h1: no se cumple normalidad
# Homocedeasticidad # lmtest::bptest(modelo) # h1: no se cumple homocedeasticidad
# Independencia # lmtest::dwtest(modelo)
# Multicolinealidad # car::vif(modelo) # > 5 o > 10 no recomendable

# Outliers # car::outlierTest(modelo) # h1: i-esima observacion es un outlier
# Leverege 
# Observaciones influyentes #dffit(modelo) # > 2√k/n potencial influyente #dfbetas(modelo) # > 2/√n potencial influyente

# a3. LINEALIDAD -----------------
plot(m_largo, 1)

## COMENTARIO: Si bien no es exactamente una linea recta, no parece haber una forma de la distribución que sugiera otro tipo de distribución.

# a3. NORMALIDAD ----------------------
lillie.test(m_largo$residuals)

# COMENTARIO: Existe evidencia para rechazar la hipótesis de que los datos se distribuyen normalmente, por ende, no se cumple el supuesto.

# a3. HOMOCEDEASTICIDAD --------------
lmtest::bptest(m_largo)

## COMENTARIO: Existe evidencia para rechazar la hipótesis de que los datos son homocedeasticios

# a4. INDEPENDENCIA

## COMENTARIO: Los datos no deberían seguir algún tipo de patrón, pero se hará el test de todas formas.

lmtest::dwtest(m_largo)

## COMENTARIO: No existe evidencia para rechazar la hipótesis nula de que los datos están autocorrelacionados, se cumple el supuesto.

# (b) ¿En cuántas unidades aumenta o decrece el peso cuerpo, por cada 5 unidades que aumenta el mejor regresor? --------------------

print(paste0("Por cada cinco unidades que aumenta el largo, el peso del abalón aumenta en ",m_largo$coefficients[2] %>% round(.,2) * 5,"."))

# (c) Mediante una técnica iterativa (forward o backward) seleccione el mejor modelo predictivo. Indique en cada paso que variable ingresa/sale. Interprete el modelo final y sus resultados. --------------------

step(lm(pesocu ~ ., data = df), direction = "backward")

## COMENTARIO: En base al metodo backward, se toma un paso para llegar al modelo con mejor ajuste (tomando AIC como criterio). El paso consiste en extraer el predictor "centro", dejando todos los demás.

m_back <- step(lm(pesocu ~ ., data = df), direction = "backward")
summary(m_back)

print(paste0("Por cada unidad que aumenta el largo, el peso del abalón aumenta en ",m_back$coefficients[2] %>% round(.,2),", manteniendo constante las demás variables"))
print(paste0("Por cada unidad que aumenta el diametro, el peso del abalón aumenta en ",m_back$coefficients[3] %>% round(.,2),", manteniendo constante las demás variables"))
print(paste0("Por cada unidad que aumenta el alto, el peso del abalón aumenta en ",m_back$coefficients[4] %>% round(.,2),", manteniendo constante las demás variables"))
print(paste0("Por cada unidad que aumenta el pesot, el peso del abalón aumenta en ",m_back$coefficients[5] %>% round(.,2),", manteniendo constante las demás variables"))
print(paste0("Por cada unidad que aumenta el pesoco, el peso del abalón aumenta en ",m_back$coefficients[6] %>% round(.,2),", manteniendo constante las demás variables"))
print(paste0("Por cada unidad que aumenta el anillos, el peso del abalón aumenta en ",m_back$coefficients[7] %>% round(.,2),", manteniendo constante las demás variables"))


print(paste0("El modelo que incluye todas las variables menos el centro ", summary(m_back)$adj.r.squared %>% round(.,2)*100, "% de la variación en el peso del abalón, penalizando por la cantidad de predictores incluidos en el modelo"))

# (d) Revise supuestos del modelo anterior y evalúe multicolinealidad. --------------------

# LINEALIDAD
dev.off()
plot(m_back, 1) # Se ve lineal, mejoró
# NORMALIDAD
lillie.test(m_back$residuals) # Existe evicencia para recahzar normalidad
# HOMOCEDASTICIDAD
lmtest::bptest(m_back) # Existe evidencia para rechazar homocedasticidad
# INDEPENDENCIA
lmtest::dwtest(m_back) # No existe evidencia para rechazar independencia
# MULTICOLINEALIAD
car::vif(m_back)

# (e) En caso de existir multicolinealidad proponga que variables dejar/eliminar, respalde su elección y vuelva a evaluar. --------------------

##  COMENTARIO: Las tres variables con mayores niveles de multicolinealidad son largo, diametro y pesot. Se eliminarán y se volverán a correr los supuestos.

m_back_rec <- lm(pesocu  ~ alto + pesoco + anillos, data = df)

# LINEALIDAD
dev.off()
plot(m_back_rec, 1) # Se ve lineal, mejoró
# NORMALIDAD
lillie.test(m_back_rec$residuals) # Existe evicencia para recahzar normalidad
# HOMOCEDASTICIDAD
lmtest::bptest(m_back_rec) # Existe evidencia para rechazar homocedasticidad
# INDEPENDENCIA
lmtest::dwtest(m_back_rec) # No existe evidencia para rechazar independencia
# MULTICOLINEALIAD
car::vif(m_back_rec)

## Respuesta: Mejora multicolinealidad

# (f) Para el mejor modelo obtenido en e) ¿Puede realizar una predicción para una pieza de abalón que tiene de largo 13, diámetro 10, alto 3.5, peso total 380, peso de la concha 110 y numero de anillos 9? --------------------

predict(m_back_rec, data.frame(alto = 3.5, anillos = 9, pesoco = 110))

print(paste0("Se espera que una pieza de abalón alto 3.5, peso de la concha 110 y numero de anillos 9 tenga un peso de ",predict(m_back_rec, data.frame(alto = 3.5, anillos = 9, pesoco = 110)) %>% round(.,2)))

# (g) [Tentativo] Revise la existencia de datos anómalos, de apalancamiento e influyentes. --------------------

# OUTLIERS
car::outlierTest(m_back_rec) # h1: i-esima observacion es un outlier
# LEVEREGE
dcook(df, m_back_rec)
# OBS INFLUYENTES
dffits_mar(m_back_rec)

car::influenceIndexPlot(m_back_rec)

