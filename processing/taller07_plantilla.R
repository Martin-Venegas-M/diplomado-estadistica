# Base de datos y paquetes ####

library(tidyverse) # manipulacion de datos
library(readxl) 
library(plotly) # graficos interactivos 
library(psych) # describe 
library(ggmosaic) # graficos de mosaico 
library(lmtest) # test de homocedasticidad 
library(pgirmess) # krustalmc 
# library(ggcorrplot) # grafico de correlaciones
# library(propagate) # multiples ajustes


PSU <- read_excel("Bases de datos/Base PSU (muestra).xlsx")
glimpse(PSU)

PSU <- PSU %>% mutate(Lenguaje = as.numeric(Lenguaje),
                      Matematicas = as.numeric(Matematicas),
                      Historia = as.numeric(Historia),
                      Ciencias = as.numeric(Ciencias),
                      Ranking = as.numeric(Ranking),
                      NEM_ceros = as.numeric(NEM_ceros))
glimpse(PSU)

# Pregunta 1 ####

# Distribucion por sexo
tabla_1 <- table(PSU$sexo)

tabla_1

prop.table(tabla_1)

tabla_plot <- data.frame(tabla_1)
names(tabla_plot) <- c("Sexo","Total")
plot_ly(data = tabla_plot, labels = ~Sexo, values = ~Total, type = "pie") %>% 
  layout(title = "Distribucion del sexo")

# Distribucion por regimen 
tabla_2 <- table(PSU$regimen)

tabla_2 

prop.table(tabla_2)

tabla_plot <- data.frame(tabla_2)
names(tabla_plot) <- c("regimen","Total")
plot_ly(data = tabla_plot, labels = ~regimen, values = ~Total, type = "pie") %>% 
  layout(title = "Distribucion del regimen")

# Distribucion por financiamiento
tabla_3 <- table(PSU$financiam)

tabla_3 

prop.table(tabla_3)

tabla_plot <- data.frame(tabla_3)
names(tabla_plot) <- c("financiam","Total")
plot_ly(data = tabla_plot, labels = ~financiam, values = ~Total, type = "pie") %>% 
  layout(title = "Distribucion del financiamiento")

# Estadisticos de las pruebas

PSU %>% dplyr::select(Lenguaje, Matematicas, Ciencias, Historia) %>% 
  psych::describe()

plot_ly(data = PSU, alpha = 0.6) %>% 
  add_histogram(x = ~Lenguaje) %>% 
  layout(title = "Prueba de Lenguaje")

plot_ly(data = PSU, alpha = 0.6) %>% 
  add_histogram(x = ~Matematicas) %>% 
  layout(title = "Prueba de Matematicas")

plot_ly(data = PSU, alpha = 0.6) %>% 
  add_histogram(x = ~Ciencias) %>% 
  layout(title = "Prueba de Ciencias")

plot_ly(data = PSU, alpha = 0.6, 
        marker = list(color = ~"coral")) %>% 
  add_histogram(x = ~Historia) %>% 
  layout(title = "Prueba de Historia")


colSums(is.na(PSU))


# Pregunta 2 ####

PSU <- PSU %>% mutate(cod_ranking = case_when(Ranking <= 450 ~ "Bajo",
                                              Ranking <= 650 ~ "Medio",
                                              Ranking > 650 ~ "Alto"))


table(PSU$sexo, PSU$cod_ranking)

# Pregunta 3 ####

hombres <- PSU %>% filter(sexo == "M")
ranking_hombres <- hombres$Ranking

mujeres <- PSU %>% filter(sexo == "F")
ranking_mujeres <- mujeres$Ranking

plot_ly(y = ~ranking_hombres, type = "box", name = "hombre", boxmean = T) %>% 
  add_trace(y = ~ranking_mujeres, type = "box", name = "mujer", boxmean = T)


# H0: mu_1 = mu_2
# H1: mu_1 != mu_2


# primero, probamos varianzas distintas

# H0: sigma_1 = sigma_2
# H1: sigma_1 != sigma_2

var.test(ranking_mujeres, ranking_hombres, alternative = "two.sided", 
         conf.level = 0.95)

# NO RECHAZO H0

###############################
# Rechazo H0: VALOR.P < ALPHA # 
###############################

t.test(ranking_mujeres, ranking_hombres, alternative = "two.sided", 
       conf.level = 0.95, var.equal = TRUE)

# Rechazo H0
# existe evidencia de que las medias no son iguales


# Pregunta 5 

hombres_mat <-  na.omit(hombres$Matematicas)
mayores_mat_hombres <- ifelse(hombres_mat > 600, 1, 0)
exitos_hombres <- sum(mayores_mat_hombres)
n_hombres <- length(mayores_mat_hombres)

mujeres_mat <- na.omit(mujeres$Matematicas)
mayores_mat_mujeres <- ifelse(mujeres_mat > 600, 1, 0)
exitos_mujeres_mat <- sum(mayores_mat_mujeres)
n_mujeres <- length(mayores_mat_mujeres)

# H0: p_hombres <= p_mujeres
# H1: p_hombres > p_mujeres

prop.test(x = c(exitos_hombres, exitos_mujeres_mat),
          n = c(n_hombres, n_mujeres),
          alternative = "greater", 
          conf.level = 0.95)

# No rechazo H0.


# Pregunta 7

PSU$Tipo_colegio %>% table()

ggplot(data = PSU, aes(x = Tipo_colegio, fill = Tipo_colegio, y = Ranking)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2)  


# H0: mu1 = mu2 = mu3
# H1: alguna es distinta

# test
anova <- aov(PSU$Ranking ~ PSU$Tipo_colegio)
summary(anova)
plot(anova)
# Rechazo H0: -> alguna media difiere (<2e-16 ~~ 0)

# H0: Normalidad 
# H1: No Normalidad

ks.test(anova$residuals, "pnorm", mean(anova$residuals), sd(anova$residuals))

# Rechazo H0, no son normales

# H0: homoceasticidad
# H1: homoceasticidad

library(lmtest)
bptest(anova)

# No rechazo H0, cumplo supuesto de homoceasticidad

# Dado que no se cumple supuesto de normalidad
kruskal.test(PSU$Ranking ~ PSU$Tipo_colegio)
# Rechazo H0: -> alguna media difiere (<2e-16 ~~ 0)
kruskalmc(PSU$Ranking ~ PSU$Tipo_colegio)


# Pregunta 8

PSU <- PSU %>% 
  mutate(promedio_mat_leng = (Matematicas + Lenguaje)/2)
glimpse(PSU)

ggplot(data = PSU, aes(x = financiam, fill = financiam, y = promedio_mat_leng)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2)  


# H0: mu1 = mu2 = mu3
# H1: alguna es distinta

# test
anova <- aov(PSU$promedio_mat_leng ~ PSU$financiam)
summary(anova)
# plot(anova)
# Rechazo H0: -> alguna media difiere (<2e-16 ~~ 0)

# H0: Normalidad 
# H1: No Normalidad

ks.test(anova$residuals, "pnorm", mean(anova$residuals), sd(anova$residuals))

# No Rechazo H0, son normales

# H0: homoceasticidad
# H1: homoceasticidad

library(lmtest)
bptest(anova)
# Rechazo No, no hay homoceasticidad


kruskal.test(PSU$promedio_mat_leng ~ PSU$financiam)
# Rechazo H0: -> alguna media difiere (<2e-16 ~~ 0)

# Probando Tukey
TukeyHSD(anova)
plot(TukeyHSD(anova), las = 1)







