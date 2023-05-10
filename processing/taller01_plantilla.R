## TALLER 1

# Librerias
library(readxl)
library(dplyr)
# library(psych)
# library(vioplot)

cv = function(x){
  aux = sd(x)/mean(x)
    return(aux)
}

# Lectura de los datos 

base <- read_excel("Bases de datos/orders_data.xlsx")
head(base)
glimpse(base)

# Pregunta 1 ####

summary(base$itemtotal) # Realiza un resumen de la columna itemtotal
hist(base$itemtotal) # Histograma de itemtotal (distribucion de los datos)
boxplot(base$itemtotal) # Grafico de caja # Muchos valores atipicos 
moments::skewness(base$itemtotal)

# Pregunta 2 ####

base$order_recodificado <- ifelse(base$order_status == 1, "Delivered", "Returned")

aggregate(itemtotal ~ order_recodificado, data = base, FUN = mean)
aggregate(itemtotal ~ order_recodificado, data = base, FUN = sd)
aggregate(itemtotal ~ order_recodificado, data = base, FUN = cv)

# Pregunta 3 ####

# Boxplot
boxplot(base$itemtotal ~ base$month_date)
# Violin
# install.packages("vioplot")
library(vioplot)
vioplot(base$itemtotal ~ base$month_date)

# Elegimos dos, tres,... meses: por ejemplo, JUL, OCT
# filtro
base_jun_jul <- filter(base, month_date == "JUN" | month_date == "JUL")

# grafico
boxplot(base_jun_jul$itemtotal ~ base_jun_jul$month_date)

# descriptivo
base_jun_jul %>%
  group_by(month_date) %>%
  summarise(Promedio = mean(itemtotal),
            Desviacion = sd(itemtotal),
            CV = sd(itemtotal)/mean(itemtotal)*100,
            min = min(itemtotal),
            max = max(itemtotal)) # En julio se alcanzan pedidos con valor mas alto

# Pregunta 4 ####

top_5 = base %>% group_by(ship_city) %>%
  summarise(cantidad_de_pedidos = n()) %>% # n() nos sirve para contar el numero de casos que tenemos
  arrange(desc(cantidad_de_pedidos)) %>% # arrange nos sirve para ordenar y desc() para ir descendiendo
  slice(1:5) # slice nos permite seleccionar, remover y duplicar observaciones

pie(top_5$cantidad_de_pedidos, label = top_5$ship_city) # como se distribuye entre ellas

# Como nos hubiesemos quedado con solo esas 5 ciudades en nuestra data?

base %>% filter(ship_city %in% top_5$ship_city)
# Esto es optimo solo para pequeños cruces, para intersecciones mas grandes
# es recomendable realizar un left_join y filtrar segun interes, por ver!!

# Pregunta 5 ####

tabla2 <- table(base$month_date)
tabla3 <- prop.table(tabla2)
barplot(tabla3, ylim = c(0,1))

# respondiendo a la pregunta
junio <- filter(base, month_date == "JUN")
tabla2 <- table(junio$month_date, junio$order_recodificado)
tabla3 <- prop.table(tabla2)
barplot(tabla3, col = "steelblue", width = 0.5, ylim = c(0,1))

# Pregunta 6 ####

base <- base %>% mutate(tramo_envio = case_when(shipping_fee>20 ~ "Mayores_a_20",
                                                shipping_fee>=10 ~ "Entre_10_y_20",
                                                shipping_fee<10 ~ "Menores_a_10",
                                                is.na(shipping_fee) ~ "gratuito"))

# Pregunta 7 ####

# Con dplyr
base %>%
  group_by(tramo_envio) %>%
  summarise(Promedio = mean(itemtotal),
            Desviacion = sd(itemtotal),
            CD = cv(itemtotal)*100) 

# extra
# Que pasa con los gratuitos?
gratuitos = filter(base, tramo_envio == "gratuito")

quantile(gratuitos$itemtotal)
quantile(gratuitos$itemtotal, prob = seq(0.1, 1, 0.1))
# Describe ####
# install.packages("psych")
library(psych)
describe(gratuitos$itemtotal)


# Pregunta 8 ####

tabla4 <- table(base$tramo_envio, base$ship_city)
tabla5 <- prop.table(tabla4, margin = 2)
barplot(tabla5, legend.text = TRUE) 

# respondiendo a la pregunta
# Busquemos el Top 3 ciudades con mayor reacudacion

top_3 = base %>% group_by(ship_city) %>%
  summarise(recaudacion = sum(itemtotal)) %>% # n() nos sirve para contar el numero de casos que tenemos
  arrange(desc(recaudacion)) %>% # arrange nos sirve para ordenar y desc() para ir descendiendo
  slice(1:3) # slice nos permite seleccionar, remover y duplicar observaciones
top_3

base_top3 <- base %>% filter(ship_city %in% top_3$ship_city)
base_top3

table(base_top3$ship_city) # garantizado que solo tenemos 3 ciudades

# Para finalizar
tabla6 <- table(base_top3$tramo_envio, base_top3$ship_city)
tabla7 <- prop.table(tabla6, margin = 2); tabla7*100
barplot(tabla7, legend.text = TRUE) 

# Pregunta 9 ####
base$cod
base$cod <- tidyr::replace_na(base$cod, "Prepaid")

# Prepagados con anterioridad y su costo de envio sea <10 o >20
# Dos opciones:

# filtro1: Menores a 10 o Mayores a 20, filtro2: Prepagados
# filtro2: Todo junto 

#1
base2 <- base %>% filter(tramo_envio == "Menores_a_10" | 
                           tramo_envio == "Mayores_a_20")
table(base2$tramo_envio)

base3 <- base2 %>% filter(cod == "Prepaid")
base3

#2
base3 <- base %>% filter(cod == "Prepaid" & (tramo_envio == "Menores_a_10" | 
                                               tramo_envio == "Mayores_a_20"))
# Como ya sabemos que son prepagados veamos como se distribuyen
# segun el tramo de envio

tabla8 <- table(base3$tramo_envio)
pie(tabla8, main= "Pedidos Prepagados")

# Pregunta 10 ####

base4 <- base %>% filter(cod == "Prepaid")

tabla9 <- table(base4$tramo_envio)
prop.table(tabla9)*100
