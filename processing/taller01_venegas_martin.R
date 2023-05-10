# 0. Identificacion ----------------------------------
#Título: Codigo elaboración del Taller 1 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del primer taller del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra)

# 2. Cargar datos --------------------

df <- read_excel("input/data/original/orders_data.xlsx")

# 3. Realización del taller --------------------

# 1. Muestre la distribución del monto total pagado de los pedidos con un grafico y comente.

hist(df$itemtotal) #Version base
sjPlot::plot_frq(df$itemtotal, type = "histogram") # Sjplot

# 2. Montos pagados promedios por estado del pedido, desviación estándar y coeficiente de variación. Comente cada uno.

df %>% group_by(order_status) %>% summarise(
  Media = mean(itemtotal),
  SD = sd(itemtotal),
  CV = sd(itemtotal) / mean(itemtotal)
) %>% rename(Estado_Pedido = order_status) %>%
  kable(caption = "Estadísticos descriptivos del monto total por estado del pedido") %>% # Pasar a objeto kable y añadir título de tabla
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = T
  )

# 3. Distribución del monto pagado según mes de compra (gráfico de cajas, histograma y violín).

## Cajas

boxplot(df$itemtotal ~ df$month_date) ## Base
sjPlot::plot_grpfrq(df$itemtotal, df$month_date, type = "boxplot") # SjPlot

## Histograma
hist(df$itemtotal ~ df$month_date) ## Base

df %>%
  group_by(month_date) %>%
  plot_frq(itemtotal, type = "hist") %>%
  plot_grid()


## Violín


# 4. Top 5 ciudades (ship_city) con más pedidos realizados. Grafique.
# 
# 5. Realice un gráfico de barras que muestre la distribución porcentual de los estados de pedidos en el mes de Junio.
# 
# 6. Separe a los pedidos por tramo de costo de envío (menores a $10, entre $10 y $20, y mayores a $20). Reemplace los NA’s.

# Version profe
df <-
  df %>% mutate(
    tramo_envio_plantilla = case_when(
      shipping_fee > 20 ~ "Mayores_a_20",
      shipping_fee >= 10 ~ "Entre_10_y_20",
      shipping_fee < 10 ~ "Menores_a_10",
      is.na(shipping_fee) ~ "Gratuito"
    )
  )

# Version mar
df <-
  df %>% mutate(
    tramo_envio = case_when(
      shipping_fee > 20 ~ "Mayores a 20",
      shipping_fee %in% round(seq(10,20, by = 0.01), 2) ~ "Entre 10 y 20",
      shipping_fee < 10 ~ "Menores a 10",
      is.na(shipping_fee) ~ "Gratuito"
    )
  )

frq(df$tramo_envio_plantilla)
frq(df$tramo_envio)

# 7. Realice un análisis descriptivo del monto total pagado por tramo de costo de envío y comente sus resultados.
# 
# 8. Tabla de contingencia de la variable tramo de costo de envío y top 3 ciudades (*). Muestre la distribución porcentual de los tramos de envío, según ciudad. Haga un grafico de barras apilado.
# 
# 9. Describa el comportamiento que tienen aquellos pedidos que no son contra rembolso y su costo de envío sea menor a $10 o mayor a $20. Solucione NA’s.
# 
# 10. ¿Que porcentaje de los pedidos que no son contra reembolso, tienen un costo de envío mayor a $20?
#   
# • ¿Siempre nuestros datos faltantes se verán como casillas vacías?
# • Estudie qué ocurre con la variable order_status_2. ¿Qué podría significar el valor 99?
# • Si mezclamos dos clases de variable (por ejemplo, categórica y numérica) en una ¿Cómo operamos en el calculo de estadísticos?
# • Estudie qué ocurre con la variable itemtotal_2. Solucione datos faltantes.
# • Estudie funciones como; as.numeric(), as.character(), valores como: NA, argumentos de funciones como na.rm = TRUE para sum(), mean(), etc.