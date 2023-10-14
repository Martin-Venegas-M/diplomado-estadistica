# 0. Identificacion ----------------------------------------------------------------------------------------------------------------------------------------
#Título: Codigo elaboración del Control 1 del curso 3 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del primer control del tercer curso del diplomado.

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
               samplingbook
)

# 2. Cargar datos -------------------------------------------------------------------------------------------------------------------------------------------

df <- read_excel("input/data/original/Salud2017.xlsx")

# 3. Recodificaciones y etiquetado --------------------------------------------------------------------------------------------------------------------------

df <- df %>% mutate(
  Diabetes = if_else(Diabetes == "Si", 1, 0),
  IMC = as.numeric(IMC)
)

frq(df$Diabetes)
frq(df$IMC)

# 4. Comienza prueba ----------------------------------------------------------------------------------------------------------------------------------------

######################################################### INSTRUCCIONES #####################################################################################

# El MINSAL liberó una base de datos con 6233 casos analizados en el contexto de salud. 
# Entre la información disponible destaca el IMC, sexo, edad y la presencia de enfermedades tales como Hipertensión, Diabetes y Asma, entre otras. 
# Sobre esta información usted, junto a su(s) colega(s), tendrán que trabajar usando la librería samplingbook (datos - ver base SALUD2017.XLSX).

# Primera parte:  Interesa estimar, con un 95% de confianza:
# a)	Proporción de diabéticos con un error del 5%
# b)	El nivel medio del IMC con un error de un punto.
# 
# Para la determinación óptima del tamaño muestral utilice la siguiente información disponible de la encuesta nacional de salud previa: 
# El 12% padece de diabetes y el IMC medio fue de 29 con una desviación estándar de 5 puntos.
# 
# Use como semilla en la selección set.seed(fecha nacimiento del jefe de grupo) – la que se debe indicar en el informe. 
# Proceda a seleccionar desde la base SALUD2017 el número de casos determinado en el punto anterior, y con esta información complete la siguiente tabla


# REQUERIMIENTOS:

# - Unidad de muestreo: Personas
# - Unidad de análisis: Personas
# - Tamaño poblacional (N): 6233
# - Parametro a estimar:

# a)	Proporción de diabéticos con un error del 5%
# b)	El nivel medio del IMC con un error de un punto.

# - Error de estimación aceptado (e):

# a)	Error del 5% (0.05)
# b)	Error de un punto (1).

# - Parametro aproximado (¿antecedente o conservador?) (P o S):

# a)	P = 12%% (0.12)
# b)	S = 5 

# - Tipo de muestreo (MAS o ME): MAS
# - Estrato (h): NO APLICA
# - Método de afijación (proporcional u optimo): NO APLICA

##############################################################################################################################################################

############################################################## PRIMERA PARTE #################################################################################

# 4.1 Ver variables ------------------------------------------------------------------------------------------------------------------------------------------

frq(df$EdadCat)
frq(df$Diabetes)
descr(df$IMC)

# 4.2 Calcular tamaños de muestra ----------------------------------------------------------------------------------------------------------------------------

# a)	Proporción de diabéticos con un error del 5%
n_prop <- sample.size.prop(
  e = 0.05, 
  P = 0.12, 
  N = NROW(df)
  )

# b)	El nivel medio del IMC con un error de un punto.
n_mean <- sample.size.mean(
  e = 1, 
  S = 5, 
  N = NROW(df)
)

# 4.3 Muestrear ----------------------------------------------------------------------------------------------------------------------------

# Muestrar numeros aleatorios
set.seed(06051997) # 6 de mayo del 1997
# sample_prop <- sample(1:NROW(df), size = n_prop$n, replace = FALSE)
# sample_mean <- sample(1:NROW(df), size = n_prop$n, replace = FALSE) # ojo, se usa el tamaño de muestra de la prop para maximalismo

sample_mas <- sample(1:NROW(df), size = 193, replace = FALSE)

# Filtrar base con los nums aleatorios
# df_sample_prop <- df[sample_prop, ]
# df_sample_mean <- df[sample_mean, ]

df_sample_mas <- df[sample_mas, ]

df_sample_mas <- df_sample_mas %>% filter(!is.na(IMC))

# Ver n de cats de edad
frq(df_sample_mas$EdadCat)

# Ver estimaciones

# a)	Proporción de diabéticos con un error del 5%
Sprop(df_sample_mas$Diabetes, N = NROW(df))


df_sample_mas %>% 
  group_by(EdadCat) %>% 
  summarise(promedio = mean(Diabetes), 
            sd = sd(Diabetes),
            n = n(),
            se = sd/sqrt(n)) 

# b)	El nivel medio del IMC con un error de un punto.
Smean(df_sample_mas$IMC, N = NROW(df))


df_sample_mas %>% 
  group_by(EdadCat) %>% 
  summarise(promedio = mean(IMC), 
            sd = sd(IMC),
            n = n(),
            se = sd/sqrt(n)) 

############################################################## SEGUNDA PARTE #################################################################################

# 4.4 Calcular tamaños de muestra ----------------------------------------------------------------------------------------------------------------------------

# Tamaño poblacional estratos

Nh <- c(
  1296, # 15-29  
  3010, # 30-59
  1927 # 60+
)

# a)	Proporción de diabéticos con un error del 5%

PQ <- c(
  0.04 * (1 - 0.04), # 15-29
  0.10 * (1 - 0.10), # 30-59
  0.24 * (1 - 0.23) # 60+
)

Sh_prop <- sqrt(PQ)

stratasize(e = 0.05, Nh = Nh, Sh = Sh_prop, type = "opt")
stratasamp(n = 152, Sh = Sh_prop, Nh = Nh, type = "prop")


# b)	El nivel medio del IMC con un error de 0.7 puntos.

Sh = c(
  4, # 15-29
  5, # 30-59
  7 # 60+
  )

stratasize(e = 0.7, Nh = Nh, Sh = Sh, type = "opt")
stratasamp(n = 222, Sh = Sh, Nh = Nh, type = "prop")

# n minimo 222 + 47 de sobremuestreo

n1 = 46 + round(46 * 0.231)
n2 = 107 + round(107 * 0.231)
n3 = 69 + round(69 * 0.231)

# 4.5 Muestrear ----------------------------------------------------------------------------------------------------------------------------

# Muestrar numeros aleatorios
set.seed(06051997) # 6 de mayo del 1997

muestra1 <- df %>% filter(EdadCat == "15-29") %>% sample_n(n1)
muestra2 <- df %>% filter(EdadCat == "30-59") %>% sample_n(n2)
muestra3 <- df %>% filter(EdadCat == "60 y +") %>% sample_n(n3)

muestraR <- rbind(muestra1, muestra2, muestra3)
muestraR <- muestraR %>% filter(!is.na(IMC))

frq(muestraR$EdadCat)

# ESTIMACIONES

stratamean(muestraR$IMC, muestraR$EdadCat, Nh, eae = TRUE)
stratamean(muestraR$Diabetes, muestraR$EdadCat, Nh, eae = TRUE)
