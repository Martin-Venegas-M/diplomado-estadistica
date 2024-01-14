
###########################################################################
# 0. Identificación -------------------------------------------------------
#Título: Código de procesamiento de EPSOC 2018
#Institución: PUC
#Encargado: Martín Venegas - Estudiante

# Resumen ejecutivo: El presente documento contiene el código para procesar los
# datos correspondientes a la encuesta EPSOC 2018
# El objetivo es poder contar con una tabla de datos lista para los análisis
# descriptivos y multivariados
# El producto de este script será la tabla de datos con las variables listas 
# para ser analizadas.
###########################################################################

# 1. Cargar librerías -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar

pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               summarytools,
               sjlabelled,
               car,
               haven)

# 2. Cargar datos ---------------------------------------------------------

epsoc <- readRDS("input/data/original/epsoc_public.rds")


# 3. Seleccionar datos ----------------------------------------------------

epsoc_proc <- epsoc %>% select(
  folio,
  id = sbj_num,
  region,
  psu,
  estrato,
  factor = rake_factor_corregido,
  edad = edad_seleccionado,
  sexo = sexo_enc,
  educ = f1,
  actividad_principal = f2,
  pueblo = f15,
  clase_perc = f16,
  educ_padre = f13,
  educ_madre = f14,
  hijos = f22,
  
  bsjo1 = i_1_h1, # Igualdad
  bsjo2 = i_2_h1, # Igualdad
  bsjo3 = i_3_h1, # Necesidad # OPCIONAL
  bsjo4 = i_4_h1, # Necesidad
  bsjo5 = i_5_h1, # Necesidad
  bsjo6 = i_6_h1, # Necesidad # OPCIONAL
  bsjo7 = i_7_h1, # Equidad
  bsjo8 = i_8_h1, # Equidad
  bsjo9 = i_9_h1, # Equidad # OPCIONAL
  bsjo10 = i_10_h1, # Derecho
  bsjo11 = i_11_h1, # Derecho
  bsjo12 = i_12_h1, # Derecho # OPCIONAL
  bsjo13 = i_13_h1, # Equidad # No están en la escala original
  bsjo14 = i_14_h1, # Equidad # No están en la escala original
)


# 4. Ver freqs ------------------------------------------------------------

frq(epsoc_proc$sexo)
frq(epsoc_proc$educ)
frq(epsoc_proc$actividad_principal)
frq(epsoc_proc$pueblo)
frq(epsoc_proc$clase_perc)
frq(epsoc_proc$educ_padre)
frq(epsoc_proc$educ_madre)
frq(epsoc_proc$hijos)

frq(epsoc_proc$bsjo1)
frq(epsoc_proc$bsjo2)
frq(epsoc_proc$bsjo3)
frq(epsoc_proc$bsjo4)
frq(epsoc_proc$bsjo5)
frq(epsoc_proc$bsjo6)
frq(epsoc_proc$bsjo7)
frq(epsoc_proc$bsjo8)
frq(epsoc_proc$bsjo9)
frq(epsoc_proc$bsjo10)
frq(epsoc_proc$bsjo11)
frq(epsoc_proc$bsjo12)
frq(epsoc_proc$bsjo13)
frq(epsoc_proc$bsjo14)


# 5. Recodificar ----------------------------------------------------------


