# 0. Identificacion ----------------------------------
#Título: Codigo elaboración del Taller 7 del Diplomado de Estadística - UC
#Institucion: Pontificia Universidad Católica de Chile
#Encargado: Martín Venegas - Estudiante
# Resumen ejectuvo: El presente documento contiene el codigo para la realización del septimo taller del diplomado.

# 1. Cargar paquetes ------------------------

if (!require("pacman")) install.packages("pacman")  #si falta pacman, instalar
pacman::p_load(tidyverse,
               sjmisc,
               readxl,
               sjPlot,
               kableExtra,
               gridExtra)

# 2. Cargar datos --------------------