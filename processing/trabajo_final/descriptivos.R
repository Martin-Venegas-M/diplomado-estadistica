
###########################################################################
# 0. Identificación -------------------------------------------------------
#Título: Código de descriptivos de EPSOC 2018
#Institución: PUC
#Encargado: Martín Venegas - Estudiante

# Resumen ejecutivo: El presente documento contiene el código para calcular los
# los descriptivos de EPSOC 2018 procesada
# El objetivo es poder contar con tablas y gráficos para la realización
# del trabajo final
# El producto de este script serán distintas tablas y gráficos de descriptivos
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

sjPlot::set_theme(
  base = theme_bw()
)

# 2. Cargar datos y funciones ----------------------------------------------

epsoc <- readRDS("input/data/proc/epsoc_proc.RDS")

source("processing/trabajo_final/functions.R")


# 3. Plots BSJO -----------------------------------------------------------

plot_stackfrq(epsoc %>% select(bsjo1:bsjo7), show.total = FALSE, title = "Porcentajes de respuesta a escala BSJO") + theme(legend.position = "bottom")
plot_stackfrq(epsoc %>% select(bsjo1:bsjo7), show.total = FALSE, title = "Porcentajes de respuesta a escala BSJO", weight.by = epsoc$factor) + theme(legend.position = "bottom")


plot_stackfrq(epsoc %>% select(bsjo8:bsjo14), show.total = FALSE, title = "Porcentajes de respuesta a escala BSJO (continuación)") + theme(legend.position = "bottom")
plot_stackfrq(epsoc %>% select(bsjo8:bsjo14), show.total = FALSE, title = "Porcentajes de respuesta a escala BSJO (continuación)", weight.by = epsoc$factor) + theme(legend.position = "bottom")


# 4. Tabs vars ------------------------------------------------------------


