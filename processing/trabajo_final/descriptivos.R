
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

# igual <- plot_stackfrq(epsoc %>% select(bsjo1:bsjo3), sort.frq =, show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")
# nec <- plot_stackfrq(epsoc %>% select(bsjo4:bsjo6), sort.frq =, show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")
# equi <- plot_stackfrq(epsoc %>% select(bsjo7:bsjo9, bsjo13:bsjo14), show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")
# der <- plot_stackfrq(epsoc %>% select(bsjo10:bsjo12), show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")

igual <- plot_stackfrq(epsoc %>% select(bsjo1:bsjo3), sort.frq =, show.total = FALSE, show.legend = F, wrap.legend.labels = 30)
nec <- plot_stackfrq(epsoc %>% select(bsjo4:bsjo6), sort.frq =, show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")
equi <- plot_stackfrq(epsoc %>% select(bsjo7:bsjo9, bsjo13:bsjo14), show.total = FALSE, show.legend = F, wrap.legend.labels = 30)
der <- plot_stackfrq(epsoc %>% select(bsjo10:bsjo12), show.total = FALSE, show.legend = T, wrap.legend.labels = 30) + theme(legend.position = "bottom")


p1 <- gridExtra::arrangeGrob(igual, nec)
p2 <- gridExtra::arrangeGrob(equi, der)

# ggsave(filename = "igual.png", igual, path = "output/trabajo_final", units = "cm", width = 30, height = 15, limitsize = F)
# ggsave(filename = "nec.png", nec, path = "output/trabajo_final", units = "cm",   width = 30, height = 15, limitsize = F)
# ggsave(filename = "equi.png", equi, path = "output/trabajo_final", units = "cm",  width = 30, height = 15, limitsize = F)
# ggsave(filename = "der.png", der, path = "output/trabajo_final", units = "cm",   width = 30, height = 15, limitsize = F)

ggsave(filename = "plot1.png", p1, path = "output/trabajo_final", units = "cm", width = 30, height = 15, limitsize = F)
ggsave(filename = "plot2.png", p2, path = "output/trabajo_final", units = "cm",   width = 30, height = 15, limitsize = F)

# 4. Tabs vars ------------------------------------------------------------

frq(epsoc$sexo_rec, show.na = F)
frq(epsoc$educ_rec, show.na = F)
frq(epsoc$actividad_principal_rec, show.na = F)
frq(epsoc$pueblo_rec, show.na = F)
frq(epsoc$clase_perc_rec, show.na = F)
frq(epsoc$educ_padre_rec, show.na = F)
frq(epsoc$educ_madre_rec, show.na = F)
frq(epsoc$hijos_rec, show.na = F)


# 5. Correlaciones --------------------------------------------------------

bsjo <- epsoc %>% select(
  IGUAL1 = bsjo1,
  IGUAL2 = bsjo2,
  IGUAL3 = bsjo3,
  NEC1 = bsjo4,
  NEC2 = bsjo5,
  NEC3 = bsjo6,
  EQUI1 = bsjo7,
  EQUI2 = bsjo8,
  EQUI3 = bsjo9,
  EQUI4 = bsjo13,
  EQUI5 = bsjo14,
  DER1 = bsjo10,
  DER2 = bsjo11,
  DER3 = bsjo12
)

sjPlot::tab_corr(bsjo, triangle = "lower")
