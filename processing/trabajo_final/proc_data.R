
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

# frq(epsoc_proc$sexo)
# frq(epsoc_proc$educ)
# frq(epsoc_proc$actividad_principal)
# frq(epsoc_proc$pueblo)
# frq(epsoc_proc$clase_perc)
# frq(epsoc_proc$educ_padre)
# frq(epsoc_proc$educ_madre)
# frq(epsoc_proc$hijos)
# 
# frq(epsoc_proc$bsjo1)
# frq(epsoc_proc$bsjo2)
# frq(epsoc_proc$bsjo3)
# frq(epsoc_proc$bsjo4)
# frq(epsoc_proc$bsjo5)
# frq(epsoc_proc$bsjo6)
# frq(epsoc_proc$bsjo7)
# frq(epsoc_proc$bsjo8)
# frq(epsoc_proc$bsjo9)
# frq(epsoc_proc$bsjo10)
# frq(epsoc_proc$bsjo11)
# frq(epsoc_proc$bsjo12)
# frq(epsoc_proc$bsjo13)
# frq(epsoc_proc$bsjo14)


# 5. Recodificar ----------------------------------------------------------

# NA's de bsjo

epsoc_proc_original <- epsoc_proc

epsoc_proc <- epsoc_proc %>% mutate(
  across(starts_with("bsjo"), ~ifelse(. %in% c(8, 9), NA, .))
  )

epsoc_proc <-sjlabelled::copy_labels(epsoc_proc, epsoc_proc_original)

epsoc_proc <- epsoc_proc %>% mutate(
  across(starts_with("bsjo"), ~remove_labels(., labels = c("No sabe [No leer]", "No responde [No leer]")))
  
)

# Cambiar etiquetas

# epsoc_proc$bsjo1 <- set_label(epsoc_proc$bsjo1, label = "IGUAL1. Todos mismas condiciones vida")
# epsoc_proc$bsjo2 <- set_label(epsoc_proc$bsjo2, label = "IGUAL2. Ingresos y riquezas distribuidas igualitariamente")
# epsoc_proc$bsjo3 <- set_label(epsoc_proc$bsjo3, label = "IGUAL3. Diferencias ingresos pequeñas")
# epsoc_proc$bsjo4 <- set_label(epsoc_proc$bsjo4, label = "NEC1. Sociedad cuida necesitados")
# epsoc_proc$bsjo5 <- set_label(epsoc_proc$bsjo5, label = "NEC2. Cuidadores reciben beneficios")
# epsoc_proc$bsjo6 <- set_label(epsoc_proc$bsjo6, label = "NEC3. Personas tienen nutrición")
# epsoc_proc$bsjo7 <- set_label(epsoc_proc$bsjo7, label = "EQUI1. Trabajan duro ganan más")
# epsoc_proc$bsjo8 <- set_label(epsoc_proc$bsjo8, label = "EQUI2. Personas reciben en base a sus esfuerzos")
# epsoc_proc$bsjo9 <- set_label(epsoc_proc$bsjo9, label = "EQUI3. Diferencias ingresos reflejen desempeño")
# epsoc_proc$bsjo10 <- set_label(epsoc_proc$bsjo10, label = "DER1. Familias respetables más ventajas")
# epsoc_proc$bsjo11 <- set_label(epsoc_proc$bsjo11, label = "DER2. Más estatus mejores condiciones de vida")
# epsoc_proc$bsjo12 <- set_label(epsoc_proc$bsjo12, label = "DER3. Buena reputación y riqueza más beneficios vejez")
# epsoc_proc$bsjo13 <- set_label(epsoc_proc$bsjo13, label = "EQUI4. Diferencias ingresos reflejan esfuerzo")
# epsoc_proc$bsjo14 <- set_label(epsoc_proc$bsjo14, label = "EQUI5. Más inteligencia/habilidades más ingresos")

epsoc_proc$bsjo1 <- set_label(epsoc_proc$bsjo1, label = "IGUAL1")
epsoc_proc$bsjo2 <- set_label(epsoc_proc$bsjo2, label = "IGUAL2")
epsoc_proc$bsjo3 <- set_label(epsoc_proc$bsjo3, label = "IGUAL3")
epsoc_proc$bsjo4 <- set_label(epsoc_proc$bsjo4, label = "NEC1")
epsoc_proc$bsjo5 <- set_label(epsoc_proc$bsjo5, label = "NEC2")
epsoc_proc$bsjo6 <- set_label(epsoc_proc$bsjo6, label = "NEC3")
epsoc_proc$bsjo7 <- set_label(epsoc_proc$bsjo7, label = "EQUI1")
epsoc_proc$bsjo8 <- set_label(epsoc_proc$bsjo8, label = "EQUI2")
epsoc_proc$bsjo9 <- set_label(epsoc_proc$bsjo9, label = "EQUI3")
epsoc_proc$bsjo10 <- set_label(epsoc_proc$bsjo10, label = "DER1")
epsoc_proc$bsjo11 <- set_label(epsoc_proc$bsjo11, label = "DER2")
epsoc_proc$bsjo12 <- set_label(epsoc_proc$bsjo12, label = "DER3")
epsoc_proc$bsjo13 <- set_label(epsoc_proc$bsjo13, label = "EQUI4")
epsoc_proc$bsjo14 <- set_label(epsoc_proc$bsjo14, label = "EQUI5")

# Vars categoricas
epsoc_proc <- epsoc_proc %>% mutate(
  educ_rec = case_when(
    educ %in% c(1:3) ~ 1, # Sin estudios o Básica
    educ %in% c(4:5) ~ 2, # Media
    educ %in% c(6:7) ~ 3, # Técnica Superior
    educ %in% c(8:10) ~ 4, # Universitaria y posgrado
    educ %in% c(88, 99) ~ 8899 # NSNR
  ),
  actividad_principal_rec = case_when(
    actividad_principal %in% c(1:2) ~ 1, # Trabajo en la ocupación
    actividad_principal %in% c(3:9) ~ 2, # No trabaja en la ocupación
    actividad_principal %in% c(88, 99) ~ 8899, # NSNR
  ),
  pueblo_rec = case_when(
    pueblo %in% (1:9) ~ 1, # Pertenece
    pueblo %in% (10) ~ 2, # No pertenece
    pueblo %in% c(88, 99) ~ 8899, # NSNR
  ),
  clase_perc_rec = case_when(
    clase_perc %in% c(4) ~ 1, # Clase baja
    clase_perc %in% c(3) ~ 2, # Clase media-baja
    clase_perc %in% c(1:2) ~ 3, # Clase media alta o alta
    clase_perc %in% c(8, 9) ~ 8899, # NSNR
  ),
  educ_padre_rec = case_when(
    educ_padre %in% c(1:3) ~ 1, # Sin estudios o Básica
    educ_padre %in% c(4:5) ~ 2, # Media
    educ_padre %in% c(6:7) ~ 3, # Técnica Superior
    educ_padre %in% c(8:10) ~ 4, # Universitaria y posgrado
    educ_padre %in% c(88, 99) ~ 8899 # NSNR
  ),
  educ_madre_rec = case_when(
    educ_madre %in% c(1:3) ~ 1, # Sin estudios o Básica
    educ_madre %in% c(4:5) ~ 2, # Media
    educ_madre %in% c(6:7) ~ 3, # Técnica Superior
    educ_madre %in% c(8:10) ~ 4, # Universitaria y posgrado
    educ_madre %in% c(88, 99) ~ 8899 # NSNR
  ),
  hijos_rec = case_when(
    hijos %in% c(1) ~ 1, # Ninguno
    hijos %in% c(2:3) ~ 2, # Uno o dos
    hijos %in% c(4:7) ~ 3, # Tres o más
    hijos %in% c(8, 9) ~ 8899, # NSNR
  )
)

# frq(epsoc_proc$sexo_rec)
# frq(epsoc_proc$educ_rec)
# frq(epsoc_proc$actividad_principal_rec)
# frq(epsoc_proc$pueblo_rec)
# frq(epsoc_proc$clase_perc_rec)
# frq(epsoc_proc$educ_padre_rec)
# frq(epsoc_proc$educ_madre_rec)
# frq(epsoc_proc$hijos_rec)

# 6. Pasar a factor -------------------------------------------------------

epsoc_proc$sexo_rec <- factor(
  epsoc_proc$sexo, levels = c(1:2),
  labels = c("Hombre", "Mujer")
)

epsoc_proc$educ_rec <- factor(
  epsoc_proc$educ_rec, levels = c(1:4, 8899),
  labels = c("Sin estudios o básica", "Media", "Técnica superior", "Universitaria y posgrado", "NSNR")
)

epsoc_proc$actividad_principal_rec <- factor(
  epsoc_proc$actividad_principal_rec, levels = c(1:2, 8899),
  labels = c("Trabajo en la ocupación", "No trabaja en la ocupación", "NSNR")
)

epsoc_proc$pueblo_rec <- factor(
  epsoc_proc$pueblo_rec, levels = c(1:2, 8899),
  labels = c("Pertenece", "No pertenece", "NSNR")
)

epsoc_proc$clase_perc_rec <- factor(
  epsoc_proc$clase_perc_rec, levels = c(1:3, 8899),
  labels = c("Clase baja" , "Clase media-baja", "Clase media-alta o alta", "NSNR")
)

epsoc_proc$educ_padre_rec <- factor(
  epsoc_proc$educ_padre_rec, levels = c(1:4, 8899),
  labels = c("Sin estudios o básica", "Media", "Técnica superior", "Universitaria y posgrado", "NSNR")
)

epsoc_proc$educ_madre_rec <- factor(
  epsoc_proc$educ_madre_rec, levels = c(1:4, 8899),
  labels = c("Sin estudios o básica", "Media", "Técnica superior", "Universitaria y posgrado", "NSNR")
)

epsoc_proc$hijos_rec <- factor(
  epsoc_proc$hijos_rec, levels = c(1:3, 8899),
  labels = c("Ninguno", "Uno o dos", "Tres o más", "NSNR")
)

# frq(epsoc_proc$sexo_rec)
# frq(epsoc_proc$educ_rec)
# frq(epsoc_proc$actividad_principal_rec)
# frq(epsoc_proc$pueblo_rec)
# frq(epsoc_proc$clase_perc_rec)
# frq(epsoc_proc$educ_padre_rec)
# frq(epsoc_proc$educ_madre_rec)
# frq(epsoc_proc$hijos_rec)

# 7. Omitir NA's ----------------------------------------------------------

epsoc_proc_original <- epsoc_proc

epsoc_proc <- na.omit(epsoc_proc)

epsoc_proc <-sjlabelled::copy_labels(epsoc_proc, epsoc_proc_original)


# 8. Guardar --------------------------------------------------------------

saveRDS(epsoc_proc, "input/data/proc/epsoc_proc.RDS")
