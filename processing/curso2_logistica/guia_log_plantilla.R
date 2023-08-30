## Trabajo guiado
## Cargamos la base de datos y revisamos
d <- readxl::read_excel("Depre1.xlsx", sheet="CIDI")
str(d)
table(d$Depresion)
round(prop.table(table(d$Depresion))*100,2)
## hay un 11% de entrevistados en categoria DEPRE

## Tabla Depresion vs Sexo

round(prop.table(table(d$Depresion, d$Sexo),2)*100,2)
## solo el 6% de lo hombres son DEPRE vs el 13,8% de las mujeres
chisq.test(table(d$Depresion,d$Sexo), correct=F)
## asociacion MUY significativa, valor-p ~ 0,0

## Tabla y test oara EdadC
round(prop.table(table(d$Depresion,d$EdadC),2)*100,2)
chisq.test(table(d$Depresion,d$EdadC), correct=F)
## significativo, pero menos importante...

## Ecivil
round(prop.table(table(d$Depresion,d$Ecivil),2)*100,1)
chisq.test(table(d$Depresion,d$Ecivil), correct=F)

## Nivel educacional
round(prop.table(table(d$Depresion,d$Neduc),2)*100,1)
chisq.test(table(d$Depresion,d$Neduc), correct=F)

## Percepcion de salud fisica
round(prop.table(table(d$Depresion,d$Sfisica),2)*100,1)
chisq.test(table(d$Depresion,d$Sfisica), correct=F)

## Percepcion de salud mental
round(prop.table(table(d$Depresion,d$Smental),2)*100,1)
chisq.test(table(d$Depresion,d$Smental), correct=F)

## Crisis de miedo
round(prop.table(table(d$Depresion,d$Miedo),2)*100,1)
chisq.test(table(d$Depresion,d$Miedo), correct=F)

## Tristeza
round(prop.table(table(d$Depresion,d$Tristeza),2)*100,1)
chisq.test(table(d$Depresion,d$Tristeza), correct=F)


## En resumen, lo mas relevante es TRISTEZA
## modelo simple:  Depresion <- tristeza
m1<-glm(Depresion~Tristeza, data=d, family=binomial)
summary(m1)
## OR = exp(coeficiente)
exp(m1$coef)
## OR = 46 .. muy alto!
DescTools::PseudoR2(m1,"N")

## Tres variables Adicionales
# MIEDO, SMENTAL, SFISICA ** as.factor

d$Smental <- as.factor(d$Smental)
d$Sfisica <- as.factor(d$Sfisica)

m2<-glm(Depresion~Tristeza+Miedo+Sfisica+ Smental, 
                    data=d, family=binomial)
summary(m2)

cbind(exp(m2$coefficients))

DescTools::PseudoR2(m2,"N")

## Curva ROC y AUC
InformationValue::plotROC(actuals = d$Depresion,
        predictedScores = fitted.values(m2))

## AUROC = 86%

## Matriz de confusion..

p <- InformationValue::optimalCutoff(actuals = d$Depresion,
           predictedScores = fitted.values(m2), optimiseFor = "Both")
p

install.packages("caret")
d$Pred <- ifelse(fitted.values(m2)>p,1,0)       # predicciones
xt<-table(d$Pred, d$Depresion)                 # almacenar en tabla
caret::confusionMatrix(xt, positive="1")   

