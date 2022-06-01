##########################################################################
############--------------Universidad ORT Uruguay---------------##########
############--------Obligatorio Machine Learning Supervisado----##########
############--------------------Cecilia Machado-----------------##########
############----------------Prof. Santiago Acerenza-------------##########
##########################################################################

#============================= PARTE 1 ===================================

# Preámbulo 1 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio MLS")

# Cargamos libreria a utilizar
library(ISLR)
library(leaps)

# Fin del preambulo #
#=========================================================================

#******************************************
# Modelo lineal Y Regularización de métodos 
#******************************************

# Se crea función lineal

set.seed(1234)

x <- rnorm(100)
e <- rnorm(100)
Y <- 1 + x + x^2 + x^3 + e

best_model_1 <- regsubsets(Y ~ ., 
                         data = x, 
                       nvmax = x^10)

#============================= PARTE 2 ===================================

# Preámbulo 2 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio MLS")

# Cargamos libreria a utilizar
library(MASS)
library(ISLR)
library(car)

# Cargamos datos
datos <-read.table("Auto.data", 
                   header = T, 
                   na.strings = "?", 
                   stringsAsFactors = T)

# Visualizamos datos
View(datos)
dim(datos)
names(datos)

# Omitimos valores NA
datos <- na.omit(datos)
dim(datos)
View(datos)

# Fin del preambulo #
#=========================================================================

# Diagrama de dispersión con todas las varibles

diagrama_disp <- pairs(~ ., col = factor(mtcars$am), pch = 19, data = datos)


# Matriz de correlaciones 

correlaciones <- cor(datos[,-9])
correlaciones
  
# Regresión lineal múltiple para la variable "mpg"

reg_multtiple_mpg <- lm(mpg ~ . - name, data = datos)
summary(reg_multtiple_mpg)
vif(reg_multtiple_mpg)

#COMENTAR!!

# Verificar fit del modelo 

fit <- plot(reg_multtiple_mpg)

# VER

plot(predict(reg_multtiple_mpg), residuals(reg_multtiple_mpg))
plot(predict(reg_multtiple_mpg), rstudent(reg_multtiple_mpg))

plot(hatvalues(reg_multtiple_mpg))
which.max(hatvalues(reg_multtiple_mpg))

#COMENTAR!!

# Modelo lineal con interacciones

summary(lm(mpg ~ .* horsepower , data = datos[,-9]))

#COMENTAR!!


