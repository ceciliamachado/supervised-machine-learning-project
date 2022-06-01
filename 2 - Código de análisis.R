##########################################################################
############--------------Universidad ORT Uruguay---------------##########
############--------Obligatorio Machine Learning Supervisado----##########
############--------------------Cecilia Machado-----------------##########
############----------------Prof. Santiago Acerenza-------------##########
##########################################################################

#=========================================================================

# Preámbulo 1 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio MLS")

# Cargamos libreria a utilizar
library(ISLR)
library(leaps)

# Fin del preambulo #
#====================================================================

#**********
# Parte 1 
#**********

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

#=========================================================================

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
datos <-read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)

# Visualizamos datos
View(datos)
dim(datos)
names(datos)

# Omitimos valores NA
datos <- na.omit(datos)
dim(datos)
View(datos)

# Fin del preambulo #
#====================================================================

#**********
# Parte 2 
#**********

# Diagrama de dispersión con todas las varibles

diagrama_disp <- pairs(datos)

# Matriz de correlaciones 

correlaciones <- cor(datos[,-9])
correlaciones
  
# Regresión lineal múltiple para la variable "mpg"

reg_multtiple_mpg <- lm(mpg ~ . - name, data = datos)
summary(reg_multtiple_mpg)

# Verificar fit del modelo 

fit <- plot(reg_multtiple_mpg)



