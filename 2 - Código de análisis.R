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


