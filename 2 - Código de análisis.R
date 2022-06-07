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
library(caret)
library(glmnet)


# Fin del preambulo #
#=========================================================================

# a. Se crea función lineal

set.seed(1234)

x <- rnorm(100)
e <- rnorm(100)
y <- 1 + x + x^2 + x^3 + e

# b. Se selecciona mejor modelo 

df <- data.frame(y, x)

model <- regsubsets(y ~ poly(x, 10), 
                      data = df, 
                      nvmax = 10)
model_summary <- summary(model)
model_summary

min_cp <- which.min(model_summary$cp)
min_cp

min_bic <- which.min(model_summary$bic)
min_bic

max_adjr2 <- which.max(model_summary$adjr2)
max_adjr2



# Se visualizan valores

par(mfrow = c(2, 2))

plot(model_summary$cp, main = "Mejor Cp" , xlab = "Tamaño del subset", ylab= "Cp", pch = 20, type = "l")
points(3, model_summary$cp[3], pch = 4, col = "brown3",cex = 2, lwd = 3)

plot(model_summary$bic, main = "Mejor BIC" , xlab = "Tamaño del subset", ylab= "BIC", pch = 20, type = "l")
points(3, model_summary$bic[3], pch = 4, col = "brown3", cex = 2, lwd = 3)

plot(model_summary$adjr2, main = "Mejor R2 ajustado" , xlab = "Tamaño del subset", ylab= "R2 ajustado", pch = 20, type = "l")
points(3, model_summary$adjr2[3], pch = 4, col = "brown3", cex = 2, lwd = 3)



# c. Selección hacia adelante

model_fwd <- regsubsets(y ~ poly(x, 10), 
                    data = df, 
                    nvmax = 10, 
                    method = "forward")
model_fwd_summary <- summary(model_fwd)
model_fwd_summary

min_cp <- which.min(model_fwd_summary$cp)
min_cp

min_bic <- which.min(model_fwd_summary$bic)
min_bic

max_adjr2 <- which.max(model_fwd_summary$adjr2)
max_adjr2

# Se visualizan valores

par(mfrow = c(2, 2))

plot(model_fwd_summary$cp, main = "Mejor Cp" , xlab = "Tamaño del subset", ylab= "Cp", pch = 20, type = "l")
points(3, model_fwd_summary$cp[3], pch = 4, col = "brown3",cex = 2, lwd = 3)

plot(model_fwd_summary$bic, main = "Mejor BIC" , xlab = "Tamaño del subset", ylab= "BIC", pch = 20, type = "l")
points(3, model_fwd_summary$bic[3], pch = 4, col = "brown3", cex = 2, lwd = 3)

plot(model_fwd_summary$adjr2, main = "Mejor R2 ajustado" , xlab = "Tamaño del subset", ylab= "R2 ajustado", pch = 20, type = "l")
points(3, model_fwd_summary$adjr2[4], pch = 4, col = "brown3", cex = 2, lwd = 3)

coefficients(model_fwd, id = 3)
coefficients(model_fwd, id = 4)


#COMENTAR!!!

# d.Lasso y Cross-validation

matrix = model.matrix(y ~ poly(x, 10, raw = T), data = df)



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


