##########################################################################
############--------------Universidad ORT Uruguay---------------##########
############--------Obligatorio Machine Learning Supervisado----##########
############--------------------Cecilia Machado-----------------##########
############----------------Prof. Santiago Acerenza-------------##########
##########################################################################

#============================= PARTE 1 ===================================

# Preambulo 1 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Documents/GitHub/supervised-machine-learning-project")

# Cargamos libreria a utilizar
library(ISLR)
library(leaps)
library(caret)
library(glmnet)


# Fin del preambulo #
#=========================================================================

# a. Se crea funcion lineal

set.seed(1)

x <- rnorm(100)
e <- rnorm(100)

b0 <- 2
b1 <- 3
b2 <- -1
b3 <- 0.5

y <- b0 + b1*x + b2*x^2 + b3*x^3

# b. Se selecciona mejor modelo 

df <- data.frame(y, x)

model <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), 
                      data = df, 
                      nvmax = 10)

model_summary <- summary(model)
model_summary

which.min(model_summary$cp)
which.min(model_summary$bic)
which.max(model_summary$adjr2)

# Se visualizan valores

par(mfrow = c(2, 2))


plot(model_summary$cp, main = "Mejor Cp" , xlab = "Tamaño del subset", ylab= "Cp", pch = 20, type = "l")
points(3, model_summary$cp[3], pch = 4, col = "brown3",cex = 2, lwd = 3)
plot(model_summary$bic, main = "Mejor BIC" , xlab = "Tamaño del subset", ylab= "BIC", pch = 20, type = "l")
points(3, model_summary$bic[3], pch = 4, col = "brown3", cex = 2, lwd = 3)
plot(model_summary$adjr2, main = "Mejor R2 ajustado" , xlab = "Tamaño del subset", ylab= "R2 ajustado", pch = 20, type = "l")
points(3, model_summary$adjr2[3], pch = 4, col = "brown3", cex = 2, lwd = 3)


coef(model, which.max(model_summary$adjr2))


# c. Selecci?n hacia adelante

model_fwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), 
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

plot(model_fwd_summary$cp, main = "Mejor Cp" , xlab = "Tama?o del subset", ylab= "Cp", pch = 20, type = "l")
points(3, model_fwd_summary$cp[3], pch = 4, col = "brown3",cex = 2, lwd = 3)

plot(model_fwd_summary$bic, main = "Mejor BIC" , xlab = "Tama?o del subset", ylab= "BIC", pch = 20, type = "l")
points(3, model_fwd_summary$bic[3], pch = 4, col = "brown3", cex = 2, lwd = 3)

plot(model_fwd_summary$adjr2, main = "Mejor R2 ajustado" , xlab = "Tama?o del subset", ylab= "R2 ajustado", pch = 20, type = "l")
points(3, model_fwd_summary$adjr2[3], pch = 4, col = "brown3", cex = 2, lwd = 3)

coefficients(model_fwd,3)
coefficients(model_fwd, id = 4)


#COMENTAR!!!

# d.Lasso y Cross-validation

matrix = model.matrix(y ~ poly(x, 10, raw = T), data = df)



#============================= PARTE 2 ===================================

# Pre?mbulo 2 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Documents/GitHub/supervised-machine-learning-project")

# Cargamos libreria a utilizar
library(MASS)
library(ISLR2)
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

# a.Diagrama de dispersion con todas las varibles

diagrama_disp <- pairs(~ ., col = factor(mtcars$am), pch = 19, data = datos)


# b.Matriz de correlaciones 

correlaciones <- cor(datos[,-9])
correlaciones
  
# c.Regresion lineal multiple para la variable "mpg"

reg_multtiple_mpg <- lm(mpg ~ . - name, data = datos)
summary(reg_multtiple_mpg)

# Se puede afirmar que al menos uno de los predictores introducidos en el modelo está relacionado con la variable mpg
# ya que el p-value obtenido para el estadístico F es muy pequeño. 

# El modelo es capaz de explicar el 82% de la variablilidad observada (r2= 0.8182)

# Algunos de los predictores tienen p-values muy altos, sugiriendo que no contribuyen al modelo por lo que deben ser excluidos
# por ejemplo, cylinders, horsepower y acceleration. Por el contrario, los predictores weight, year, origin y displacement
# tienen p-values más pequeños, por lo cual, estos le aportan más significancia de relación al modelo. 


# d.Verificar fit del modelo 

par(mfrow= c(2,2))

fit <- plot(reg_multtiple_mpg)

# VER

plot(predict(reg_multtiple_mpg), residuals(reg_multtiple_mpg))
plot(predict(reg_multtiple_mpg), rstudent(reg_multtiple_mpg))
plot(hatvalues(reg_multtiple_mpg))
which.max(hatvalues(reg_multtiple_mpg))



#COMENTAR!!

# e.Modelo lineal con interacciones

summary(lm(mpg ~ .* horsepower , data = datos[,-9]))

#COMENTAR!!

# f. Teastear hipótesis

