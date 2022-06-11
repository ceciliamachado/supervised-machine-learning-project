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
# ya que el p-value obtenido para el estadístico F es muy pequeño (p-value= < 2.2e-16). 

# El modelo es capaz de explicar el 82% de la variablilidad observada (r2aj= 0.8182)

# Algunos de los predictores tienen p-values muy altos, sugiriendo que no contribuyen al modelo 
# por ejemplo, cylinders, horsepower y acceleration. Por el contrario, los predictores weight, year, origin y displacement
# tienen p-values más pequeños, por lo cual, estos le aportan más significancia de relación al modelo. 


# d.Verificar fit del modelo 

par(mfrow= c(2,2))

fit <- plot(reg_multtiple_mpg)

# Se observa una no linealidad entre los residuos, por lo cual, hay componentes no lineles
# del modelo que están quedando sin modelar.


# e.Modelo lineal con interacciones

summary(lm(formula= mpg ~ .*. , data = datos[,-9]))


# Se observa que las variables más significativas en este modelo son acceleration y origin 
# al igual que su interacción entre ambas ya que obtuvieron los niveles más bajos de significancia
# (0.00735, 0.00345, 0.00365 respectivamente).

# También se puede ver que hay predictores con un solo asterisco, pero estas no serían lo
# suficientemente significativas para este modelo debido al gran número de predictores que contiene.

# f. Testear hipótesis Beta1=Beta2=0

# H0: Si Beta1 = 0 
# H1: Beta1 != 0

coef(summary(reg_multtiple_mpg))

#============================= PARTE 2 ===================================

# Preambulo 3 #

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
setwd("C:/Users/Cecilia Machado/Documents/GitHub/supervised-machine-learning-project")

# Cargamos libreria a utilizar
library(ISLR2)
library(MASS)
library(class)

# Cargamos datos
datos <- Weekly

# Visualizamos datos
View(datos)
dim(datos)
names(datos)


# Fin del preambulo #
#=========================================================================

# a. Medidas de resumen

summary(datos)

pairs(datos)

cor(datos[, -9]) #se excluye Direction porque es una variable cualitativa.

# Se denota una clara relación no lineal entre las variables Year y Volume.
# No se distinguen otros patrones entre las restantes variables.

# b. Regresión Logística 

attach(datos)

reg_log <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = datos, 
               family = binomial)
summary(reg_log)

# La única variable que parece tener significancia estadística es Log2, 
# debido a que tiene el p-value más bajo (0.0296)

# c. Matriz de confusión

glm_probs <- predict(reg_log, type = "response") 
glm_pred <- rep("Down", length(glm_probs))
glm_pred[glm_probs > 0.5] = "Up"
tabla <- table(glm_pred, Direction)
tabla

falso_positivo <- paste(round(tabla[2,2] / (tabla[2,2] + tabla[1,2])*100, 1), "%", sep = "")
falso_positivo

falso_negativo <- paste(round(tabla[1,1] / (tabla[2,1] + tabla[1,1])*100, 1), "%", sep = "")
falso_negativo

# d. Ajuste de modelo logístico (LDA)

train <- (Year < 2009)
datos_0910 <- datos[!train, ]

reg_log2 <- glm(Direction ~ Lag2, 
                data = datos, 
                family = binomial, 
                subset = train)
glm_probs2 <- predict(reg_log2, datos_0910 , type = "response")
glm_pred2 <- rep("Down", length(glm_probs2))
glm_pred2[glm_probs2 > 0.5] = "Up"
Direction_0910 <- Direction[!train]
tabla <- table(glm_pred2, Direction_0910)
tabla

mean(glm_pred2 == Direction_0910)

# e. Discriminante lineal

lda_fit = lda(Direction ~ Lag2, data = datos, subset = train)
lda_pred = predict(lda.fit, datos_0910)
tabla <- table(lda.pred$class, Direction_0910)
tabla

mean(lda.pred$class == Direction_0910)

# f. Discriminante cuadrático

qda.fit = qda(Direction ~ Lag2, data = datos, subset = train)
qda.class = predict(qda.fit, datos_0910)$class
tabla <- table(qda.class, Direction_0910)
tabla

mean(qda.class == Direction_0910)

# g. KNN

train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
tabla <- table(knn.pred, Direction_0910)
tabla

mean(knn.pred == Direction_0910)

# ¿Qué método ajusta mejor?

# Los métodos que ajustan mejor son el de Regresión logística y LDA
# ya que arrojan similares tasas de error. 