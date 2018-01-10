ObjectsCountOfEachClass <- 5
## Подключаем библиотеку MASS для генерации многомерногонормального распределения
library(MASS)
library(kernlab)
library(e1071)
## Генерируем тестовые данные
Sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
Sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)
Mu1 <- c(0, 1)
Mu2 <- c(0, 1)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
## Собираем два класса в одну выборку
xl <- rbind(cbind(xy1, 1), cbind(xy2, -1))
x1s <-c(xy1[,1],xy1[,2])
#x1s <- c(.5,1,1,2,3,3.5,     1,3.5,4,5,5.5,6)
x2s <-c(xy2[,1],xy2[,2])
#x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,5),          rep(-1,5))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))

svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear', cost=1, scale=FALSE)

plot(my.data[,-3],col=(ys+3)/2, pch=19)
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) # show the support vectors

# get parameters of hiperplane
w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
# in this 2D case the hyperplane is the line w[1,1]*x1 + w[1,2]*x2 + b = 0
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="blue", lty=5)
abline(a=-(b+1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3)
abline(a=-(b-1)/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=3)
