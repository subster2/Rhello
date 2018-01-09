colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

CoreGauss <- function(point) {
  (2*pi)^(-0.5)*exp(-0.5*point^2)
}

naiveBayesianClassifier <- function(point) { 
  n <- 3:4 # numbers of features
  l <- dim(iris)[1] # 150
  m <- length(levels(iris$Species)) # number of classes (3)
  #P_apriori <- c(0,0,0) # apriori probability
  p <- c(0,0,0) # distribution density
  
  
  answer <- data.frame(1:m, levels(iris$Species))
  colnames(answer) <- c("OptimalBayesianRule", "Species")
  P_apriori <- 1 / 3 # apriori probability
  petalwidth <- c(0.246,1.326,2.026)
  #petalwidth <- c(0.101,0.545,0.832)
  petalwidthSigm <- c(0.1054,0.1978,0.2747)
  #petalwidthSigm <- c(0.297,0.557,0.690)
  petalenght <- c(1.462,4.26,5.552)
  #petalenght <- c(0.204,0.596,0.776)
  petalenghtSigm <- c(0.1737,0.4699,0.5519)
  #petalenghtSigm <- c(0.2330,0.6304,0.7405)
  
  
  #p[1]<-log2((1/(sqrt(2*pi)*petalwidthSigm[1]))*exp(-((point[4]-petalwidth[1])^2)/2*petalwidthSigm[1]^2))
  #p[1]<-log2(P_apriori) + p[1] + log2((1/(sqrt(2*pi)*petalenghtSigm[1]))*exp(-((point[3]-petalenght[1])^2)/2*petalenghtSigm[1]^2))
  p[1]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[1], sd = petalwidthSigm[1], log = FALSE)) +log2(dnorm(point[3], mean = petalenght[1], sd = petalenghtSigm[1], log = FALSE))
  #p[2]<-log2((1/(sqrt(2*pi)*petalwidthSigm[2]))*exp(-((point[4]-petalwidth[2])^2)/2*petalwidthSigm[2]^2))
  #p[2]<-log2(P_apriori) + p[2] + log2((1/(sqrt(2*pi)*petalenghtSigm[2]))*exp(-((point[3]-petalenght[2])^2)/2*petalenghtSigm[2]^2))
  p[2]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[2], sd = petalwidthSigm[2], log = FALSE))+log2(dnorm(point[3], mean = petalenght[2], sd = petalenghtSigm[2], log = FALSE))
  p[3]<-log2(P_apriori) +log2(dnorm(point[4], mean = petalwidth[3], sd = petalwidthSigm[3], log = FALSE))+log2(dnorm(point[3], mean = petalenght[3], sd = petalenghtSigm[3], log = FALSE))
  #p[3]<-log2((1/(sqrt(2*pi)*petalwidthSigm[3]))*exp(-((point[4]-petalwidth[3])^2)/2*petalwidthSigm[3]^2))
  #p[3]<-log2(P_apriori)+p[3] + log2((1/(sqrt(2*pi)*petalenghtSigm[3]))*exp(-((point[3]-petalenght[3])^2)/2*petalenghtSigm[3]^2))
    
  print(log2(dnorm(point[4], mean = petalwidth[1], sd = petalwidthSigm[1], log = FALSE)))
  #print(p[2])
  #print(p[3])
  #print(max(p))
  #print(match(max(p), p))
  print(022)
  #print(p)
  #print(answer)
  return(match(max(p), p))
}



col3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1)
col4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.1)

for(i in col3) {
  for(j in col4) {
    point <- c(0, 0, i, j)
    points(point[3], point[4],  pch = 21, bg = "white", col = colors[naiveBayesianClassifier(point)])
  }
}
points(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])

legend("bottomright", c("virginica", "versicolor", "setosa"), pch = c(15,15,15), col = c("blue", "green3", "red"))
