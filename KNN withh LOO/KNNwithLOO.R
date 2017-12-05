

euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

KNN <- function(xl , point_to_classify, k , metric = euclideanDistance){
  distances <- c()
  for(i in 1:nrow(xl)){
    distances[i] <- metric(xl[i , 1:length(xl) - 1] , point_to_classify)
  }
  xl <- cbind(xl , distances)
  ordered_dist_array <- xl[order(distances),]
  k_arr <- ordered_dist_array[1:k , 3]
  class_iris <- table(k_arr) 
  return(names(which.max(class_iris)))
}

LOO <- function(test_table_class , k){
  res_sum <- 0
  
  for(i in 1:nrow(test_table_class)){
    test_point <- c(test_table_class[i , 1] , test_table_class[i , 2])
    test_data <- test_table_class[-i, ,]
    value <- KNN(test_data , test_point , k)
    if(test_table_class[i , 3] != value){
      res_sum <- res_sum + 1
    }
    
  }
  return(res_sum / nrow(test_table_class))
}
plot(NULL, NULL, type = "l", xlim = c(0, 20), ylim = c(0, 0.2), xlab = 'k', ylab = 'LOO')

x_sign <- iris[ ,3]
y_sign <- iris[ ,4]
class_of_sign <- iris[ ,5]
test_table_class <- data.frame(x_sign , y_sign , class_of_sign)

x <- c()
y <- c()

for(k in 1:20)
{
  x <- c(x, k)
  y <- c(y, LOO(test_table_class, k))
  points(k,LOO(test_table_class, k), pch = 21, bg = "black", asp = 1)

}
}
