
euclideanDistance <- function(a, b)
{
  sqrt(sum((a - b)^2))
}

KNN <- function(yl , point_to_classify, k , metric = euclideanDistance){
  distances <- c()
  for(i in 1:nrow(yl)){
    distances[i] <- metric(yl[i , 1:length(yl) - 1] , point_to_classify)
  }
  yl <- cbind(yl , distances)
  ordered_dist_array <- yl[order(distances),]
  k_arr <- ordered_dist_array[1:k , 3]
  class_iris <- table(k_arr) 
  return(names(which.max(class_iris)))
}



point_to_classify <- c(1, 4)
x_sign <- iris[ ,3]
y_sign <- iris[ ,4]
class_of_sign <- iris[ ,5]
test_table_class <- data.frame(x_sign , y_sign , class_of_sign)
k <- 7

to_classify <- KNN(test_table_class , point_to_classify, k)
