euclideanDistance <- function(a, b)
{
  sqrt(sum((a - b)^2))
}


FIND_MAX_ID <- function(arr_max){
  id <- 0
  max <- arr_max[1]
  for(i in 1:length(arr_max)){
    if(arr_max[i] >= max){
      id <- i
    }
  }
  return(id)
}


KNN <- function(xl , point_to_classify , k , q, metric = euclideanDistance){
  res <- 0
  distances <- c()
  for(i in 1:nrow(xl)){
    distances[i] <- metric(xl[i , 1:length(xl) - 1] , point_to_classify)
  }
  xl <- cbind(xl , distances)
  ordered_dist_arr <- xl[order(distances),]
  k_arr <- ordered_dist_arr[1:k , 3]
  class_iris <- table(k_arr) 
  if(q != 0){
    weight_k <- c()
    for(i in 1:k){
      weight_k[i] <- q^i
      
    }
    
    xl_ordered_w <- data.frame(k_arr , weight_k)
    xl_ordered_w <- xl_ordered_w[1:k ,]
    wght_max <- c( sum_setosa <- sum(xl_ordered_w[xl_ordered_w$k_arr == "setosa" , 2]), 
                   sum_versicolor <- sum(xl_ordered_w[xl_ordered_w$k_arr == "versicolor" , 2]),
                   sum_virginica <- sum(xl_ordered_w[xl_ordered_w$k_arr == "virginica" , 2]) )
    
    
    res <- levels(xl$class_of_sign)[FIND_MAX_ID(wght_max)]
    
  }else{
    
    res <- names(which.max(class_iris))
  }
  
  return(res)
}


point_to_classify <- c(1, 4)
x_sign <- iris[ ,3]
y_sign <- iris[ ,4]
class_of_sign <- iris[ ,5]
test_table_class <- data.frame(x_sign , y_sign , class_of_sign)
k <- 7
q <- 0.1

to_classify <- KNN(test_table_class , point_to_classify, k,q)
