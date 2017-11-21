METRIC <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

GAUSS_CORE <- function(dist_param) {
  return(((2*pi)^(-0.5))*exp(-0.5*(dist_param^2)))
}

PARZEN_WINDOW_VAR <- function(xl , point_to_classify , k){
  distances <- c()
  for(i in 1:nrow(xl)){
    distances[i] <- METRIC(xl[i , 1:length(xl) - 1] , point_to_classify)
  }
  xl <- cbind(xl , distances)
  ordered_dist_arr <- xl[order(distances),]
  
  dist_thr_core <- c()
  
  for(i in 1:k){
    dist_thr_core[i] <- GAUSS_CORE(ordered_dist_arr[i , 4] / ordered_dist_arr[k + 1 , 4])
    
  }
  
  dist_class_core <- data.frame(first_k <- ordered_dist_arr$class_of_sign[1:k] , dist_thr_core[1:k])
  wght_max <- c( sum_setosa <- sum(dist_class_core[dist_class_core$first_k == "setosa" , 2]),
                 sum_versicolor <- sum(dist_class_core[dist_class_core$first_k == "versicolor" , 2]),
                 sum_virginica <- sum(dist_class_core[dist_class_core$first_k == "virginica" , 2]) )
  
  return(levels(xl$class_of_sign)[FIND_MAX_ID(wght_max)])
  
}

x_sign <- iris[ ,3]
y_sign <- iris[ ,4]
class_of_sign <- iris[ ,5]
test_table_class <- data.frame(x_sign , y_sign , class_of_sign)
point_to_classify <- c(1, 4) 
h <- 0.4
k <- 2
to_classify <- PARZEN_WINDOW_VAR(test_table_class, point_to_classify,k)
