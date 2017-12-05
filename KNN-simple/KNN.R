
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

plot(NULL, NULL, type = "l", xlim = c(min(iris[, 3]), max(iris[, 3])), ylim = c(min(iris[, 4]), max(iris[, 4])), xlab = 'Petal.Length', ylab = 'Petal.Width')

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
col3 <- seq(from = min(iris[, 3]), to = max(iris[, 3]), by = 0.1) 
col4 <- seq(from = min(iris[, 4]), to = max(iris[, 4]), by = 0.05) 
for(i in col3){
  for(l in col4){
    z <- c(i, l)
    xl <- iris[, 3:5]
    class <- KNN(xl, z, k)
    points(z[1], z[2], pch = 21, bg = "white", col = colors[class])
  }
};

points(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
       = colors[iris$Species], asp = 1)
