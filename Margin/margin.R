distance <- function(x1, x2)
{
  return(sqrt(sum((x1 - x2) ^ 2)))
}

w <- function(i,k=0,q=0) #âåñîâàÿ ô-öèÿ
{
  if(k>0) {
    (k+1-i)/k
  }
  else if(q>0&q<1){
    q^i
  }
  else return(-1)
}

printer <- function() {
  cat("draw in progress... ")
  plot(marginsArr, type="l",lwd=3,xlim =c(0,150), ylim = c(-2,2), xlab = "index (sorted)", ylab ="margin")
  rect(xleft = 13, xright = 140,ybottom = -3,ytop = 2.5,col = "light green",border = NA)
  rect(xleft = 140, xright = 150,ybottom = -3,ytop = 2.5,col = "green",border = NA)
  rect(xleft = 6, xright = 13,ybottom = -3,ytop = 2.5,col = "yellow",border = NA)
  rect(xleft = 0, xright = 6,ybottom = -3,ytop = 2.5,col = "red",border = NA)
  lines(marginsArr,lwd=2)
  cat("Done!")
}

margin <- function(inputData,i)
{
  l <- dim(inputData)[1]
  n <- dim(inputData)[2] - 1
  classesList <- unique(inputData[,n+1])
  counts <- rep(0,length(classesList))
  distances <- matrix(NA, l, 2)
  for (j in 1:l)
  {
    distances[j, ] <- c(j, distance(inputData[i, 1:n], inputData[j,1:n]))
  }
  orderedinputData <- inputData[order(distances[-i, 2]), ]
  for(j in  1:(l-1))
  {
    ind <- which(orderedinputData[j,n+1]==classesList)
    counts[ind] <- counts[ind]+w(j,k=l)
  }
  margY <- counts[which(inputData[i,n+1]==classesList)]
  margNY <- max(counts[-which(inputData[i,n+1]==classesList)]) 
  if(margY<margNY) return(margY-margNY)
  else return(margY/margNY)
}

marginsArr<-c()
for(k in 1:150){
  marginsArr[k]<-margin(iris,k)
}
marginsArr<-sort(marginsArr)
printer()
