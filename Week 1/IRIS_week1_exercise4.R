data("iris")
attach(iris)


######## Part 1 #########

## Linear regression between column 1 & 3
fit.13 <- lm(Sepal.Length ~ Petal.Length, data=iris)
summary(fit.13)   # R-squared:   0.76


## Linear regression between column 1 & 2
fit.12 <- lm(Sepal.Length ~ Sepal.Width, data=iris)
summary(fit.12)   # R-squared:    0.01382


## Linear regression between column 1 & 4
fit.14 <- lm(Sepal.Length ~ Petal.Width, data=iris)
summary(fit.14)   # R-squared:   0.669


###### Part 2 ######

#1.  transform to a binary problem with classes 1 and 2 only

binary.iris <- iris[!(Species=="virginica"),] 
binary.iris$Species <- ifelse(binary.iris$Species=="setosa",1,2)



#Spliting the data set to training and testing sets

set.seed(123)
training.samples <- sample(1:nrow(binary.iris), nrow(binary.iris)*0.6, replace = F)

binary_iristrain  <- binary.iris[training.samples, ]
binary_iristest <- binary.iris[-training.samples, ]

#### (i) Without normalization ####

 # fixing the data as matrices
x <- as.matrix(binary_iristrain[,1:4])
X <- cbind(rep(1,nrow(x)), x)
y <- as.matrix(binary_iristrain$Species)
m <- length(y)
theta<-rep(0,5)


# cost function

cost<-function(X, y, theta){
  MSE <- sum((X%*%theta- y)^2)/(2*m)
  return(MSE)
}

# gradient decent
graddec<-function(X, y, theta, alpha, iters){
  MSE_store <- rep(0, iters)
  for(i in 1:iters){
    
    theta <- theta - alpha*(1/m)*(t(X)%*%(X%*%theta - y))
    
    MSE_store[i]  <- cost(X, y, theta)
  }
  
  results<-list(theta, MSE_store)
  return(results)
}

alpha <- .001
iters <- 1000
results <- graddec(X, y, theta, alpha, iters)
theta <- results[[1]]
cost_hist <- results[[2]]
print(theta)

#               [,1]
#              0.021093121
# Sepal.Length 0.136962483
# Sepal.Width  0.000115254
# Petal.Length 0.225650930
# Petal.Width  0.086051863


# plot of the cost
plot(1:iters, cost_hist, type = 'l')

#### (ii) With normalized data ####


#normalization function
norm.function <- function(feature) {
  (feature - min(feature)) / (max(feature) - min(feature))
}

iris.normal <- as.data.frame (lapply(binary_iristrain[1:4], norm.function ))
iris.normal$Species <- binary_iristrain$Species
head(iris.normal)

x.norm <- as.matrix(iris.normal[,1:4])
X.norm <- cbind(rep(1,nrow(x.norm)), x.norm)
y.norm <- as.matrix(iris.normal$Species)
m.norm <- length(y.norm)
theta.norm <- rep(0,5)

m <- m.norm
alpha <- .001
iters <- 1000


# cost function

cost.norm<-function(X.norm, y.norm, theta.norm){
  MSE.norm <- sum((X.norm%*%theta.norm- y.norm)^2)/(2*m.norm)
  return(MSE.norm)
}

# gradient decent
graddec.norm<-function(X.norm, y.norm, theta.norm, alpha, iters){
  MSE_store.norm <- rep(0, iters)
  for(i in 1:iters){
    
    theta.norm <- theta.norm - alpha*(1/m.norm)*(t(X.norm)%*%(X.norm%*%theta.norm - y.norm))
    
    MSE_store.norm[i]  <- cost.norm(X.norm, y.norm, theta.norm)
  }
  
  results.norm<-list(theta.norm, MSE_store.norm)
  return(results.norm)
}

results.norm <- graddec.norm(X.norm, y.norm, theta.norm, alpha, iters)
theta.norm <- results.norm[[1]]
cost_hist.norm <- results.norm[[2]]
print(theta.norm)
# 
#                 [,1]
#              0.6744380
# Sepal.Length 0.3435897
# Sepal.Width  0.2346614
# Petal.Length 0.4148836
# Petal.Width  0.3803888

# plot of the cost
plot(1:iters, cost_hist.norm, type = 'l')

