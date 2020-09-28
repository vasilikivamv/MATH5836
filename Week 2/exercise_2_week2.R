PimaIndiansDiabetes <- read.csv("D:/pima-indians-diabetes.csv", header = T)
library(glmnet)


#Spliting the data set to training and testing sets
set.seed(1234)
N <- nrow(PimaIndiansDiabetes)
train_id <- sample(1:nrow(PimaIndiansDiabetes), nrow(PimaIndiansDiabetes)*0.6, replace = F)


train.data <- PimaIndiansDiabetes[train_id,]
test.data <- PimaIndiansDiabetes[-train_id,]

#### Ridge regression ####

# predictors
x <- model.matrix(train.data$X1 ~., train.data)[,-1]
y <- train.data$X1

lambda=exp(seq(-6,6,0.2))
fit=glmnet(x,y,alpha=0,lambda=lambda,family="binomial")
plot(fit,xvar="lambda",label=T)

# choosing lambda through 10-fold cross validation
cv.fit=cv.glmnet(x,y,alpha=0,nfolds=10,lambda=lambda,family="binomial")
plot(cv.fit)

ii1=which(cv.fit$lambda==cv.fit$lambda.min)
ii2=which(cv.fit$lambda==cv.fit$lambda.1se)
cv.fit$lambda.min  ## 0.008229747

# MSE associated with the value of lambda chosen via CV

x.test <- model.matrix(test.data$X1~., test.data)[,-1]
# Convert the outcome (class) to a numerical variable
y.test <- test.data$X1 
ridge.pred <- predict(fit, s=cv.fit$lambda.min,newx=x.test)
mean((ridge.pred - y.test)^2)  # 3.539881

# compare with the logistic regression
train.data.fin <- data.frame(cbind(y,x))

glm.fit <- glm(y ~ .,family="binomial", data=train.data.fin)
summary(glm.fit)

test.data.fin <- data.frame(cbind(y=y.test,x.test))
glm.pred <- predict(glm.fit, newdata=test.data.fin)
mean((glm.pred - y.test)^2)  # 3.981528

# The MSE is reduced  with respect to Logistic regression

#### LASSO ####

fit.lasso=glmnet(x,y,alpha=1,lambda=lambda,family="binomial")
cv.fit.lasso=cv.glmnet(x,y,alpha=1,nfolds=10,lambda=lambda,family="binomial")
plot(fit.lasso,xvar="lambda",label=T)

plot(cv.fit.lasso)

# Now we can select the best lambda chosen via CV and compute the estimate of the test error

lasso.pred <- predict(fit.lasso,s=cv.fit.lasso$lambda.min,newx=x.test)
mean((lasso.pred - y.test)^2)  # 3.497737
#smaler MSE than the Ridge regression
