PimaIndiansDiabetes <- read.csv("D:/pima-indians-diabetes.csv", header = T)
attach(PimaIndiansDiabetes)

########### Question 1 ###########

#Spliting the data set to training and testing sets
set.seed(1234)
N <- nrow(PimaIndiansDiabetes)
train_id <- sample(1:nrow(PimaIndiansDiabetes), nrow(PimaIndiansDiabetes)*0.6, replace = F)


train.set <- PimaIndiansDiabetes[train_id,]
test.set <- PimaIndiansDiabetes[-train_id,]

# Apply logistic regression

glm.fits=glm(X1~.,data=test.set,family=binomial)
summary(glm.fits)


predict.logistic <- predict(glm.fits, data= test.set, type= "response")
predict.logistic[1:10]

# predict if a patient has diabetes or not
# if the estimated probability is larger than 0.5, we predict that the patient has diabetes

glm.pred=rep(0,N)
glm.pred[predict.logistic>.5]=1
table(glm.pred,X1)

(369+84)/N
## [1] 0.5906128

mean(glm.pred==X1)
## [1] 0.5906128

# in this case, logistic regression classifies correctly 59% of the times
require("MLmetrics")
LogLoss(predict.logistic, test.set$X1)
## [1] 0.4459375
RMSE(predict.logistic, test.set$X1)
## [1] 0.3724953

########### Question 2 ###########

# Plot ROC curve and compute AUC
library(pROC)
roc_obj <- roc(test.set$X1, predict.logistic, plot = TRUE)
auc(roc_obj)

# Area under the curve: 0.8578

# Precision-Recall curve
library(precrec)

# F1 score
F1_Score(predict.logistic, test.set$X1, positive = 1)

library(caret)

# Define training control
set.seed(1234)
train.control <- trainControl(method = "cv", number = 10)

# Train the model
PimaIndiansDiabetes$X1 <- as.factor(PimaIndiansDiabetes$X1)
data_cv <- train(X1~., data = PimaIndiansDiabetes, method = "glm",
                trControl = train.control)

print(data_cv)
 
# Generalized Linear Model 
# 
# 767 samples
# 8 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 690, 690, 690, 691, 691, 690, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.7744703  0.4752576







