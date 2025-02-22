setwd("C:\\Users\\hilla\\OneDrive\\Documents\\418 Sports Analytics Project")
pacman::p_load(tidyverse, DataExplorer, ipred, pROC, rpart, caret, ROCR, magrittr, gbm, xgboost, doParallel)
df <- readRDS("cleanedData.RDS")
df %<>% drop_na(target) 
df %<>% select(.,-playResult, -season) 

# Split into training and test data ---------------------------------------
set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]

# Create additional indexes and controls ----------------------------------
cvindx <- createFolds(trainIndex, k=10, returnTrain = TRUE)
ctrl <- trainControl(method="cv", index=cvindx, summaryFunction = twoClassSummary, classProbs = TRUE)
y <- training$target
x <- select(training, -target)

# Random Forest Model 1 ---------------------------------------------------
tunegrid <- expand.grid(.mtry = c(2, 5, 10), .splitrule = "gini", .min.node.size = c(150,200,250))
c1 <- makePSOCKcluster(7)
registerDoParallel(c1)
rforest1 <- train(x=x, y=y, method="ranger", tuneGrid=tunegrid, metric="ROC",
                  num.trees=500, importance="impurity", trControl=ctrl )
stopCluster(c1)
saveRDS(rforest1, "rf1.RDS")
# Random Forest Model 2 ---------------------------------------------------
tunegrid2 <- expand.grid(.mtry = c(2, 5, 10, 12), .splitrule = "gini", .min.node.size = c(10, 20))
c1 <- makePSOCKcluster(7)
registerDoParallel(c1)
rforest2 <- train(x=x, y=y, method="ranger", tuneGrid=tunegrid2, metric="ROC",
                  num.trees=500, importance="impurity", trControl=ctrl )
stopCluster(c1)
saveRDS(rforest2, "rf2.RDS")
# Gradient Boosted Tree Model 1 -------------------------------------------
GBMgrid1 <- expand.grid(n.trees = seq(50,150,50), interaction.depth = c(10, 20),shrinkage = c(0.1, 0.01), 
                        n.minobsinnode=c(25))
c1 <- makePSOCKcluster(7)
registerDoParallel(c1)
gb.tree <- train(target~., data=training, method = 'gbm', trControl=ctrl, 
                 tuneGrid=GBMgrid1, metric='ROC')
stopCluster(c1)
saveRDS(gb.tree, "gb1.RDS")

# Gradient Boosted Tree Model 2 -------------------------------------------
GBMgrid2 <- expand.grid(n.trees = seq(50,500,50), interaction.depth = c(10, 20),
                        shrinkage = c(0.1, 0.01), n.minobsinnode=c(25))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
gb.tree2 <- train(target~., data=training, method = 'gbm', trControl=ctrl, 
                  tuneGrid=GBMgrid2, metric='ROC')
stopCluster(cl)
saveRDS(gb.tree2, "gb2.RDS")
# XgBoost Model 1 ---------------------------------------------------------
XGgrid1 <-  expand.grid(nrounds = 100, max_depth = c(3,6), eta = c(0.001,0.1), gamma = c(0.5, 0.75),
                        colsample_bytree = c(0.3, 1), min_child_weight = c(0,15), subsample = c(0.3,1))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
xgboost <- train(target~., data = training, method = "xgbTree", trControl=ctrl, 
                 tuneGrid=XGgrid1, metric='ROC')
stopCluster(cl)
saveRDS(xgboost, "xgb1.RDS")
# XgBoost Model 2 ---------------------------------------------------------
XGgrid2 <-  expand.grid(nrounds = 100, max_depth = c(10,20), eta = c(0.001,0.1), gamma = c(0.5, 0.75),
                        colsample_bytree = c(0.3, 1), min_child_weight = c(0,15), subsample = c(0.3,1))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
xgboost2 <- train(target~., data = training, method = "xgbTree", trControl=ctrl, 
                  tuneGrid=XGgrid2, metric='ROC')
stopCluster(cl)
saveRDS(xgboost2, "xgb2.RDS")
# Neural Network with RF Terms --------------------------------------------
library(randomForest)
varI <- varImp(rforest1)
var <- as.data.frame(varI$importance)
var <- cbind(rownames(var), data.frame(var, row.names=NULL))
var <- var[order(-var$Overall), ]
terms.rf <- var$`rownames(var)`[1:37, drop=TRUE]
library(nnet)
tunegrid <- expand.grid(.size=1:10, .decay= c(0, 0.1, 0.5))
maxSize <- max(tunegrid$.size)
numWts <- 500
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
nnetFit.rf <- train(x=x, y=training$target, method="nnet", metric="ROC", linout=FALSE, preProcess = c("range"), 
                    tuneGrid = tunegrid, trace=FALSE, maxit=100, MaxNWts=numWts, trControl=ctrl)
stopCluster(cl) 
saveRDS(nnetFit.rf, "nnrf.RDS")


















