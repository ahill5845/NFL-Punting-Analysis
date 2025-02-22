setwd("C:\\Users\\hilla\\OneDrive\\Documents\\418 Sports Analytics Project")
pacman::p_load(tidyverse, DataExplorer, ipred, pROC, rpart, caret, ROCR, magrittr, gbm, xgboost)
df <- readRDS("cleanedData.RDS")
df %<>% drop_na(target) 
df %<>% select(.,-playResult)
set.seed(13)
trainIndex <- createDataPartition(df$target, p=0.8, list=FALSE, times=1)
training <- df[trainIndex,]
valid <- df[-trainIndex,]

# RF Comparison Tables ----------------------------------------------------
rf1 <- readRDS("rf1.RDS")
p.rf.t <- predict(rf1, data=train, type="prob")
rt <- roc(training$target, p.rf.t[,2])
r.rf.auc.t<-rt$auc
p.rf<-predict(rf1, newdata = valid, type="prob")
r<-roc(valid$target,  p.rf[,2])
r.rf.auc<-r$auc

rf2 <- readRDS("rf2.RDS")
p.rf.t.2 <- predict(rf2, data=training, type="prob")
rt.2 <- roc(training$target, p.rf.t.2[,2])
r.rf.auc.t.2<-rt.2$auc
p.rf.2 <- predict(rf2, newdata = valid, type="prob")
r.2 <- roc(valid$target, p.rf.2[,2])
r.rf.auc.2<-r.2$auc

library(kableExtra)
rfResults <- data.frame(Model = c("RF Model 1", "RF Model 2"),
                        AUC.Train = c(r.rf.auc.t, r.rf.auc.t.2),
                        AUC.Valid = c(r.rf.auc, r.rf.auc.2),
                        mtry = c("2, 5, 10", "2, 5, 10, 12"),
                        min.node.size = c("150,200,250", "10,20"),
                        Num.Trees = c(500, 500))
knitr::kable(rfResults) %>% kable_styling(full_width = F)

# Boosted Tree Comparison Tables ------------------------------------------
gb1 <- readRDS("gb1.RDS")
p.gb1.t<-predict(gb1, data=training, type="prob")
rt.gb1<-roc(training$target, p.gb1.t[,2])
r.gb.auc.t1<-rt.gb1$auc
p.gb1<-predict(gb1, newdata=valid, type="prob")
rgb1<-roc(valid$target, p.gb1[,2])
r.gb.auc1<-rgb1$auc

gb2 <- readRDS("gb2.RDS")
p.gb2.t<-predict(gb2, data=training, type="prob")
rt.gb2<-roc(training$target, p.gb2.t[,2])
r.gb.auc.t2<-rt.gb2$auc
p.gb2<-predict(gb2, newdata=valid, type="prob")
rgb2<-roc(valid$target, p.gb2[,2])
r.gb.auc2<-rgb2$auc

xg1 <- readRDS("xgb1.RDS")
p.xg1.t<-predict(xg1, newdata=training, type="prob")
rt.xg1<-roc(training$target, p.xg1.t[,2])
r.xg.auc.t1<-rt.xg1$auc
p.xg1<-predict(xg1, newdata=valid, type="prob")
r.xg1<-roc(valid$target, p.xg1[,2])
r.xg.auc1<-r.xg1$auc

xg2 <- readRDS("xgb2.RDS")
p.xg2.t<-predict(xg2, newdata=training, type="prob")
rt.xg2<-roc(training$target, p.xg2.t[,2])
r.xg.auc.t2<-rt.xg2$auc
p.xg2<-predict(xg2, newdata=valid, type="prob")
r.xg2<-roc(valid$target, p.xg2[,2])
r.xg.auc2<-r.xg2$auc

treeResults <- data.frame(Model = c("GB Model 1", "GB Model 2", "XgBoost Model 1","XgBoost Model 2"),
                         AUC.Train = c(r.gb.auc.t1, r.gb.auc.t2, r.xg.auc.t1, r.xg.auc.t2),
                         AUC.Valid = c(r.gb.auc1, r.gb.auc2, r.xg.auc1, r.xg.auc2),
                         Complexity = c("N.Trees=150","N.Trees=500", "depth=(3, 6)","depth=(10, 20)"))
knitr::kable(treeResults) %>% kable_styling(full_width = F)

# NN, Best RF, and Best BT Comparison -------------------------------------
nnetFit1 <- readRDS("nnrf.RDS")
p.nnet<-predict(nnetFit1, data=training, type="prob")
r<-roc(training$target,  p.nnet[,2])
r.nnet.auct<-r$auc
p.nnet<-predict(nnetFit1, newdata=valid, type="prob")
r<-roc(valid$target, p.nnet[,2])
r.nnet.auc<-r$auc

bestResults <- data.frame(Model = c("NN RF Model", "RF Model 1", "XgBoost Model 1", "GB Model 1"),
                          AUC.Train = c(r.nnet.auct, r.rf.auc.t, r.xg.auc.t1, r.gb.auc.t1),
                          AUC.Valid = c(r.nnet.auc, r.rf.auc, r.xg.auc1, r.gb.auc2),
                          Complexity = c("Weights=500", "N.Trees=500", "depth=(3, 6)", "N.Trees=150"))
knitr::kable(bestResults) %>% kable_styling(full_width = F)

# ROC Curve ---------------------------------------------------------------
# list of validation data ROC
r<-roc(valid$target,  p.rf[,2]) #random forest
rgb1<-roc(valid$target, p.gb1[,2]) #gb tree 
r.xg1<-roc(valid$target, p.xg1[,2]) #xg boost
r.nn<-roc(valid$target, p.nnet[,2]) #nn model
roc <- list("RF" = r, "GB.Tree" = rgb1, "XgBoost" = r.xg1, "RF NN" = r.nn)
p <- ggroc(roc)
p + scale_color_discrete(name="Model") + theme_bw() + xlab("True Negative Rate") + ylab("True Positive Rate")

# Lift Chart --------------------------------------------------------------
# predictions from the validation data
result <- data.frame(valid$target, p.rf[,2], p.gb1[,2], p.xg1[,2], p.nnet[,2])
pred <- prediction(result$p.rf...2., result$valid.target)
pred2 <- prediction(result$p.gb1...2., result$valid.target)
pred3 <- prediction(result$p.xg1...2., result$valid.target)
pred4 <- prediction(result$p.nnet...2., result$valid.target)
lift <- performance(pred, "lift", "rpp")
lift2 <- performance(pred2, "lift", "rpp")
lift3 <- performance(pred3, "lift", "rpp")
lift4 <- performance(pred4, "lift", "rpp")
plot(lift,main="Lift chart", col="brown1", ylim=c(0,2.5), xlim=c(0,0.3))
plot(lift2, col="blue", add=T)
plot(lift3, col="red", add=T)
plot(lift4, col="chartreuse", add=T)
legend(0.03, 1, legend = c("Random Forest", "Gradient Boosted", "XgBoost", "RF Neural Network"),
       col = c("brown1", "blue", "red", "chartreuse"), lty=1, cex=0.8)


# Lift Analysis -----------------------------------------------------------
dfLift<-data.frame(tp=unlist(pred4@tp)[1:20], 
                   pos.pred=unlist(pred4@n.pos.pred)[1:20], 
                   cuts=unlist(pred4@cutoffs)[1:20])
head(dfLift, 10)
numerator <- dfLift$tp[2] / dfLift$pos.pred[2]
numerator
tot1 <- table(valid$target)
names(tot1) <- c("Bad", "Good")
tot1[2]
totprop1 <- prop.table(tot1)
totprop1[2]
valid.lift <- numerator / totprop1[2]
valid.lift
# Cutoff Analysis ---------------------------------------------------------
# neural network model is the best 
dfc <- data.frame(lift4@y.values, lift4@alpha.values)
colnames(dfc) <- c("lift", "cutoff")
coords(r.nn, "best", ret="threshold") #0.4139722
result$Cut<-as.factor(ifelse(result$p.nnet...2.>0.4139722, "Good", "Bad"))
confusionMatrix(data = result$Cut, reference = result$valid.target, positive = "Good")




