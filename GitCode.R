# Library Import ----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(xlsx)
library(readxl)
library(XLConnect)
library(ggpubr)
library(caret)
library(dplyr)        
library(pROC)
library(ROCR)
library(factoextra)
library(tictoc)


# Data Import ----------------------------------------------------------------

tic()


df_final <- read_excel("df_combined_2.xlsx", sheet=1)

df_ground_truth <- df_final %>%
  mutate(
    Tumor = case_when(
      Gleason_1 == 3 & Gleason_1 == 3 ~ "0",
      TRUE ~ "1"
    )
  ) %>%
  mutate(
    PIRADS_grading = case_when(
      PIRADS == 3 ~ "0",
      TRUE ~ "1"
    )
  ) 

col_vector_factor <- c(1, 12:14, 17, 18)
df_ground_truth[, col_vector_factor] <- lapply(df_ground_truth[, col_vector_factor], as.factor)

df_ground_truth <- df_ground_truth %>%
  select(PIRADS, Gleason_1:PIRADS_grading, T2_Normal:Ve)


# First Data Visualization ------------------------------------------------

#Data Vis Gleason

(e <- df_ground_truth %>%
  select(PIRADS, Gleason_1:Gleason_sum, T2, ADC, Ktrans, Kep, Ve)%>%
  mutate(Gleason_sum=fct_relevel(Gleason_sum, "6", "7", "8", "9"))%>%
  gather("Parameter", "Value", -c(PIRADS, Gleason_1, Gleason_2, Gleason_sum))%>%
  ggplot(aes(x=Gleason_sum, y=Value))+
  geom_boxplot()+
  facet_wrap(~Parameter, scales = "free_y")+
  xlab("Gleason Sum"))+
  theme_minimal()


#Data Vis PIRADS

(f <- df_ground_truth %>%
  select(PIRADS, Gleason_1:Gleason_sum, T2, ADC, Ktrans, Kep, Ve)%>%
  mutate(Gleason_sum=fct_relevel(Gleason_sum, "6", "7", "8", "9"))%>%
  gather("Parameter", "Value", -c(PIRADS, Gleason_1, Gleason_2, Gleason_sum))%>%
  ggplot(aes(x=PIRADS, y=Value))+
  geom_boxplot()+
  facet_wrap(~Parameter, scales = "free_y")+
  xlab("PIRADS v2 assessment score"))+
  theme_minimal()


# Training/Testing Split --------------------------------------------------

df_analyis <- df_ground_truth %>%
  select(Tumor, T2, ADC, Ktrans, Kep, Ve, 
         PIRADS, PIRADS_grading, Gleason_1, Gleason_2, Gleason_sum)
  

n <- nrow(df_analyis)
n_train <- round(0.80 * n) 

set.seed(123)
train_indices <- sample(1:n, n_train)

(prostate_train <- df_analyis[train_indices, ])  

(prostate_test <- df_analyis[-train_indices, ])

trainX <- prostate_train[, -c(1,7:11)]
testX <- prostate_test[, -c(1,7:11)]
prostate_train$Tumor <- ifelse(prostate_train$Tumor == 1, "Yes", "No")
prostate_train$Tumor <- as.factor(prostate_train$Tumor)
prostate_test$Tumor <- ifelse(prostate_test$Tumor == 1, "Yes", "No")
prostate_test$Tumor <- as.factor(prostate_test$Tumor)

glimpse(prostate_train)
glimpse(prostate_test)
glimpse(trainX)
glimpse(testX)



# Training GBM Model ------------------------------------------------------


myControl <- trainControl(method = "repeatedcv", number = 5,repeats = 5, summaryFunction = twoClassSummary, 
                          classProbs = TRUE, search="grid")

man_grid_gbm <- expand.grid(interaction.depth = c(1,2,3,4,5), n.trees = seq(1,350,50), 
                            shrinkage = c(0.1,0.01), n.minobsinnode = seq(1, 10, 1))

set.seed(123)
model_gbm <- train(x=trainX, y=prostate_train$Tumor, method = "gbm", metric="ROC", 
                   trControl = myControl, tuneGrid = man_grid_gbm)
summary(model_gbm)
model_gbm
(a <- plot(model_gbm))
model_gbm$modelType

pred_gbm_train <- predict(model_gbm, trainX)
(cm_gbm_train <- confusionMatrix(pred_gbm_train, prostate_train$Tumor))

probs_gbm_train <- predict(model_gbm, trainX, type = "prob")
ROC_gbm_train <- roc(predictor=probs_gbm_train$Yes, response = prostate_train$Tumor)
ROC_gbm_train$auc

pred_gbm <- predict(model_gbm, testX)
(cm_gbm <- confusionMatrix(pred_gbm, prostate_test$Tumor))
pred_gbm
probs_gbm <- predict(model_gbm, testX, type = "prob")
probs_gbm
ROC_gbm <- roc(predictor=probs_gbm$Yes, response = prostate_test$Tumor)
ROC_gbm$auc



# Training Neural Network -------------------------------------------------


man_grid_nnet <- expand.grid(size=seq(1,10,1), decay=c(0,0.1,0.01,0.001,0.0001,0.00001))

model_nnet <- train(x=trainX, y=prostate_train$Tumor, method = "nnet", metric="ROC", trControl = myControl, tuneGrid = man_grid_nnet)
summary(model_nnet)
model_nnet
(b <- plot(model_nnet))

pred_nnet_train <- predict(model_nnet, trainX)
(cm_nnet_train <- confusionMatrix(pred_nnet_train, prostate_train$Tumor))
probs_nnet_train <- predict(model_nnet, trainX, type = "prob")
ROC_nnet_train <- roc(predictor=probs_nnet_train$Yes, response = prostate_train$Tumor)
ROC_nnet_train$auc

pred_nnet <- predict(model_nnet, testX)
(cm_nnet <- confusionMatrix(pred_nnet, prostate_test$Tumor))
pred_nnet
probs_nnet <- predict(model_nnet, testX, type = "prob")
probs_nnet
ROC_nnet <- roc(predictor=probs_nnet$Yes, response = prostate_test$Tumor)
ROC_nnet$auc




# Training Random Forest --------------------------------------------------


man_grid_rf <- expand.grid(mtry=c(1:5), splitrule = c("gini", "splitrule"), 
                           min.node.size = seq(1,5,1))

model_rf <- train(x=trainX, y=prostate_train$Tumor, method = "ranger", metric="ROC", 
                  trControl = myControl, tuneGrid = man_grid_rf)
summary(model_rf)
model_rf
(c <- plot(model_rf))


pred_rf_train <- predict(model_rf, trainX)
(cm_rf_train <- confusionMatrix(pred_rf_train, prostate_train$Tumor))
probs_rf_train <- predict(model_rf, trainX, type = "prob")
ROC_rf_train <- roc(predictor=probs_rf_train$Yes, response = prostate_train$Tumor)
ROC_rf_train$auc


pred_rf <- predict(model_rf, testX)
(cm_rf <- confusionMatrix(pred_rf, prostate_test$Tumor))
pred_rf
probs_rf <- predict(model_rf, testX, type = "prob")
probs_rf
ROC_rf <- roc(predictor=probs_rf$Yes, response = prostate_test$Tumor)
ROC_rf$auc


# Training Support Vector Machines with RBF -------------------------------

man_grid_svm <- expand.grid(C=seq(0.1,1,0.1), sigma=seq(0,1,0.1))

model_svm <- train(x=trainX, y=prostate_train$Tumor, method = "svmRadial", metric="ROC", 
                   trControl = myControl, tuneGrid = man_grid_svm)
summary(model_svm)
model_svm
(d <- plot(model_svm))


pred_svm_train <- predict(model_svm, trainX)
(cm_svm_train <- confusionMatrix(pred_svm_train, prostate_train$Tumor))
probs_svm_train <- predict(model_svm, trainX, type = "prob")
(ROC_svm_train <- roc(predictor=probs_svm_train$Yes, response = prostate_train$Tumor))
ROC_svm_train$auc

pred_svm <- predict(model_svm, testX)
(cm_svm <- confusionMatrix(pred_svm, prostate_test$Tumor))
pred_svm
probs_svm <- predict(model_svm, testX, type = "prob")
(probs_svm)
(ROC_svm <- roc(predictor=probs_svm$Yes, response = prostate_test$Tumor))
ROC_svm$auc


# Data Preparation for Comparison -----------------------------------------


prostate_test$PIRADS_grading <- ifelse(prostate_test$PIRADS_grading == 1, "Yes", "No")
prostate_test$PIRADS_grading <- as.factor(prostate_test$PIRADS_grading)
prostate_train$PIRADS_grading <- ifelse(prostate_train$PIRADS_grading == 1, "Yes", "No")
prostate_train$PIRADS_grading <- as.factor(prostate_train$PIRADS_grading)

roc_obj_PIRADS <- auc(as.numeric(prostate_test$Tumor), as.numeric(prostate_test$PIRADS_grading))
confusionMatrix(prostate_test$Tumor, prostate_test$PIRADS_grading)
(test <- auc(as.numeric(prostate_test$Tumor), as.numeric(prostate_test$PIRADS_grading)))

confusionMatrix(prostate_train$Tumor, prostate_train$PIRADS_grading)
(train <- auc(as.numeric(prostate_train$Tumor), as.numeric(prostate_train$PIRADS_grading)))

# AUC Calculation and Curve Visualization ---------------------------------


(actual <- as.numeric(prostate_test$Tumor))

list <- list(as.numeric(probs_gbm[,2]), as.numeric(probs_nnet[,2]), as.numeric(probs_rf[,2]),
             as.numeric(probs_svm[,2]), as.numeric(prostate_test$PIRADS_grading))

m <- length(list)
actuals_list <- rep(list(actual), m)

pred <- prediction(list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(rocs, col = as.list(1:m), main = "ROC Curve Anaylsis", lwd=3, lty=1)
legend(x = "bottomright",
       legend = c("GBM","NNet",
                  "Random Forest", "SVM with RBF",
                  "PIRADS Grading"),
       fill = 1:m)

sprintf("GBM: %.3f", ROC_gbm$auc)
sprintf("Neural Network: %.3f", ROC_nnet$auc)
sprintf("Random Forest: %.3f", ROC_rf$auc)
sprintf("Support Vector Machines with Radial Basis Function: %.3f", ROC_svm$auc)
sprintf("PIRADS Grading Testing: %.3f", test)

sprintf("GBM Traing: %.3f", ROC_gbm_train$auc)
sprintf("Neural Network Traing: %.3f", ROC_nnet_train$auc)
sprintf("Random Forest Traing: %.3f", ROC_rf_train$auc)
sprintf("Support Vector Machines with Radial Basis Function Traing: %.3f", ROC_svm_train$auc)
sprintf("PIRADS Grading Traing: %.3f", train)

toc()

# Statistical Testing two paired ROC Curves -------------------------------


set.seed(42)
roc.test(ROC_gbm, test, method = "delong", boot.n=2000)
roc.test(ROC_nnet, test, method = "delong", boot.n=2000)
roc.test(ROC_rf, test, method = "delong", boot.n=2000)
roc.test(ROC_svm, test, method = "delong", boot.n=2000)


set.seed(42)
roc.test(ROC_gbm_train, train, method = "delong", boot.n=2000)
roc.test(ROC_pcaNNet_train, train, method = "delong", boot.n=2000)
roc.test(ROC_rf_train, train, method = "delong", boot.n=2000)
roc.test(ROC_svm_train, train, method = "delong", boot.n=2000)


# Figure Hyperparameter Selection -----------------------------------------

ggarrange(a,b,c,d, labels = c("A", "B", "C", "D"))

ggarrange(e, f, labels = c("A", "B"))

