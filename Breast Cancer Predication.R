library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(pROC)
library(MASS)
library(caTools)
library(caret)
library(caretEnsemble)
library(doMC)
library(partykit)
str(data)
data$diagnosis <- as.factor(data$diagnosis)
summary(data)
prop.table(table(data$diagnosis))
data[,33] <- NULL
corr_mat <- cor(data[,3:ncol(data)])
corrplot(corr_mat, order = "hclust", tl.cex = 0.5, addrect = 8)
set.seed(123)
split = sample.split(data$diagnosis, SplitRatio = 0.8)
train_data <- subset(data,split == TRUE)
test_data <- subset(data,split == FALSE)
pca_res <- prcomp(data[,3:ncol(data)], center = TRUE, scale = TRUE)
plot(pca_res, type="l")
summary(pca_res)
pca_df <- as.data.frame(pca_res$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=data$diagnosis)) + geom_point(alpha=0.5)
g_pc1 <- ggplot(pca_df, aes(x=PC1, fill=data$diagnosis)) + geom_density(alpha=0.25)  
g_pc2 <- ggplot(pca_df, aes(x=PC2, fill=data$diagnosis)) + geom_density(alpha=0.25)  
grid.arrange(g_pc1, g_pc2, ncol=2)
fitControl <- trainControl(method="cv",
                           number = 5,
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

model_rf <- train(diagnosis~.,
                  train_data,
                  method="ranger",
                  metric="ROC",
                  #tuneLength=10,
                  #tuneGrid = expand.grid(mtry = c(2, 3, 6)),
                  preProcess = c('center', 'scale'),
                  trControl=fitControl)
pred_rf <- predict(model_rf, test_data)
cm_rf <- confusionMatrix(pred_rf, test_data$diagnosis, positive = "M")
cm_rf

model_knn <- train(diagnosis~.,
                   train_data,
                   method="knn",
                   metric="ROC",
                   preProcess = c('center', 'scale'),
                   tuneLength=10,
                   trControl=fitControl)
pred_knn <- predict(model_knn, test_data)
cm_knn <- confusionMatrix(pred_knn, test_data$diagnosis, positive = "M")
cm_knn
pred_prob_knn <- predict(model_knn, test_data, type="prob")
roc_knn <- roc(test_data$diagnosis, pred_prob_knn$M)
plot(roc_knn)

model_nnet <- train(diagnosis~.,
                    train_data,
                    method="nnet",
                    metric="ROC",
                    preProcess=c('center', 'scale'),
                    trace=FALSE,
                    tuneLength=10,
                    trControl=fitControl)
pred_nnet <- predict(model_nnet, test_data)
cm_nnet <- confusionMatrix(pred_nnet, test_data$diagnosis, positive = "M")
cm_nnet

model_list <- list(RF=model_rf, 
                   NNET=model_nnet, 
                   KNN = model_knn)
resamples <- resamples(model_list)

model_cor <- modelCor(resamples)
corrplot(model_cor)
bwplot(resamples, metric="ROC")
cm_list <- list(RF=cm_rf,
                NNET=cm_nnet, 
                KNN = cm_knn)
cm_list_results <- sapply(cm_list, function(x) x$byClass)
cm_list_results

data$diagnosis <- as.character.factor(data$diagnosis)
data <- within(data, diagnosis[diagnosis == 'M'] <- 'Malignant')
data <- within(data, diagnosis[diagnosis == 'B'] <- 'Benign')
ggplot(data, aes(x=diagnosis, fill = diagnosis))+ 
  theme_bw()+ geom_bar()+
  labs(x = "Diagnosis", y = "Data Count", title="Breast Cancer Data")

x <- ctree(diagnosis~., data=train_data)
plot(x, type = "simple")

