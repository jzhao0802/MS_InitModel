library(glmnet)
library(pROC)
library(ROCR)
library(caret)

data_dir <- "C:/Work/Projects/MultipleSclerosis/Results/2015-08-25/2015-08-25 11.18.18/"

myDataFrame <- read.csv(paste(data_dir, "data_B2B_edssprog_initmodel.csv", sep=""), 
                        header=TRUE, sep=",")

X <- data.matrix(myDataFrame[, 2:ncol(myDataFrame)])
y <- data.matrix(myDataFrame[, 1])

# stratification for evaluation

kFolds = 3
folds <- createFolds(y, k=kFolds, returnTrain=TRUE)

#

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

#

aucs_all_folds <- matrix(data=NA, nrow=kFolds, ncol=1)
lambda_all_folds <- matrix(data=NA, nrow=kFolds, ncol=1)

for (iFold in 1:length(folds))
{
  cat("Fold ", iFold, "\n")
  train_val_ids <- folds[[iFold]]
  X_train_val <- X[train_val_ids,]
  X_test <- X[-train_val_ids,]
  y_train_val <- y[train_val_ids]
  y_test <- y[-train_val_ids]
  
  # train
  
  set.seed(1011)
  # alpha=1 (default), lasso; alpha=0, ridge
  cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial", 
                   type.measure="auc", alpha=1)
  
  png(filename=paste(resultDir, "lambdaSelection_fold_", iFold, ".png"))
  plot(cv.fit)
  title("Binomial Family",line=2.5)
  dev.off()
  
  # test
  
  preds_probs <- predict(cv.fit, newx = X_test, type="response")
  
  # result metrics
  
  pred <- prediction(predictions=preds_probs, labels=y_test)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
  png(filename=paste(resultDir, "roc_fold_", iFold, ".png"))
  plot(perf, col=rainbow(10))
  dev.off()
  
  rocValues <- roc(response=as.vector(y_test), predictor=as.vector(preds_probs))
  cat("AUC: ", rocValues$auc, "\n")
  aucs_all_folds[iFold] = rocValues$auc
  lambda_all_folds[iFold] = cv.fit$lambda.1se
}

resultMat <- cbind(aucs_all_folds, lambda_all_folds)
colnames(resultMat) <- c("AUC", "lambda")
# rownames(resultMat) <- as.character(1:kFolds)
# for (iFold in 1:kFolds)
# {
#   rownames(resultMat)[iFold] <- paste("Fold", iFold)
# }
write.table(resultMat, sep=",", 
            file=paste(resultDir,"aucs.csv"), row.names=FALSE)

