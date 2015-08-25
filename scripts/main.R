library(glmnet)
library(pROC)
library(ROCR)
library(caret)

# params

kFolds = 5

# data

data_dir <- "C:/Work/Projects/MultipleSclerosis/Results/2015-08-25/2015-08-25 11.18.18/"

data_names <- c("B2B_edssprog", 
                "B2B_relapse_fu_any",
                "B2F_edssprog",
                "B2F_relapse_fu_any",
                "B2S_edssprog",
                "B2S_relapse_fu_any",
                "continue_edssprog",
                "continue_relapse_fu_any")

#

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


fileAvAUCs <- file(paste(resultDir, "AvAUCs.csv", sep=""), "w")

source("functions/computeWeights.R")

for (iDataSet in 1:length(data_names))
{
  cat(paste(data_names[iDataSet], "...\n", sep=""))
  
  dataset <- read.csv(paste(data_dir, "data_", data_names[iDataSet],"_initmodel.csv", sep=""), 
                      header=TRUE, sep=",")
  
  X <- data.matrix(dataset[, 2:ncol(dataset)])
  y <- data.matrix(dataset[, 1])
  
  # stratification for evaluation7
  
  folds <- createFolds(y, k=kFolds, returnTrain=TRUE)
  
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
    
    # compute weights
    
    weight_vec <- computeWeights(y_train_val)
    
    # train
    
    set.seed(1011)
    # alpha=1 (default), lasso; alpha=0, ridge
    cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial", 
                     type.measure="auc", alpha=0, weights=weight_vec)
    
#     png(filename=paste(resultDir, "lambdaSelection_fold_", iFold, ".png"))
#     plot(cv.fit)
#     title("Binomial Family",line=2.5)
#     dev.off()
    
    # test
    
    preds_probs <- predict(cv.fit, newx = X_test, type="response")
    
    # result metrics
    
    pred <- prediction(predictions=preds_probs, labels=y_test)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
    png(filename=paste(resultDir, data_names[iDataSet], "_roc_fold_", iFold, ".png"))
    plot(perf, col=rainbow(10))
    dev.off()
    
    rocValues <- roc(response=as.vector(y_test), predictor=as.vector(preds_probs))
    cat("AUC: ", rocValues$auc, "\n")
    aucs_all_folds[iFold] = rocValues$auc
    lambda_all_folds[iFold] = cv.fit$lambda.1se
  }
  
  resultMat <- cbind(aucs_all_folds, lambda_all_folds)
  colnames(resultMat) <- c("AUC", "lambda")
  
  write.table(resultMat, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_aucs.csv"), col.names=NA)
  
  writeLines(paste(data_names[iDataSet], mean(resultMat[,1]), sep=","), fileAvAUCs)
}

close(fileAvAUCs)



