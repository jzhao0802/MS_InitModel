library(glmnet)
library(pROC)
library(ROCR)
library(caret)

# params

kFoldsEval <- 5
kFoldsVal <- 4
alphaVals <- seq(0,1,0.1)
log_lambda_seq <- seq(log(1e-4),log(1e4),0.1)
lambda_seq <- exp(log_lambda_seq)

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

# data_names <- c("B2S_edssprog",
#                 "continue_relapse_fu_any")

#

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


fileAvAUCs <- file(paste(resultDir, "AvAUCs.csv", sep=""), "w")


source("functions/computeWeights.R")
source("functions/manualStratify.R")

for (iDataSet in 1:length(data_names))
{
  cat(paste(data_names[iDataSet], "...\n", sep=""))
  
  dataset <- read.csv(paste(data_dir, "data_", data_names[iDataSet],"_initmodel.csv", sep=""), 
                      header=TRUE, sep=",")
  
  X <- data.matrix(dataset[, 2:ncol(dataset)])
  y <- dataset[, 1]
  
  # stratification for evaluation7
  
  folds <- manualStratify(y, kFoldsEval)
  # folds <- createFolds(y, k=kFolds, returnTrain=TRUE)
  
  #
  
  aucs_all_folds_allAlphas <- matrix(data=NA, nrow=kFoldsEval, ncol=length(alphaVals))
  colnames(aucs_all_folds_allAlphas) <- rep("",ncol(aucs_all_folds_allAlphas))
  rownames(aucs_all_folds_allAlphas) <- rep("",nrow(aucs_all_folds_allAlphas))
  lambda_all_folds_allAlphas <- matrix(data=NA, nrow=kFoldsEval, ncol=length(alphaVals))
  colnames(lambda_all_folds_allAlphas) <- rep("",ncol(lambda_all_folds_allAlphas))
  rownames(lambda_all_folds_allAlphas) <- rep("",nrow(lambda_all_folds_allAlphas))
  
  for (iFold in 1:length(folds))
  {
    train_val_ids <- folds[[iFold]]
    X_train_val <- X[train_val_ids,]
    X_test <- X[-train_val_ids,]
    y_train_val <- y[train_val_ids]
    y_test <- y[-train_val_ids]
    
    # plot the ordinal variables
    png(filename=paste(resultDir, data_names[iDataSet], "age_edss_fold_", iFold, ".png"))
    plot(X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="age")], 
         X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="baseline_edss_score")],
         col="blue", pch=5)
    points(X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="age")], 
           X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="baseline_edss_score")],
           col="red", pch=20)
    dev.off()
    
    if ((data_names[iDataSet] == "continue_edssprog") | (data_names[iDataSet] == "continue_relapse_fu_any"))
    {
      dayssup_name <- "precont_dayssup"
    } else
    {
      dayssup_name <- "switch_rx_dayssup"
    }
    png(filename=paste(resultDir, data_names[iDataSet], "age_dayssup_fold_", iFold, ".png"))
    plot(X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="age")], 
         X_train_val[which(y_train_val==0),which(colnames(X_train_val)==dayssup_name)],
         col="blue", pch=5)
    points(X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="age")], 
           X_train_val[which(y_train_val==1),which(colnames(X_train_val)==dayssup_name)],
           col="red", pch=20)
    dev.off()
    
    rownames(aucs_all_folds_allAlphas)[iFold] <- paste("fold_",iFold,sep="")
    rownames(lambda_all_folds_allAlphas)[iFold] <- paste("fold_",iFold,sep="")
    
    # compute weights
    
    weight_vec <- computeWeights(y_train_val)
    
    for (iAlpha in 1:length(alphaVals))
    {
      cat("Fold ", iFold, "alpha ", alphaVals[iAlpha], "\n")
      
      # train
      
      set.seed(1011)
      # alpha=1 (default), lasso; alpha=0, ridge
      cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial", 
                       type.measure="auc", alpha=alphaVals[iAlpha], 
                       weights=weight_vec, nfolds=kFoldsVal, 
                       lambda=lambda_seq)
      #     cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial", 
      #                      type.measure="auc", alpha=0)
      
      #     png(filename=paste(resultDir, "lambdaSelection_fold_", iFold, ".png"))
      #     plot(cv.fit)
      #     title("Binomial Family",line=2.5)
      #     dev.off()
      
      # test
      
      preds_probs <- predict(cv.fit, newx = X_test, type="response")
      
      # result metrics
      
      pred <- prediction(predictions=preds_probs, labels=y_test)
      perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
      png(filename=paste(resultDir, data_names[iDataSet], "_roc_alpha", alphaVals[iAlpha], "_fold_", iFold, ".png"))
      plot(perf, col=rainbow(10))
      dev.off()
      
      rocValues <- roc(response=as.vector(y_test), predictor=as.vector(preds_probs))
      cat("AUC: ", rocValues$auc, "\n")
      aucs_all_folds_allAlphas[iFold, iAlpha] = rocValues$auc
      colnames(aucs_all_folds_allAlphas)[iAlpha] <- paste("alpha_",alphaVals[iAlpha],sep="")
      lambda_all_folds_allAlphas[iFold, iAlpha] = cv.fit$lambda.1se
      colnames(lambda_all_folds_allAlphas)[iAlpha] <- paste("alpha_",alphaVals[iAlpha],sep="")
    }
  }
   
  
  write.table(aucs_all_folds_allAlphas, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_aucs.csv"), col.names=NA)
  write.table(lambda_all_folds_allAlphas, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_lambdas.csv"), col.names=NA)
  
  if (iDataSet == 1)
    writeLines(paste("", paste(colnames(aucs_all_folds_allAlphas),collapse=","),sep=","), fileAvAUCs)
  writeLines(paste(data_names[iDataSet], paste(colMeans(aucs_all_folds_allAlphas),collapse=","), sep=","), fileAvAUCs)
}

close(fileAvAUCs)



