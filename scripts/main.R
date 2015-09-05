rm(list=ls())

library(glmnet)
library(pROC)
library(ROCR)
library(caret)
library(doParallel)

source("functions/getPredefinedFolds.R")

#



#

# cl <- makeCluster(detectCores() - 1)
# registerDoParallel(cl, cores = detectCores() - 1)

# params


# alphaVals <- seq(0,1,0.1)
alphaVals <- c(1)
log_lambda_seq <- seq(log(1e-3),log(1e2),length.out=2)
lambda_seq <- exp(log_lambda_seq)
validation_thresh <- 0.2

# 

ptm <- proc.time()

# data

data_dir <- "C:/Work/Projects/MultipleSclerosis/Results/2015-09-04/2015-09-04 16.46.30/"

# data_names <- c("B2B_edssprog", 
#                 "B2B_relapse_fu_any_01",
#                 "B2B_progrelapse",
#                 "B2F_edssprog",
#                 "B2F_relapse_fu_any_01",
#                 "B2F_progrelapse",
#                 "B2S_edssprog",
#                 "B2S_edssconf3",
#                 "B2S_relapse_fu_any_01",
#                 "B2S_confrelapse",
#                 "B2S_progrelapse",
#                 "continue_edssprog",
#                 "continue_edssconf3",
#                 "continue_relapse_fu_any_01",
#                 "continue_confrelapse",
#                 "continue_progrelapse")

data_names <- c("B2B_edssconf3", "continue_progrelapse")

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
  
  dataset <- read.csv(paste(data_dir, data_names[iDataSet],".csv", sep=""), 
                      header=TRUE, sep=",")
  
  X <- data.matrix(dataset[, 2:(ncol(dataset)-1)])
  y <- dataset[, 1]
  
  # stratification for evaluation7
  
  folds <- getPredefinedFolds(dataset[,ncol(dataset)])
  kFoldsEval <- max(dataset$fold_id)
  # kFoldsVal <- length(folds) - 1
  # folds <- manualStratify(y, kFoldsEval)
  # folds <- createFolds(y, k=kFolds, returnTrain=TRUE)
  
  #
  
  preds_alldata_allAlphas <- matrix(data=-1.0, nrow=nrow(dataset), ncol=length(alphaVals))
  # aucs_all_folds_allAlphas <- matrix(data=0, nrow=kFoldsEval, ncol=length(alphaVals))
  colnames(preds_alldata_allAlphas) <- rep("",ncol(preds_alldata_allAlphas))
  # rownames(aucs_all_folds_allAlphas) <- rep("",nrow(aucs_all_folds_allAlphas))
  lambda_all_folds_allAlphas <- matrix(data=0, nrow=kFoldsEval, ncol=length(alphaVals))
  colnames(lambda_all_folds_allAlphas) <- rep("",ncol(lambda_all_folds_allAlphas))
  rownames(lambda_all_folds_allAlphas) <- rep("",nrow(lambda_all_folds_allAlphas))
  
  for (iFoldEval in 1:kFoldsEval)
  {
    test_ids <- folds[[iFoldEval]]
    X_test <- X[test_ids, ]
    y_test <- y[test_ids]
    fold_id_vec_test <- dataset$fold_id[test_ids]
    X_train_val <- X[-test_ids,]
    y_train_val <- y[-test_ids]
    fold_id_vec_train_val <- dataset$fold_id[-test_ids]
    
    
#     # plot the ordinal variables
#     png(filename=paste(resultDir, data_names[iDataSet], 
#                        "age_edss_fold_", iFold, ".png", sep=""))
#     plot(X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="age")], 
#          X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="baseline_edss_score")],
#          col="blue", pch=5)
#     points(X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="age")], 
#            X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="baseline_edss_score")],
#            col="red", pch=20)
#     dev.off()
    
    if (any(data_names[iDataSet] == 
            c("continue_edssprog", "continue_relapse_fu_any_01", 
              "continue_edssconf3", "continue_confrelapse",
              "continue_progrelapse")))
    {
      dayssup_name <- "precont_dayssup"
    } else
    {
      dayssup_name <- "switch_rx_dayssup"
    }
#     png(filename=paste(resultDir, data_names[iDataSet], "age_dayssup_fold_", 
#                        iFold, ".png", sep=""))
#     plot(X_train_val[which(y_train_val==0),which(colnames(X_train_val)=="age")], 
#          X_train_val[which(y_train_val==0),which(colnames(X_train_val)==dayssup_name)],
#          col="blue", pch=5)
#     points(X_train_val[which(y_train_val==1),which(colnames(X_train_val)=="age")], 
#            X_train_val[which(y_train_val==1),which(colnames(X_train_val)==dayssup_name)],
#            col="red", pch=20)
#     dev.off()
    
    rownames(lambda_all_folds_allAlphas)[iFoldEval] <- paste("fold_",iFoldEval,sep="")
    
    # compute weights
    
    weight_vec_train_val <- computeWeights(y_train_val)
    
    for (iAlpha in 1:length(alphaVals))
    {
      cat("Fold ", iFoldEval, "alpha ", alphaVals[iAlpha], "\n")
      
      # train
      
      # manual cross-validation
      
      auc_all_lambdas <-  rep(0, length(lambda_seq))
      
      for (iLambda in 1:length(lambda_seq))
      {
        auc_all_val_folds_one_lambda <- rep(0, kFoldsEval-1)
        
        for (iFoldVal in 1:(kFoldsEval-1))
        {
          fold_ids_train_val <- 1:kFoldsEval
          fold_ids_train_val <- fold_ids_train_val[fold_ids_train_val != iFoldEval]
          
          
          # only pick data in X_train_val and y_train_val
          
          # two cases: switches and continue
          
          if (grepl("continue", data_names[iDataSet])) # only pick one fold
          {
            fold_id_val <- fold_ids_train_val[iFoldVal]
            X_val <- X_train_val[which(fold_id_vec_train_val == fold_id_val),]
            X_train <- X_train_val[-which(fold_id_vec_train_val == fold_id_val),]
            y_val <- y_train_val[which(fold_id_vec_train_val == fold_id_val)]
            y_train <- y_train_val[-which(fold_id_vec_train_val == fold_id_val)]
            
            weight_vec_train <- 
              weight_vec_train_val[-which(fold_id_vec_train_val == fold_id_val)]
            
            
          } else 
          {
            # gather a number of folds so that validation has p percent of 
            # the positives in train_val, where p = validation_thresh. 
            # every fold has 1 positive
            n_folds_train_val <- length(fold_ids_train_val)
            n_folds_val <- ceiling(validation_thresh * n_folds_train_val)
            fold_ids_val_ids <- iFoldVal:(iFoldVal+n_folds_val-1)
            fold_ids_val_ids[which(fold_ids_val_ids > n_folds_train_val)] <- 
              fold_ids_val_ids[which(fold_ids_val_ids > n_folds_train_val)] - n_folds_train_val
            fold_ids_val <- fold_ids_train_val[fold_ids_val_ids]
            
            X_val <- X_train_val[fold_id_vec_train_val %in% fold_ids_val,]
            X_train <- X_train_val[!(fold_id_vec_train_val %in% fold_ids_val),]
            y_val <- y_train_val[fold_id_vec_train_val %in% fold_ids_val]
            y_train <- y_train_val[!(fold_id_vec_train_val %in% fold_ids_val)]
            
            weight_vec_train <- 
              weight_vec_train_val[!(fold_id_vec_train_val %in% fold_ids_val)]
          }
          
          # train and take down result for parameter selection
          
          fit_local <- glmnet(X_train, y_train, family="binomial", 
                              alpha=alphaVals[iAlpha],
                              weights=weight_vec_train, lambda=lambda_seq[iLambda])
          
          preds_probs_local <- predict(fit_local, newx = X_val, type="response",
                                       s="lambda.min")
          
          roc_values_local <- roc(response=as.vector(y_val), 
                                  predictor=as.vector(preds_probs_local))
          auc_all_val_folds_one_lambda[iFoldVal] = roc_values_local$auc
        }
        
        auc_all_lambdas[iLambda] <- mean(auc_all_val_folds_one_lambda)
      }
      
      iLambda_best <- which.max(auc_all_lambdas)
      lambda_best <- lambda_seq[iLambda_best]
      
      
      # evaluation
      fit_eval <- glmnet(X_train_val, y_train_val, family="binomial", 
                          alpha=alphaVals[iAlpha],
                          weights=weight_vec_train_val, lambda=lambda_best)
      
      save(fit_eval, file=paste(resultDir, "model_", data_names[iDataSet], "alpha", 
                              alphaVals[iAlpha], "_fold_", iFold, ".RData", sep=""))
      
      preds_probs_eval <- predict(fit_eval, newx = X_test, type="response")
      
      
      
      preds_alldata_allAlphas[test_ids, iAlpha] <- preds_probs_eval
      
#       roc_values_eval <- roc(response=as.vector(y_test), 
#                              predictor=as.vector(preds_probs_eval))
#       aucs_all_folds_allAlphas[iFoldEval, iAlpha] = roc_values_eval$auc
      colnames(preds_alldata_allAlphas)[iAlpha] <- paste("alpha_",alphaVals[iAlpha],sep="")
      lambda_all_folds_allAlphas[iFoldEval, iAlpha] = lambda_best
      colnames(lambda_all_folds_allAlphas)[iAlpha] <- paste("alpha_",alphaVals[iAlpha],sep="")
    }
  }
  
  
  # finally compute the result metrics using every prediction
  
  auc_all_alphas <- matrix(data=0, nrow=1, ncol=length(alphaVals))
  for (iAlpha in 1:length(alphaVals))
  {
    pred <- prediction(predictions=preds_alldata_allAlphas[,iAlpha], labels=y)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
    png(filename=paste(resultDir, data_names[iDataSet], "_roc_alpha", 
                       alphaVals[iAlpha], ".png", sep=""))
    plot(perf, col=rainbow(10))
    dev.off()
    
    roc_values_eval <- roc(response=as.vector(y), 
                           predictor=as.vector(preds_alldata_allAlphas[,iAlpha]))
    auc_all_alphas[iAlpha] = roc_values_eval$auc
    colnames(auc_all_alphas)[iAlpha] <- paste("alpha_",alphaVals[iAlpha],sep="")
  }
  
  write.table(auc_all_alphas, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_aucs.csv", sep=""), row.names=FALSE)
  
  write.table(preds_alldata_allAlphas, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_preds.csv", sep=""), row.names=FALSE)
  
#   write.table(aucs_all_folds_allAlphas, sep=",", 
#               file=paste(resultDir,data_names[iDataSet],"_aucs.csv", sep=""), col.names=NA)
  write.table(lambda_all_folds_allAlphas, sep=",", 
              file=paste(resultDir,data_names[iDataSet],"_lambdas.csv", sep=""), col.names=NA)
  
  if (iDataSet == 1)
    writeLines(paste("", paste(colnames(auc_all_alphas),collapse=","),sep=","), fileAvAUCs)
  writeLines(paste(data_names[iDataSet], paste(auc_all_alphas,collapse=","), sep=","), fileAvAUCs)
}

close(fileAvAUCs)

runtime <- proc.time() - ptm

cat(paste("Time elapsed:", round(runtime[3],1), "seconds."))

# stopCluster(cl)

