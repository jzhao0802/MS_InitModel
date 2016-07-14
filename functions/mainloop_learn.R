library(dplyr)

selectAlphaLambda_ManualCV <- function(alphaVals, X_train_val, y_train_val, 
                                       weight_vec, bClassWeights, kFoldsVal, 
                                       lambda_seq, bParallel, iFold, n_alphas,
                                       # the next are output variables
                                       coefs_allalphas_folds, ranks_allalphas_folds)
{
  cv_results_allalphas <- list()
  
  for (iAlpha in 1:length(alphaVals))
  {
    # cat("Fold ", iFold, "alpha ", alphaVals[iAlpha], "\n")
    
    # train
    
    # set.seed(1011)
    # alpha=1 (default), lasso; alpha=0, ridge
    
    cv_results_allalphas[[iAlpha]] <- 
      manualCV_lambdaonly(X_train_val, y_train_val,
                          alphaVals[iAlpha], weight_vec, bClassWeights,
                          kFoldsVal, lambda_seq, bParallel)
    if (bClassWeights)
      fit_glmnet <- glmnet(X_train_val,y_train_val, family="binomial", 
                           weights=weight_vec,
                           alpha=alphaVals[iAlpha], lambda=lambda_seq)
    else
      fit_glmnet <- glmnet(X_train_val,y_train_val, family="binomial", 
                           alpha=alphaVals[iAlpha], lambda=lambda_seq)
    
    coefficients <- 
      coef(fit_glmnet, s=cv_results_allalphas[[iAlpha]][[1]])[2:(ncol(X_train_val)+1),]
    coefs_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <- coefficients
    colnames(coefs_allalphas_folds)[iAlpha+(iFold-1)*n_alphas] <-
      paste("alpha_", alphaVals[iAlpha], "_fold_", iFold, sep="")
    
    ranks_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <- 
      getRanking(coefficients)
    colnames(ranks_allalphas_folds)[iAlpha+(iFold-1)*n_alphas] <-
      paste("alpha_", alphaVals[iAlpha], "_fold_", iFold, sep="")
  }
  
  # find the best alpha
  best_auc <- 0
  for (iAlpha in 1:length(alphaVals))
  {
    if (cv_results_allalphas[[iAlpha]][[2]] > best_auc)
    {
      best_auc <- cv_results_allalphas[[iAlpha]][[2]]
      best_alpha <- alphaVals[iAlpha]
      best_lambda <- cv_results_allalphas[[iAlpha]][[1]]
    }
  }
  
  result <- list(alpha=best_alpha, lambda=best_lambda, 
                 coefs_allalphas_folds=coefs_allalphas_folds, 
                 ranks_allalphas_folds=ranks_allalphas_folds)
  return (result)
}

selectAlphaLambda_BuiltInCV <- function(alphaVals, X_train_val, y_train_val, 
                                       weight_vec, bClassWeights, kFoldsVal, 
                                       lambda_seq, bParallel, iFold, n_alphas,
                                       # the next are output variables
                                       coefs_allalphas_folds, ranks_allalphas_folds)
{
  cv_results_allalphas <- list()
  
  fold_ids <- stratifyFoldIDs(y_train_val, kFoldsVal)
  
  for (iAlpha in 1:length(alphaVals))
  {
    if (bClassWeights)
      cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial",
                       type.measure="auc", alpha=alphaVals[iAlpha],
                       weights=weight_vec, foldid=fold_ids,
                       lambda=lambda_seq, parallel=bParallel)
    else
      cv.fit=cv.glmnet(X_train_val, y_train_val, family="binomial",
                       type.measure="auc", alpha=alphaVals[iAlpha],
                       nfolds=kFoldsVal, foldid=fold_ids,
                       lambda=lambda_seq, parallel=bParallel)
    cv_result_auc <- cv.fit$cvm[which(cv.fit$lambda == cv.fit$lambda.min)]
    cv_results_allalphas[[iAlpha]] <- list(cv.fit, cv_result_auc)
    
    coefficients <-
      coef(cv.fit, s=cv.fit$lambda.min)[2:nrow(coef(cv.fit, s=cv.fit$lambda.min)),]
    coefs_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <- coefficients
    colnames(coefs_allalphas_folds)[iAlpha+(iFold-1)*n_alphas] <-
      paste("alpha_", alphaVals[iAlpha], "_fold_", iFold, sep="")
    
    ranks_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <-
      getRanking(coefficients)
    colnames(ranks_allalphas_folds)[iAlpha+(iFold-1)*n_alphas] <-
      paste("alpha_", alphaVals[iAlpha], "_fold_", iFold, sep="")
  }
  
  # find the best alpha
  best_auc <- 0
  for (iAlpha in 1:length(alphaVals))
  {
    if (cv_results_allalphas[[iAlpha]][[2]] > best_auc)
    {
      best_auc <- cv_results_allalphas[[iAlpha]][[2]]
      best_alpha <- alphaVals[iAlpha]
      best_lambda <- cv_results_allalphas[[iAlpha]][[1]]$lambda.min
    }
  }
  
  result <- list(alpha=best_alpha, lambda=best_lambda, 
                 coefs_allalphas_folds=coefs_allalphas_folds, 
                 ranks_allalphas_folds=ranks_allalphas_folds)
  return (result)
}

mainloop_learn <- function(bParallel, bManualCV, kFoldsEval, kFoldsVal, alphaVals, 
                           log_lambda_seq, bClassWeights, 
                           n_alphas, lambda_seq, data_dir, data_names, outcomeNames,
                           idColName,
                           resultDir)
{
  fileAvAUCs_test <- file(paste(resultDir, "AvAUCs_test.csv", sep=""), "w")
  fileAvAUCs_train <- file(paste(resultDir, "AvAUCs_train.csv", sep=""), "w")
  
  
  for (iCohort in 1:length(data_names))
  {
    cat(paste0(data_names[iCohort], ", "))
    
    dataset <- read.csv(paste0(data_dir, data_names[iCohort],"4model.csv"), 
                        header=TRUE, sep=",", check.names=FALSE)
    
    resultDirPerCohort <- paste0(resultDir, data_names[iCohort], "/")
    dir.create(resultDirPerCohort, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    
    for (outcomeName in outcomeNames)
    {
      cat(paste0(outcomeName, "\n"))
      
      resultDirPerOutcome <- paste0(resultDirPerCohort, outcomeName, "/")
      dir.create(resultDirPerOutcome, showWarnings = TRUE, recursive = TRUE, mode = "0777")
      
      
      y <- dataset[, outcomeName]
      
      top10varsDir <- paste0("F:\\Jie\\MS\\02_Code\\MS_InitModel\\Results\\2016-07-12 14.54.21\\1\\"
                             , data_names[iCohort]
                             , '\\'
                             , outcomeName
                             , '\\')
      
      avRank <- read.table(paste0(top10varsDir, 'av_ranking_Cmp.csv')
                           , sep=','
                           , header = T
                           , stringsAsFactors = F
      )
      top10Vars <- rownames(avRank)[order(avRank$x, decreasing = T)][1:10]
      
      X <- dplyr::select(dataset, -one_of(c(outcomeNames, idColName))) %>%
        select(one_of(top10Vars))
      
#       #
#       # for test
#       X <- dplyr::select(dataset, -one_of(c("B2B", "B2Fir", "B2Sec")))
#       X <- dplyr::select(dataset, -matches("dmts"))
      
      X <- data.matrix(X)
      
      write.table(cbind(y,X), sep=",", 
                  file=paste(resultDirPerOutcome, data_names[iCohort],"_data_for_model", 
                             ".csv", sep=""), row.names=F)
      
      
      #   # standardise
      #   col_ids_2_check <- which(apply(X, 2, sd) != 0)
      #   if (any(apply(X[,col_ids_2_check], 2, sd) != 1) 
      #       | any(apply(X[,col_ids_2_check], 2, mean) != 0)) {
      #     X[,col_ids_2_check] <- scale(X[,col_ids_2_check])
      #   } 
      n_data = nrow(X)
      
      
      # stratification for evaluation
      
      folds <- manualStratify(y, kFoldsEval)
      
      #
      # containers 
      
      predprobs_alldata <- matrix(data=-1, nrow=n_data, ncol=1)
      
      auc_train_allfolds <- 
        matrix(data=-1, nrow=kFoldsEval, ncol=1)
      
      params_allfolds <- matrix(data=-1, nrow=kFoldsEval, ncol=2)
      colnames(params_allfolds) <- c("alpha", "lambda")
      rownames(params_allfolds) <- rep("", nrow(params_allfolds))
      
      # variable importance rankings
      ranks_allalphas_folds <- matrix(data=-1, nrow=ncol(X), ncol=n_alphas*kFoldsEval)
      colnames(ranks_allalphas_folds) <- rep("", n_alphas*kFoldsEval)
      rownames(ranks_allalphas_folds) <- colnames(X)
      
      coefs_allalphas_folds <- matrix(data=-1, nrow=ncol(X), ncol=n_alphas*kFoldsEval)
      colnames(coefs_allalphas_folds) <- rep("", n_alphas*kFoldsEval)
      rownames(coefs_allalphas_folds) <- colnames(X)
      
      
      cat("Fold ")
      for (iFold in 1:length(folds))
      {
        cat(paste(iFold, "..", sep=""))
        
        train_val_ids <- folds[[iFold]]
        test_ids <- which(!((1:n_data) %in% train_val_ids))
        X_train_val <- X[train_val_ids,]
        X_test <- X[-train_val_ids,]
        y_train_val <- y[train_val_ids]
        y_test <- y[-train_val_ids]
        
        # compute weights
        
        weight_vec <- computeWeights(y_train_val)
        
        if (bManualCV)
          selected_alpha_lambda <- 
          selectAlphaLambda_ManualCV(alphaVals, X_train_val, y_train_val, 
                                     weight_vec, bClassWeights, kFoldsVal, 
                                     lambda_seq, bParallel, iFold, n_alphas,
                                     coefs_allalphas_folds, ranks_allalphas_folds)
        else
          selected_alpha_lambda <- 
          selectAlphaLambda_BuiltInCV(alphaVals, X_train_val, y_train_val, 
                                      weight_vec, bClassWeights, kFoldsVal, 
                                      lambda_seq, bParallel, iFold, n_alphas,
                                      coefs_allalphas_folds, ranks_allalphas_folds)
        
        best_alpha <- selected_alpha_lambda$alpha
        best_lambda <- selected_alpha_lambda$lambda
        coefs_allalphas_folds <- selected_alpha_lambda$coefs_allalphas_folds
        ranks_allalphas_folds <- selected_alpha_lambda$ranks_allalphas_folds
        
        # train with the selected params
        
        if (bClassWeights)
          fit_glmnet <- glmnet(X_train_val,y_train_val, family="binomial", 
                               weights=weight_vec,
                               alpha=best_alpha, lambda=lambda_seq)
        else
          fit_glmnet <- glmnet(X_train_val,y_train_val, family="binomial", 
                               alpha=best_alpha, lambda=lambda_seq)
        
        # test immediately on the training
        
        predprobs_train <- 
          predict(fit_glmnet, newx = X_train_val, type="response", s=best_lambda)
        rocValues_train <- 
          roc(response=as.vector(y_train_val), 
              predictor=as.vector(predprobs_train),
              direction="<")
        auc_train_allfolds[iFold, 1] <- rocValues_train$auc
        
        
        
        
        # test
        
        predprobs_test <- 
          predict(fit_glmnet, newx = X_test, type="response", s=best_lambda)
        
        
        
        # keep the prediction probs
        
        predprobs_alldata[test_ids, 1] <- 
          predprobs_test
        
        # 
        
        params_allfolds[iFold, 1] <- best_alpha
        params_allfolds[iFold, 2] <- best_lambda
        rownames(params_allfolds)[iFold] <- paste("fold_", iFold, sep="")
      }
      cat("\n")
      
      
      # result metrics
      
      pred <- 
        prediction(predictions=predprobs_alldata[, 1], labels=y)
      perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
      png(filename=paste(resultDirPerOutcome, data_names[iCohort], "_roc.png", sep=""))
      plot(perf, col=rainbow(10))
      dev.off()
      
      rocValues <- 
        roc(response=as.vector(y), 
            predictor=as.vector(predprobs_alldata[, 1]),
            direction="<")
      auc_test <- rocValues$auc
      
      # save all the predictions (of every imputation version and the pooled version)
      
      write.table(predprobs_alldata, sep=",", 
                  file=paste(resultDirPerOutcome, data_names[iCohort],"_probs.csv", sep=""), col.names=NA)
      
      write.table(params_allfolds, sep=",", 
                  file=paste(resultDirPerOutcome,data_names[iCohort],"_params.csv", sep=""), col.names=NA)
      
      # average ranking
      
      write.table(ranks_allalphas_folds, sep=",", 
                  file=paste(resultDirPerOutcome, "rankings_",data_names[iCohort], 
                             ".csv", sep=""))
      
      av_ranking <- matrix(rowMeans(ranks_allalphas_folds), ncol=1)
      rownames(av_ranking) <- rownames(ranks_allalphas_folds)
      av_ranking <- av_ranking[order(av_ranking),]
      write.table(av_ranking, sep=",", 
                  file=paste(resultDirPerOutcome, "av_ranking_",data_names[iCohort], 
                             ".csv", sep=""))
      
      # average coefficients
      
      write.table(coefs_allalphas_folds, sep=",", 
                  file=paste(resultDirPerOutcome, "coefs_",data_names[iCohort], 
                             ".csv", sep=""))
      
      av_coefs <- matrix(rowMeans(coefs_allalphas_folds), ncol=1)
      rownames(av_coefs) <- rownames(coefs_allalphas_folds)
      write.table(av_coefs, sep=",", 
                  file=paste(resultDirPerOutcome, "av_coefs_",data_names[iCohort], 
                             ".csv", sep=""))
      
      
      writeLines(paste(data_names[iCohort], outcomeName, 
                       paste(colMeans(auc_train_allfolds),collapse=","), 
                       sep=","), fileAvAUCs_train)
      writeLines(paste(data_names[iCohort], outcomeName, auc_test, sep=","), 
                 fileAvAUCs_test)
    }
    
  }
  
  close(fileAvAUCs_train)
  close(fileAvAUCs_test)
}