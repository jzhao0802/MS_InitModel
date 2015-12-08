mainloop_transferlearn <- function(bParallel, kFoldsEval, kFoldsVal, alphaVals, 
                                   log_lambda_seq, bClassWeights, 
                                   n_alphas, lambda_seq, data_dir, cohort_names, 
                                   outcome_names, resultDir)
{
  fileAvAUCs_test <- file(paste(resultDir, "AvAUCs_test.csv", sep=""), "w")
  fileAvAUCs_train_newdomain <- file(paste(resultDir, "AvAUCs_train_newdomain.csv", sep=""), "w")
  fileAvAUCs_train_olddomain <- file(paste(resultDir, "AvAUCs_train_olddomain.csv", sep=""), "w")
  
  i_study = 1
  
  for (i_coh in 1:length(cohort_names))
  {
    if (cohort_names[i_coh] == "continue")
      next
    
    for (i_out in 1:length(outcome_names))
    {
      if (((cohort_names[i_coh] == "B2B") | (cohort_names[i_coh] == "B2F")) & 
          ((outcome_names[i_out] == "edssconf3") | (outcome_names[i_out] == "confrelapse")))
        next
      
      study_name <- paste(cohort_names[i_coh], "_", outcome_names[i_out], sep="")
      cat(paste(study_name, ", fold ", sep=""))
      
      # data from the new domain
      dataset_newdomain <- read.csv(paste(data_dir, study_name,"_data_for_model.csv", sep=""), 
                                    header=TRUE, sep=",", check.names=FALSE)
      # the first column is index
      dataset_newdomain[,1] <- NULL
      
      y_newdomain <- dataset_newdomain[, 1]
      X_newdomain <- dataset_newdomain[, 2:ncol(dataset_newdomain)]
      
      # remove vars in the new domain that have constant values
      
      sds_vars_newdomain <- apply(X_newdomain, 2, sd)
      const_vars_newdomain <- colnames(X_newdomain)[which(sds_vars_newdomain == 0)]
      X_newdomain <- X_newdomain[, !(colnames(X_newdomain) %in% const_vars_newdomain)]
      
      # one more set of predictors indiciting the continuation/switch/escalation
      # 3 Booleans corresponding to B2B, B2F and B2S. It's continue if all zeros. 
      
      cohort_vars <- matrix(data=0, nrow=length(y_newdomain), ncol=3)
      if (cohort_names == "B2B")
        cohort_vars[,1] <- 1
      else if (cohort_names == "B2F")
        cohort_vars[,2] <- 1
      else
        cohort_vars[,3] <- 1
      colnames(cohort_vars) <- c("B2B", "B2F", "B2S")
      
      X_newdomain <- cbind(X_newdomain, cohort_vars)
      
      # data from the old domain
      dataset_olddomain <- read.csv(paste(data_dir, "continue_", outcome_names[i_out],"_data_for_model.csv", sep=""), 
                                    header=TRUE, sep=",", check.names=FALSE)
      # the first column is index
      dataset_olddomain[,1] <- NULL
      
      y_olddomain <- dataset_olddomain[, 1]
      X_olddomain <- dataset_olddomain[, 2:ncol(dataset_olddomain)]
      
      # also has the set of Booleans indicating the cohort, but all zeros
      X_olddomain <- cbind(X_olddomain, matrix(data=0, nrow=length(y_olddomain), ncol=3))
      colnames(X_olddomain)[(ncol(X_olddomain)-2):ncol(X_olddomain)] <- c("B2B", "B2F", "B2S")
      
      # merge the two domains: [intersect, old, new]
      
      org_varnames_newdomain <- colnames(X_newdomain)
      org_varnames_olddomain <- colnames(X_olddomain)
      varnames_intersect <- intersect(colnames(X_newdomain), colnames(X_olddomain))
      n_intersect_vars <- length(varnames_intersect)
      n_olddomain_vars <- ncol(X_olddomain)
      n_newdomain_vars <- ncol(X_newdomain)
      
      X_newdomain <- cbind(X_newdomain[, varnames_intersect], 
                           matrix(data=0, nrow=length(y_newdomain), ncol=n_olddomain_vars),
                           X_newdomain)
      colnames(X_newdomain)[1:n_intersect_vars] <- 
        AddSuffix2SetOfStrings("intersect", varnames_intersect)
      colnames(X_newdomain)[(n_intersect_vars+1):(n_intersect_vars+n_olddomain_vars)] <- 
        AddSuffix2SetOfStrings("olddomain", org_varnames_olddomain)
      colnames(X_newdomain)[(n_intersect_vars+n_olddomain_vars+1):ncol(X_newdomain)] <- 
        AddSuffix2SetOfStrings("newdomain", org_varnames_newdomain)
      
      X_newdomain <- data.matrix(X_newdomain)
      
      X_olddomain <- cbind(X_olddomain[, varnames_intersect],
                           X_olddomain, 
                           matrix(data=0, nrow=length(y_olddomain), ncol=n_newdomain_vars))
      colnames(X_olddomain)[1:n_intersect_vars] <- 
        AddSuffix2SetOfStrings("intersect", varnames_intersect)
      colnames(X_olddomain)[(n_intersect_vars+1):(n_intersect_vars+n_olddomain_vars)] <- 
        AddSuffix2SetOfStrings("olddomain", org_varnames_olddomain)
      colnames(X_olddomain)[(n_intersect_vars+n_olddomain_vars+1):ncol(X_olddomain)] <- 
        AddSuffix2SetOfStrings("newdomain", org_varnames_newdomain)
      
      X_olddomain <- data.matrix(X_olddomain)
      
      
      #   # standardise
      #   col_ids_2_check <- which(apply(X, 2, sd) != 0)
      #   if (any(apply(X[,col_ids_2_check], 2, sd) != 1) 
      #       | any(apply(X[,col_ids_2_check], 2, mean) != 0)) {
      #     X[,col_ids_2_check] <- scale(X[,col_ids_2_check])
      #   } 
      
      n_data_newdomain <- length(y_newdomain)
      
      
      # stratification for evaluation
      
      folds <- manualStratify(y_newdomain, kFoldsEval)
      # folds <- stratifySmallSample(y_newdomain, kFoldsEval)
      #     for (i_fold in 1:kFoldsEval)
      #     {
      #       retain_ids <- folds[[i_fold]]
      #       y_retain <- y_newdomain[retain_ids]
      #       y_thisfold <- y_newdomain[-retain_ids]
      #       cat("fold", i_fold, ":")
      #       cat("nPos", sum(y_thisfold), "; nNeg: ", length(y_thisfold)-sum(y_thisfold))
      #       cat("\n")
      #     }
      
      #
      # containers 
      
      predprobs_alldata <- matrix(data=-1, nrow=n_data_newdomain, ncol=1)
      
      auc_train_allfolds_olddomain <- 
        matrix(data=-1, nrow=kFoldsEval, ncol=1)
      
      auc_train_allfolds_newdomain <- 
        matrix(data=-1, nrow=kFoldsEval, ncol=1)
      
      params_allfolds <- matrix(data=-1, nrow=kFoldsEval, ncol=2)
      colnames(params_allfolds) <- c("alpha","lambda")
      rownames(params_allfolds) <- rep("", nrow(params_allfolds))
      
      # variable importance rankings
      ranks_allalphas_folds <- matrix(data=-1, nrow=ncol(X_newdomain), ncol=n_alphas*kFoldsEval)
      colnames(ranks_allalphas_folds) <- rep("", n_alphas*kFoldsEval)
      rownames(ranks_allalphas_folds) <- colnames(X_newdomain)
      
      coefs_allalphas_folds <- matrix(data=-1, nrow=ncol(X_newdomain), ncol=n_alphas*kFoldsEval)
      colnames(coefs_allalphas_folds) <- rep("", n_alphas*kFoldsEval)
      rownames(coefs_allalphas_folds) <- colnames(X_newdomain)
      
      
      
      for (iFold in 1:length(folds))
      {
        cat(paste(iFold, "..", sep=""))
        
        train_val_ids <- folds[[iFold]]
        test_ids <- which(!((1:n_data_newdomain) %in% train_val_ids))
        X_train_val <- X_newdomain[train_val_ids,]
        X_test <- X_newdomain[test_ids,]
        y_train_val <- y_newdomain[train_val_ids]
        y_test <- y_newdomain[test_ids]
        
        if (any(study_name == 
                c("continue_edssprog", "continue_relapse_fu_any_01", 
                  "continue_confrelapse", "continue_edssconf3", 
                  "continue_progrelapse")))
        {
          dayssup_name <- "precont_dayssup"
        } else
        {
          dayssup_name <- "switch_rx_dayssup"
        }
        
        # compute weights
        
        if (bClassWeights)
          weight_vec <- computeWeights_2domains(y_train_val, y_olddomain)
        else
          weight_vec <- NULL
        
        
        cv_results_allalphas <- list()
        
        for (iAlpha in 1:length(alphaVals))
        {
          cat("alpha: ", alphaVals[iAlpha],"\n")
          # train
          
          # set.seed(1011)
          # alpha=1 (default), lasso; alpha=0, ridge
          
          # a list: list(lambda_best, max_auc)      
          cv_results_allalphas[[iAlpha]] <- 
            manualCV_lambdaonly_2Domains(X_train_val, X_olddomain, 
                                         y_train_val, y_olddomain, 
                                         alphaVals[iAlpha], 
                                         weight_vec, bClassWeights, kFoldsVal, lambda_seq, bParallel)
          
          # take down the coefficients and ranking
          
          if (bClassWeights)
          {
            fit_glmnet <- glmnet(rbind(X_train_val, X_olddomain), c(y_train_val, y_olddomain), family="binomial", 
                                 weights=weight_vec,
                                 alpha=alphaVals[iAlpha], lambda=lambda_seq)
          } else
          {
            fit_glmnet <- glmnet(rbind(X_train_val, X_olddomain), c(y_train_val, y_olddomain), family="binomial", 
                                 alpha=alphaVals[iAlpha], lambda=lambda_seq)
          }
          
          
          coefficients_minlambda <- 
            predict(fit_glmnet, s=cv_results_allalphas[[iAlpha]][[1]], type="coefficients")[2:(1+ncol(X_train_val))]
          coefs_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <- 
            coefficients_minlambda
          colnames(coefs_allalphas_folds)[iAlpha+(iFold-1)*n_alphas] <-
            paste("alpha_", alphaVals[iAlpha], "_fold_", iFold, sep="")
          
          ranks_allalphas_folds[, iAlpha+(iFold-1)*n_alphas] <- 
            getRanking(coefficients_minlambda)
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
        
        # use the selected lambda and alpha to fit the model
        
        if (bClassWeights)
        {
          fit_glmnet <- glmnet(rbind(X_train_val, X_olddomain), c(y_train_val, y_olddomain), family="binomial", 
                               weights=weight_vec,
                               alpha=best_alpha, lambda=lambda_seq)
        } else
        {
          fit_glmnet <- glmnet(rbind(X_train_val, X_olddomain), c(y_train_val, y_olddomain), family="binomial", 
                               alpha=best_alpha, lambda=lambda_seq)
        }
        
        
        # test immediately on the training
        
        # on the new domain training data
        predprobs_train_newdomain <- 
          predict(fit_glmnet, newx=X_train_val, s=best_lambda, type="response")
        rocValues_train_newdomain <- 
          roc(response=as.vector(y_train_val), 
              predictor=as.vector(predprobs_train_newdomain), 
              direction="<")
        auc_train_allfolds_newdomain[iFold, 1] <- rocValues_train_newdomain$auc
        
        # on the old domain training data
        predprobs_train_olddomain <- 
          predict(fit_glmnet, newx=X_olddomain, s=best_lambda, type="response")
        rocValues_train_olddomain <- 
          roc(response=as.vector(y_olddomain), 
              predictor=as.vector(predprobs_train_olddomain),
              direction="<")
        auc_train_allfolds_olddomain[iFold, 1] <- rocValues_train_olddomain$auc
        
        
        
        # test
        
        predprobs_test <- 
          predict(fit_glmnet, newx=X_test, s=best_lambda, type="response")
        
        # keep the prediction probs
        
        predprobs_alldata[test_ids, 1] <- predprobs_test
        
        # 
        
        params_allfolds[iFold, 1] <- best_alpha
        params_allfolds[iFold, 2] <- best_lambda
        rownames(params_allfolds)[iFold] <- paste("fold_", iFold, sep="")
      }
      cat("\n")
      
      
      # average the probabilities of all evaluation folds as the final result
      
      pred <- 
        prediction(predictions=predprobs_alldata[, 1], labels=y_newdomain)
      perf <- 
        performance(pred, measure = "tpr", x.measure = "fpr") 
      png(filename=paste(resultDir, study_name, "_roc.png", sep=""))
      plot(perf, col=rainbow(10))
      dev.off()
      
      rocValues <- 
        roc(response=as.vector(y_newdomain), 
            predictor=as.vector(predprobs_alldata[, 1]),
            direction="<")
      av_auc <- rocValues$auc
      
      # save all the predictions (of every imputation version and the pooled version)
      
      write.table(predprobs_alldata, sep=",", 
                  file=paste(resultDir, study_name,"_probs.csv", sep=""), col.names=NA)
      
      write.table(params_allfolds, sep=",", 
                  file=paste(resultDir,study_name,"params_allfolds.csv", sep=""), col.names=NA)
      
      # average ranking
      
      write.table(ranks_allalphas_folds, sep=",", 
                  file=paste(resultDir, "rankings_",study_name, ".csv", sep=""))
      
      av_ranking <- matrix(rowMeans(ranks_allalphas_folds), ncol=1)
      rownames(av_ranking) <- rownames(ranks_allalphas_folds)
      av_ranking <- av_ranking[order(av_ranking),]
      write.table(av_ranking, sep=",", 
                  file=paste(resultDir, "av_ranking_",study_name, ".csv", sep=""))
      
      # average coefficients
      
      write.table(coefs_allalphas_folds, sep=",", 
                  file=paste(resultDir, "coefs_",study_name, ".csv", sep=""))
      
      av_coefs <- matrix(rowMeans(coefs_allalphas_folds), ncol=1)
      rownames(av_coefs) <- rownames(coefs_allalphas_folds)
      write.table(av_coefs, sep=",", 
                  file=paste(resultDir, "av_coefs_",study_name, ".csv", sep=""))
      
      
      writeLines(paste(study_name, 
                       paste(colMeans(auc_train_allfolds_newdomain),collapse=","), 
                       sep=","), fileAvAUCs_train_newdomain)
      writeLines(paste(study_name, 
                       paste(colMeans(auc_train_allfolds_olddomain),collapse=","), 
                       sep=","), fileAvAUCs_train_olddomain)
      writeLines(paste(study_name, av_auc, sep=","), fileAvAUCs_test)
      
      i_study = i_study + 1
    }
  }
  
  close(fileAvAUCs_train_newdomain)
  close(fileAvAUCs_train_olddomain)
  close(fileAvAUCs_test)
}