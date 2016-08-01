runRF_eval <- function(iFold){
  cat(paste(iFold, "..", sep=""))
  
  train_val_ids <- evalFolds[[iFold]]
  
  test_ids <- which(!((1:nrow(X)) %in% train_val_ids))
  X_train_val <- X[train_val_ids,]
  X_test <- X[-train_val_ids,]
  y_train_val <- y[train_val_ids]
  y_test <- y[-train_val_ids]
  
  bNum <- sapply(as.data.frame(X_train_val), is.numeric)
  charVars <- colnames(X_train_val)[!bNum]
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' evalFold-', iFold
      , ' character-', paste0(charVars, collapse = ','), '!\n')
  
  RF_fit <- randomForest(x=X_train_val,y=as.factor(y_train_val), 
                         ntree=ntree,
                         mtry=mtry,
                         #                              nodesize=ns, 
                         #importance=T,
                         #                              replace=replace,
                         #                              sampsize=nrow(cv_x_tr),
                         #strata=cv_resp_tr,
                         #                              keep.inbag=T,
                         classwt = wt
  ) 
  
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' evalFold-', iFold, ' RF fit end!\n')
  pred_rf_tr <- predict(RF_fit, X_train_val, type='prob')[, 2]
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' evalFold-', iFold, ' predict on training end!\n')
  
  pred_rf_ts <- predict(RF_fit, X_test, type='prob')[, 2]
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' evalFold-', iFold, ' predict on test end!\n')
  
  resp_pred_ts <- data.frame(resp=y_test, pred=pred_rf_ts)
  
  rocValues_train <- 
    roc(response=as.vector(y_train_val), 
        predictor=as.vector(pred_rf_tr),
        direction="<")
  auc_train <- rocValues_train$auc
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' get auc on training end!\n')
  
  resultLst <- list(auc_train=auc_train, resp_pred_ts=resp_pred_ts)
  return(resultLst)
}


runRF_grp <- function(grpId, cohort, resultDirPerCohort, outcome)
{
  
  
  newGrpVars <- newGrpVarsLst[[grpId]][-1]
  
  newGrpVarsFlag <- newGrpVarsLst[[grpId]][1]
  
  resultDir_thisGrp <- paste(resultDirPerCohort, outcome, "/", newGrpVarsFlag, '/', sep = '')
  
  dir.create(resultDir_thisGrp, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  
  if(all(newGrpVars=="")){
    data4RF <- data
  }else{
    added_df <- as.data.frame(t(ldply(lapply(newGrpVars, function(var){
      vct <- raw_data[data$record_num, var]
      vct[is.na(vct)] <- ifelse(is.numeric(vct), 99999, 'missing')
      return(vct)
    }), quickdf)))
    names(added_df) <- newGrpVars
    data4RF <- cbind(data, added_df)
    
  }
    
  
  X <- data4RF[, -match("y", names(data4RF))]
  # stratification for evaluation
  y <- data4RF$y
  
  set.seed(1)
  
  evalFolds <- manualStratify(y, kFoldsEval)
  
  saveRDS(evalFolds, file = paste0(resultDir_thisGrp, 'evalFolds.RDS'))
  # define container
#   pred_rf_ts <- numeric(length(y))
  
  sfInit(parallel=TRUE, cpus=nCore2Use4Eval, type='SOCK')
  sfSource("functions/funs_RF.R")
  sfSource("functions/manualStratify.R")
  
  sfExport('X', 'y', 'evalFolds', 'wt', 'ntree', 'mtry', 'traceFile', 'outcome'
           , 'newGrpVarsFlag')
  sfClusterEval(library("randomForest"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("pROC"))
  sfClusterEval(library("dplyr"))
  temp=sfClusterApplyLB(1:kFoldsEval, runRF_eval)
  sfStop()
  
  auc_tr_allEvalFolds <- unlist(lapply(temp, function(X)X$auc_train))
  auc_tr_avg_acrossEvalFolds <- mean(auc_tr_allEvalFolds)
  # get the test auc
  
  resp_pred_ts <- ldply(lapply(temp, function(X)X$resp_pred_ts), rbind)
  write.csv(resp_pred_ts, paste0(resultDir_thisGrp, 'resp_pred_ts.csv'))
  
  rocValues_test <- 
    roc(response=as.vector(resp_pred_ts$resp), 
        predictor=as.vector(resp_pred_ts$pred),
        direction="<")
  auc_test <- rocValues_test$auc
  cat(file=traceFile, append = T, 'outcome-', outcome, ' grp-', newGrpVarsFlag, ' get auc on test end!\n')
  
  oneGrp_auc_tr_ts <- c(group=newGrpVarsFlag, auc_tr=auc_tr_avg_acrossEvalFolds, auc_ts=auc_test)
  write.table(oneGrp_auc_tr_ts, paste0(resultDirPerCohort, 'auc_tr_ts_', outcome, '_', newGrpVarsFlag)
              , sep=","
              , row.names = T)
  
  return(oneGrp_auc_tr_ts)
  
}


runRF_outcome <- function(outcome, data, newGrpVarsLst, cohort, resultDirPerCohort){
  
  y <- data[, outcome]
  data$y <- y
  data <- data %>% select(-one_of(outcomeNamesAll))
  
  set.seed(1)
  folds <- manualStratify(y, 2)
  data <- data[folds[[1]], ]
  cat(file=traceFile, append = T, "parallel on grps of outcome-", outcome, " starts!\n")
  
  sfInit(parallel=TRUE, cpus=nCore2Use4Grp, type='SOCK')
  sfSource("functions/funs_RF.R")
  sfSource("functions/manualStratify.R")
  
  sfExport('data', 'newGrpVarsLst', 'resultDirPerCohort', "outcome", 'nCore2Use4Eval', 'kFoldsEval'
           , 'wt', 'ntree', 'mtry', 'traceFile', 'raw_data')
  sfExport('manualStratify', 'runRF_eval')
  sfClusterEval(library("randomForest"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("dplyr"))
  sfClusterEval(library("pROC"))
  sfClusterEval(library("snowfall"))
  
  temp <- sfClusterApplyLB(1:length(newGrpVarsLst), runRF_grp, cohort, resultDirPerCohort, outcome)
  sfStop()
  grp_auc_tr_ts <- ldply(temp, quickdf)
  names(grp_auc_tr_ts) <- c('Group', "AUC_on_training", "AUC_on_test")
  
  outcome_grp_auc_tr_ts <- cbind(outcome=outcome, grp_auc_tr_ts)
  return(outcome_grp_auc_tr_ts)
}
