runRF_eval <- function(iFold, newGrpVarsFlag){
  cat(paste(iFold, "..", sep=""))
  
  train_val_ids <- evalFolds[[iFold]]
  
  test_ids <- which(!((1:nrow(data4RF)) %in% train_val_ids))
  X_train_val <- X[train_val_ids,]
  X_test <- X[-train_val_ids,]
  y_train_val <- y[train_val_ids]
  y_test <- y[-train_val_ids]
  
  # compute weights
  
  weight <- main.arglist$wt
  
  RF_fit <- randomForest(x=x_train_val,y=y_train_val, 
                         ntree=main.arglist$ntree,
                         mtry=main.arglist$mtry,
                         #                              nodesize=ns, 
                         #importance=T,
                         #                              replace=replace,
                         #                              sampsize=nrow(cv_x_tr),
                         #strata=cv_resp_tr,
                         #                              keep.inbag=T,
                         classwt = wt
  ) 
  
  pred_rf_tr <- predict(RF_fit, X_train_val, type='prob')
  pred_rf_ts[test_ids] <<- predict(RF_fit, X_ts, type='prob')
  
  rocValues_train <- 
    roc(response=as.vector(y_train_val), 
        predictor=as.vector(pred_rf_tr),
        direction="<")
  auc_train <- rocValues_train$auc
  
  return(auc_train)
}


runRF_grp <- function(grpId, cohort, resultDir, outcome)
{
  
#   resultDir_thisGrp <- paste(resultDir, outcome, "/", newGrpVarsLst[grpId], '/', sep = '')
#   
#   dir.create(resultDir_thisGrp, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  newGrpVars <- newGrpVarsLst[[grpId]][-1]
  
  newGrpVarsFlag <- newGrpVarsLst[[grpId]][1]
  
  data4RF <- cbind(data, raw_data[data$record_num, newGrpVars])
  
  X <- data4RF[, -match("y", names(data4RF))]
  # stratification for evaluation
  y <- data4RF$y
  
  set.seed(1)
  
  evalFolds <- manualStratify(y, kFoldsEval)
  
  saveRDS(evalFolds, file = paste0(resultDir_thisGrp, 'evalFolds.RDS'))
  # define container
  pred_rf_ts <- numeric(length(y))
  
  runRF_eval(evalFold, newGrpVarsFlag)
  sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
  sfSource("functions/funs_RF.R")
  sfSource("functions/manualStratify.R")
  
  sfExport('X', 'y', 'evalFolds')
  sfExport('createCurve', 'grid_search_v2')
  sfClusterEval(library("randomForest"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("dplyr"))
  auc_tr_allEvalFolds <- unlist(sfClusterApplyLB(1:kFoldsEval, runRF_eval))
  sfStop()
  auc_tr_avg_acrossEvalFolds <- mean(auc_tr_allEvalFolds)
  # get the test auc
  rocValues_test <- 
    roc(response=as.vector(y), 
        predictor=as.vector(pred_rf_ts),
        direction="<")
  auc_test <- rocValues_test$auc
  return(c(group=newGrpVarsFlag, auc_tr=auc_tr_avg_acrossEvalFolds, auc_ts=auc_test))
  
}


runRF_outcome <- function(outcome, data, newGrpVarsLst, cohort, resultDir){
  
  y <- data[, outcome]
  set.seed(1)
  folds <- manualStratify(y, 2)
  data <- data[folds[[1]], ]
  data$y <- y
  data <- data %>% select(-one_of(setdiff(outcomeNamesAll, outcome)))
  runRF_grp(grpId, cohort, resultDir, outcome)
  
  sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
  sfSource("functions/funs_RF.R")
  sfSource("functions/manualStratify.R")
  
  sfExport('data', 'newGrpVarsLst')
  sfExport('createCurve', 'grid_search_v2')
  sfClusterEval(library("glmnet"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("dplyr"))
  temp <- sfClusterApplyLB(1:length(newGrpVarsLst), runRF_grp, cohort, resultDir, outcome)
  sfStop()
  grp_auc_tr_ts <- ldply(temp, quickdf)
  names(grp_auc_tr_ts) <- c('Group', "AUC_on_training", "AUC_on_test")
  
  outcome_grp_auc_tr_ts <- cbind(outcome=outcome, grp_auc_tr_ts)
  return(outcome_grp_auc_tr_ts)
}
