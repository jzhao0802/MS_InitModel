library(foreach)
library(doParallel)
library(glmnet)
library(compiler)

# comb <- function(x, ...) {
#   lapply(seq_along(x),
#          function(i) 
#          {
#            c(x[[i]], lapply(list(...), function(y) y[[i]]))
#          })
# }

manualCV_lambdaonly <- cmpfun(function(X_train_val, y_train_val, 
                                alpha_value, weight_vec, bClassWeights, kFoldsVal, lambda_seq, bParallel)
{
  # X_newdomain isn't stratified. Stratify it first. 
  folds <- manualStratify(y_train_val, kFoldsVal)
  
  
  if (bParallel)
  {
    result_aucs <- foreach(candidate_lambda=lambda_seq, .combine='cbind', 
                           .multicombine=TRUE,
                           # .export=c('computeWeights', 'computeWeights_DiscreteClasses'),
                           .packages=c("glmnet", "pROC", "ROCR")) %dopar% 
                           {
                             auc_allfolds_onelambda <- rep(0, kFoldsVal)
                             for (i_fold in 1:kFoldsVal)
                             {
                               # cat("loop ", i_fold, "\n")
                               train_ids <- folds[[i_fold]]
                               val_ids <- which(!((1:length(y_train_val)) %in% train_ids))
                               X_train <- data.matrix(X_train_val[train_ids,])
                               y_train <- y_train_val[train_ids]
                               X_val <- data.matrix(X_train_val[val_ids,])
                               y_val <- y_train_val[val_ids]                               

                               if (bClassWeights)
                               {
                                 weight_vec_train <- weight_vec[train_ids]
                                 
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial",
                                                      weights=weight_vec_train,
                                                      alpha=alpha_value, lambda=lambda_seq)
                               } else 
                               {
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial",
                                                      alpha=alpha_value, lambda=lambda_seq)
                               }

                               # validate
                               
                               predprobs <- 
                                 predict(fit_glmnet, newx=X_val, s=candidate_lambda, type="response")
                               roc_obj <- roc(response=as.vector(y_val), predictor=as.vector(predprobs),
                                              direction="<")
                               auc_allfolds_onelambda[i_fold] <- roc_obj$auc
                             }
                             auc_allfolds_onelambda
                           }
  } else
  {
    result_aucs <- foreach(candidate_lambda=lambda_seq, .combine='cbind', 
                           .multicombine=TRUE,
                           .packages=c("glmnet", "pROC", "ROCR")) %do% 
                           {
                             auc_allfolds_onelambda <- rep(0, kFoldsVal)
                             for (i_fold in 1:kFoldsVal)
                             {
                               # cat("loop ", i_fold, "\n")
                               train_ids <- folds[[i_fold]]
                               val_ids <- which(!((1:length(y_train_val)) %in% train_ids))
                               X_train <- data.matrix(X_train_val[train_ids,])
                               y_train <- y_train_val[train_ids]
                               X_val <- data.matrix(X_train_val[val_ids,])
                               y_val <- y_train_val[val_ids]
                               
                               if (bClassWeights)
                               {
                                 weight_vec_train <- weight_vec[train_ids]
                                 
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial",
                                                      weights=weight_vec_train,
                                                      alpha=alpha_value, lambda=lambda_seq)
                               } else 
                               {
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial",
                                                      alpha=alpha_value, lambda=lambda_seq)
                               }
                               
                               # validate
                               
                               predprobs <- 
                                 predict(fit_glmnet, newx=X_val, s=candidate_lambda, type="response")
                               roc_obj <- roc(response=as.vector(y_val), predictor=as.vector(predprobs),
                                              direction="<")
                               auc_allfolds_onelambda[i_fold] <- roc_obj$auc
                               
                             }
                             
                             
                             auc_allfolds_onelambda
                           }
  }
  
  # compute the means and aucs for all lambdas
  
  col_means <- colMeans(result_aucs)
  col_ses <- apply(result_aucs, 2, sd) / sqrt(kFoldsVal)
  
  # find the 'min' and 1se lambdas
  
  max_auc <- max(col_means)
  # cat("max_auc: ", max_auc, "; ")
  arg_max <- which.max(col_means)
  lambda_best <- lambda_seq[arg_max]
  sd_max <- col_ses[arg_max]
  threshold <- max_auc - sd_max
  col_means[col_means < threshold] = 1
  arg_1se <- which.min(col_means)
  lambda_1se <- lambda_seq[arg_1se]
  
  # return the 2 selected lambdas: min and 1se
  
  list(lambda_best, max_auc)
  
  
}, options=list(optimize=3))

manualCV_lambdaonly_2Domains <- cmpfun(function(X_newdomain, X_olddomain, 
                                         y_newdomain, y_olddomain, 
                                         alpha_value, weight_vec, bClassWeights,
                                         kFoldsVal, lambda_seq, bParallel)
{
  # X_newdomain isn't stratified. Stratify it first. 
  folds <- manualStratify(y_newdomain, kFoldsVal)
  
  
  if (bParallel)
  {
    result_aucs <- foreach(candidate_lambda=lambda_seq, .combine='cbind', 
                           .multicombine=TRUE,
                           .packages=c("glmnet", "pROC", "ROCR")) %dopar% 
                           {
                             auc_allfolds_onelambda <- rep(0, kFoldsVal)
                             for (i_fold in 1:kFoldsVal)
                             {
                               # cat("loop ", i_fold, "\n")
                               train_ids <- folds[[i_fold]]
                               val_ids <- which(!((1:length(y_newdomain)) %in% train_ids))
                               X_train <- data.matrix(rbind(X_newdomain[train_ids,], X_olddomain))
                               y_train <- c(y_newdomain[train_ids], y_olddomain)
                               X_val <- data.matrix(X_newdomain[val_ids,])
                               y_val <- y_newdomain[val_ids]

                               if (bClassWeights)
                               {
                                 weight_vec_train <- c(weight_vec[train_ids], 
                                                       weight_vec[(length(y_newdomain)+1):length(weight_vec)])
                                 
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial", 
                                                      weights=weight_vec_train,
                                                      alpha=alpha_value, lambda=lambda_seq)
                               } else
                               {
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial", 
                                                      alpha=alpha_value, lambda=lambda_seq)
                               }
                               
                               # validate
                               
                               predprobs <- 
                                 predict(fit_glmnet, newx=X_val, s=candidate_lambda, type="response")
                               roc_obj <- roc(response=as.vector(y_val), predictor=as.vector(predprobs),
                                              direction="<")
                               auc_allfolds_onelambda[i_fold] <- roc_obj$auc                               
                             }
                             auc_allfolds_onelambda
                           }
  } else
  {
    result_aucs <- foreach(candidate_lambda=lambda_seq, .combine='cbind', 
                           .multicombine=TRUE,
                           .packages=c("glmnet", "pROC", "ROCR")) %do% 
                           {
                             auc_allfolds_onelambda <- rep(0, kFoldsVal)
                             for (i_fold in 1:kFoldsVal)
                             {
                               # cat("loop ", i_fold, "\n")
                               train_ids <- folds[[i_fold]]
                               val_ids <- which(!((1:length(y_newdomain)) %in% train_ids))
                               X_train <- data.matrix(rbind(X_newdomain[train_ids,], X_olddomain))
                               y_train <- c(y_newdomain[train_ids], y_olddomain)
                               X_val <- data.matrix(X_newdomain[val_ids,])
                               y_val <- y_newdomain[val_ids]
                               
                               if (bClassWeights)
                               {
                                 weight_vec_train <- c(weight_vec[train_ids], 
                                                       weight_vec[(length(y_newdomain)+1):length(weight_vec)])
                                 
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial", 
                                                      weights=weight_vec_train,
                                                      alpha=alpha_value, lambda=lambda_seq)
                               } else
                               {
                                 fit_glmnet <- glmnet(X_train, y_train, family="binomial", 
                                                      alpha=alpha_value, lambda=lambda_seq)
                               }
                               
                               # validate
                               
                               predprobs <- 
                                 predict(fit_glmnet, newx=X_val, s=candidate_lambda, type="response")
                               roc_obj <- roc(response=as.vector(y_val), predictor=as.vector(predprobs),
                                              direction="<")
                               auc_allfolds_onelambda[i_fold] <- roc_obj$auc                               
                             }
                             auc_allfolds_onelambda
                           }
  }
  
  
  # compute the means and aucs for all lambdas
  
  col_means <- colMeans(result_aucs)
  col_ses <- apply(result_aucs, 2, sd) / sqrt(kFoldsVal)
  
  # find the 'min' and 1se lambdas
  
  max_auc <- max(col_means)
  cat("max_auc: ", max_auc, "; ")
  arg_max <- which.max(col_means)
  lambda_best <- lambda_seq[arg_max]
  sd_max <- col_ses[arg_max]
  threshold <- max_auc - sd_max
  col_means[col_means < threshold] = 1
  arg_1se <- which.min(col_means)
  lambda_1se <- lambda_seq[arg_1se]
  
  
  list(lambda_best, max_auc)
  
  
}, options=list(optimize=3))
