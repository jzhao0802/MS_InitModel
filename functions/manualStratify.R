library(caret)
library(compiler)

manualStratify <- cmpfun(function(y, k_folds)
{
  if (!(all(levels(y) %in% c(0,1))))
  {
    stop(paste("Error! Invalid y-values for stratification. ", 
               "stratifySmallSample currently only supports one type ",
               "of y values: c(0, 1). "))
  }
  
  # get the positive and negative
  
  pos_indices <- which(y==1)
  if (length(pos_indices) < k_folds)
    stop("Error! Too few positives. StratifyEasyDifficultPositives failed.")
  neg_indices <- which(y==0)
  if (length(neg_indices) < k_folds)
    stop("Error! Too few negatives. StratifyEasyDifficultPositives failed.")
  pos_indices <- sample(pos_indices)
  neg_indices <- sample(neg_indices)
  
  # 
  
  pos_ids_allfolds <- DivideIntoFolds(pos_indices, k_folds)
  neg_ids_allfolds <- DivideIntoFolds(neg_indices, k_folds)
  
  folds <- list()
  
  for (i_fold in 1:k_folds)
  {
    IDsInThisFold <- c(pos_ids_allfolds[[i_fold]], 
                       neg_ids_allfolds[[i_fold]])
    folds[[i_fold]] <- 
      (1:length(y))[which(!((1:length(y)) %in% IDsInThisFold))]
  }
  
  return (folds)
}, option=list(optimize=3))

stratifyFoldIDs <- function(y, k_folds)
{
  ids <- 1:k_folds
  ids_every_pos <- rep(ids, length.out=sum(y==1))
  ids_every_neg <- rep(ids, length.out=sum(y!=1))
  
  ids_every_datum <- rep(-1, length(y))
  ids_every_datum[which(y==1)] <- ids_every_pos
  ids_every_datum[which(y!=1)] <- ids_every_neg
  
  return (ids_every_datum)
}