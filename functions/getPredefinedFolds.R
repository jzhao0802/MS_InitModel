# return a list
getPredefinedFolds <- function(fold_id_vec)
{
  n_folds <- max(fold_id_vec)
  
  folds <- as.list(rep("",n_folds))
  
  for (i_fold in 1:n_folds)
  {
    folds[[i_fold]] <- which(fold_id_vec == i_fold)
  }
  
  return (folds)
}