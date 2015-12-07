library(compiler)

calMatchVec <- cmpfun(function(patterns, string)
{
  if (length(patterns) == 0)
  {
    return (FALSE)
  }
  result <- rep(FALSE, length(patterns))
  for (i in 1:length(patterns))
  {
    if (grepl(patterns[i], string))
    {
      result[i] <- TRUE
    }
  }
  
  return (result)
}, options=list(optimize=3))

addInteractions <- cmpfun(function(org_dataset, multilevel_factors, include_outcome=TRUE)
{
  if (include_outcome)
  {
    pred_mat <- org_dataset[, 2:ncol(org_dataset)]
  } else
  {
    pred_mat <- org_dataset
  }
  
  result_mat <- org_dataset
  org_names <- colnames(pred_mat)
  for (i_col in 1:(ncol(pred_mat)-1))
  {
    match_i <- calMatchVec(multilevel_factors, org_names[i_col])
    
    for (j_col in (i_col+1):ncol(pred_mat))
    {
      match_j <- calMatchVec(multilevel_factors, org_names[j_col])
      # grepl(org_names[i_col], multilevel_factors)
      if (all(match_i == match_j) & any(match_j)) {
        next
      }
      
      new_name <- paste(org_names[i_col], ":", org_names[j_col], sep="")
      result_mat[, new_name] <- pred_mat[, i_col] * pred_mat[, j_col]
    }
  }
  
  return (result_mat)
}, options=list(optimize=3))