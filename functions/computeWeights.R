library(compiler)

computeWeights_DiscreteClasses <- cmpfun(function(label_vec)
{
  itsLevels <- levels(factor(label_vec))
  counts <- vector()
  
  for (iLevel in 1:length(itsLevels))
  {
    sizeThisClass <- sum(label_vec == itsLevels[iLevel])
    counts <- c(counts, sizeThisClass)
  }
  
  weight_vec <- rep(0.0, length(label_vec))
  for (iLevel in 1:length(itsLevels))
  {
    weight_vec[which(label_vec == itsLevels[iLevel])] <- 1/(counts[iLevel]+1e-6) / (sum(1/counts) + +1e-6)
  }
  
  return (weight_vec)
}, options=list(optimize=3))

computeWeights <- cmpfun(function(label_vec)
{
  weight_vec <- computeWeights_DiscreteClasses(label_vec)
  
  return (weight_vec)
}, options=list(optimize=3))

# input must be 0/1 boolean
computeWeights_2domains <- cmpfun(function(y_d1, y_d2)
{
  n_total <- sum(length(y_d1), length(y_d2))
  
  n_pos_d1 <- sum(y_d1)
  n_neg_d1 <- length(y_d1) - n_pos_d1
  p_pos_d1 <- n_pos_d1 / n_total
  p_neg_d1 <- n_neg_d1 / n_total
  
  n_pos_d2 <- sum(y_d2)
  n_neg_d2 <- length(y_d2) - n_pos_d2
  p_pos_d2 <- n_pos_d2 / n_total
  p_neg_d2 <- n_neg_d2 / n_total
  
  
  
  sum_invs <- 1/p_pos_d1 + 1/p_neg_d1 + 1/p_pos_d1 + 1/p_neg_d1
  w_d1 <- rep(0, length(y_d1))
  w_d2 <- rep(0, length(y_d2))
  w_d1[which(y_d1==1)] <- 1/p_pos_d1 / sum_invs
  w_d1[which(y_d1==0)] <- 1/p_neg_d1 / sum_invs
  w_d2[which(y_d2==1)] <- 1/p_pos_d2 / sum_invs
  w_d2[which(y_d2==0)] <- 1/p_neg_d2 / sum_invs
  
  return (c(w_d1, w_d2))
}, options=list(optimize=3))