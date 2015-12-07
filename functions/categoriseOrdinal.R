library(compiler)

categoriseOrdinal <- cmpfun(function(ordinal_var, org_name, thresholds, bin_names, ref_bin_name)
{
  # check
  
  if (length(thresholds) != (length(bin_names)-1)) {
    cat("\nERROR! Inconsistent number of thresholds and bin_names. Function 
        'categoriseOrdinal' aborted. \n")
    
    return
  }
  
  if (length(thresholds) == 0) {
    if (any(is.na(ordinal_var))) {
      result_var <- rep(0, length(ordinal_var))
      result_var[which(is.na(ordinal_var))] <- 1
      return (result_var)
    } else 
    {
      cat("\nERROR! There's only one category and no NA. Function 
        'categoriseOrdinal' aborted. \n")
      return
    }
  }
  
  # 
  
  b_na <- FALSE
  if (any(is.na(ordinal_var))) {
    n_bins <- length(bin_names) + 1
    b_na <- TRUE
  } else {
    n_bins <- length(bin_names)
  }
  
  matThisVar <- data.frame(mat.or.vec(length(ordinal_var), n_bins))
  colnames(matThisVar) <- rep("", n_bins)
  
  # the contrastmatrix
  
  matContr <- diag(n_bins)
  
  # 
  
  for (i_bin in 1:n_bins)
  {
    if ((i_bin == n_bins) & b_na) {
      ids <- which(is.na(ordinal_var))
      colnames(matThisVar)[i_bin] <- paste(org_name, "__NA", sep="")
    } else {
      
      if (i_bin == 1) {
        ids <- which(ordinal_var < thresholds[i_bin])
      } else if (i_bin == length(bin_names)) {
        ids <- which(ordinal_var >= thresholds[i_bin-1])
      } else {
        ids <- which((ordinal_var >= thresholds[i_bin-1]) &
                       ordinal_var < thresholds[i_bin])
      }
      colnames(matThisVar)[i_bin] <- paste(org_name, "__", bin_names[i_bin], sep="")
    }
    matThisVar[ids, ] <- t(replicate(length(ids),matContr[i_bin, ]))
  } 
  
  ref_var_name <- paste(org_name, "__", ref_bin_name, sep="")
  matThisVar[, colnames(matThisVar) == ref_var_name] <- NULL
  
  return (matThisVar)
}, options=list(optimize=3))