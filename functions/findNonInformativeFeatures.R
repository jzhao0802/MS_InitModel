library(plyr)

# find any category variables whose dominant category is more than 99%

findNonInformativeFeatures <- function(data_mat)
{
  noninformative_features <- c()
  for (i_feature in 1:ncol(data_mat))
  {
    if (substr(colnames(data_mat)[i_feature], 1, 1) == "f")
    {
      counts <- c(0,0)
      counts[2] <- sum(data_mat[, i_feature])
      counts[1] <- nrow(data_mat) - counts[2]
      dominant_ratio <- max(counts) / nrow(data_mat)
      if (dominant_ratio > 0.99)
        noninformative_features <- c(noninformative_features, colnames(data_mat)[i_feature])
    }
  }
  
  return (noninformative_features)
}