library(glmnet)
library(pROC)
library(ROCR)
library(doParallel)
library(compiler)

source("functions/computeWeights.R")
source("functions/manualStratify.R")
source("functions/manualCV.R")
source("functions/mainloop_learn.R")
source("functions/mainloop_transferlearn.R")

#

getRanking <- cmpfun(function(coefficients)
{
  winners <- sort.int(abs(coefficients), decreasing=TRUE, index.return=TRUE)$ix
  ranking <- rep(-1, length(winners))
  ranking[winners] <- 1:length(winners)
  ranking[which(coefficients == 0)] <- length(winners)
  
  return (ranking)
}, options=list(optimize=3))

AddSuffix2SetOfStrings <- cmpfun(function(suffix, str_vec)
{
  str_vec <- lapply(str_vec, paste, arg1="_", arg2=suffix, sep="")
  return (str_vec)
}, options=list(optimize=3))

#

main <- function(arglist)
{
  # input arguments
  bTransferLearn <- arglist$bTransferLearn
  bManualCV <- arglist$bManualCV
  
  bParallel <- arglist$bParallel
  nCores2Use <- arglist$nCores2Use
  
  kFoldsEval <- arglist$kFoldsEval
  kFoldsVal <- arglist$kFoldsVal
  alphaVals <- arglist$alphaVals
  log_lambda_seq <- arglist$log_lambda_seq
  bClassWeights <- arglist$bClassWeights
  n_repeats <- arglist$n_repeats
  
  n_alphas <- length(alphaVals)
  lambda_seq <- exp(log_lambda_seq)
  
  outcomeNames <- arglist$outcomes
  
  data_dir <- arglist$data_dir
  if (bTransferLearn)
  {
    cohort_names <- arglist$cohort_names
    outcome_names <- arglist$outcome_names
  } else
  {
    data_names <- arglist$data_names
  }
  
  idColName <- arglist$idColName
  
  
  # 
  if (bParallel)
  {
    cl <- makeCluster(nCores2Use)
    registerDoParallel(cl, cores = nCores2Use)
  }
  
  # 
  
  ptm <- proc.time()
  
  timeStamp <- as.character(Sys.time())
  timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
  resultDir <- paste("./Results/", timeStamp, "/", sep = '')
  dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
#   # check the seed working
#   set.seed(seed=seed)
#   
# folds4repeats <- lapply(1:n_repeats, function(i_repeat){
#     cat(paste0(data_names[iCohort], ", "))
#     
#     dataset <- read.csv(paste0(data_dir, data_names[iCohort],"4model.csv"), 
#                         header=TRUE, sep=",", check.names=FALSE)
#     
#     resultDirPerCohort <- paste0(resultDir, data_names[iCohort], "/")
#     dir.create(resultDirPerCohort, showWarnings = TRUE, recursive = TRUE, mode = "0777")
#     
#     cat(paste0(outcomeName, "\n"))
#     
#     resultDirPerOutcome <- paste0(resultDirPerCohort, outcomeName, "/")
#     dir.create(resultDirPerOutcome, showWarnings = TRUE, recursive = TRUE, mode = "0777")
#     
#     
#     y <- dataset[, outcomeName]
#     
#     X <- dplyr::select(dataset, -one_of(c(outcomeNames, idColName)))
#     
# 
#     X <- data.matrix(X)
#     
# 
#     n_data = nrow(X)
#     
#     
#     # stratification for evaluation
#     
#     # folds <- manualStratify(y, kFoldsEval, global.seed)
#     ids <- 1:k_folds
# #     set.seed(seed=seed)
#     cat(sample(1:10, 5), '\n\n')
#     ids_every_pos <- sample(rep(ids, length.out=sum(y==1)))
#     ids_every_neg <- sample(rep(ids, length.out=sum(y!=1)))
#     
#     ids_every_datum <- rep(-1, length(y))
#     ids_every_datum[which(y==1)] <- ids_every_pos
#     ids_every_datum[which(y!=1)] <- ids_every_neg
#     return(ids_every_datum)
#     
#   })
# folds4repeatsDf <- ldply(folds4repeats, quickdf)
#  sum(apply(folds4repeatsDf, 2, function(x)length(unique(x))) >1)
#   # 
  for (i_repeat in 1:n_repeats)
  {
    resultDir_thisrepeat <- paste(resultDir, i_repeat, "/", sep = '')
    dir.create(resultDir_thisrepeat, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    
    if (bTransferLearn)
      mainloop_transferlearn(bParallel, kFoldsEval, kFoldsVal, alphaVals, 
                             log_lambda_seq, bClassWeights, 
                             n_alphas, lambda_seq, data_dir, cohort_names, 
                             outcome_names, resultDir_thisrepeat)
    else
      mainloop_learn(bParallel, bManualCV, kFoldsEval, kFoldsVal, alphaVals, 
                     log_lambda_seq, bClassWeights, 
                     n_alphas, lambda_seq, data_dir, data_names, outcomeNames,
                     idColName, 
                     resultDir_thisrepeat)
  }
  
  
  
  runtime <- proc.time() - ptm
  
  cat(paste("Time elapsed:", round(runtime[3],1), "seconds."))
  
  if (bParallel)
  {
    stopCluster(cl)
  }
}