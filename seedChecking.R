folds4compare <- list()

for(runId in 1:2){
  # check the seed working
  set.seed(seed=seed)
  
  
  
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
  
  
  folds4repeats <- lapply(1:n_repeats, function(i_repeat){
    cat(paste0(data_names[iCohort], ", "))
    
    dataset <- read.csv(paste0(data_dir, data_names[iCohort],"4model.csv"), 
                        header=TRUE, sep=",", check.names=FALSE)
    
    resultDirPerCohort <- paste0(resultDir, data_names[iCohort], "/")
    dir.create(resultDirPerCohort, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    
    cat(paste0(outcomeName, "\n"))
    
    resultDirPerOutcome <- paste0(resultDirPerCohort, outcomeName, "/")
    dir.create(resultDirPerOutcome, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    
    
    y <- dataset[, outcomeName]
    
    X <- dplyr::select(dataset, -one_of(c(outcomeNames, idColName)))
    
    
    X <- data.matrix(X)
    
    
    n_data = nrow(X)
    
    
    # stratification for evaluation
    
    # folds <- manualStratify(y, kFoldsEval, global.seed)
    ids <- 1:k_folds
    #     set.seed(seed=seed)
    cat(sample(1:10, 5), '\n\n')
    ids_every_pos <- sample(rep(ids, length.out=sum(y==1)))
    ids_every_neg <- sample(rep(ids, length.out=sum(y!=1)))
    
    ids_every_datum <- rep(-1, length(y))
    ids_every_datum[which(y==1)] <- ids_every_pos
    ids_every_datum[which(y!=1)] <- ids_every_neg
    return(ids_every_datum)
    
  })
  folds4repeatsDf <- ldply(folds4repeats, quickdf)
  diffRows <- sum(apply(folds4repeatsDf, 2, function(x)length(unique(x))) >1)
  cat(diffRows)
  folds4compare[[runId]] <- apply(folds4repeatsDf, 2, mean)
  #
}

for(itest in 1:2){
  for(jtest in 1:2){
    set.seed(1)
    smp <- sample(1:10, 5)
    cat(smp, '\n\n')
  }
}
