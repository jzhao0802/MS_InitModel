library(pROC)
library(ROCR)
library(compiler)
library(snowfall)

source("functions/computeWeights.R")
source("functions/funs_RF.R")
source("functions/main_RF.R")

#


main <- function(arglist)
{
  # input arguments
  nCores2Use <- arglist$nCores2Use
  nCore2Use4Outcome <- nCores2Use[1]
  nCore2Use4Grp <- nCores2Use[2]
  nCore2Use4Eval <- nCores2Use[3]
  
  
  kFoldsEval <- arglist$kFoldsEval

  outcomeNames <- arglist$outcomeNames
  outcomeNamesAll <- arglist$outcomeNamesAll
  data_dir <- arglist$data_dir
  
  cohort <- arglist$cohortName

  dataFileSuffix <- arglist$dataFileSuffix 
  
  newGrpVarsLst <- arglist$newGrpVarsLst

  ptm <- proc.time()
  
  timeStamp <- as.character(Sys.time())
  timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
  resultDir <- paste("./Results/", timeStamp, "/", sep = '')
  dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  cat(paste0(cohort, ", "))
  
  data <- read.csv(paste0(data_dir, cohort, dataFileSuffix), 
                   header=TRUE, sep=",", check.names=FALSE)
  
  resultDirPerCohort <- paste0(resultDir, cohort, "/")
  dir.create(resultDirPerCohort, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  
  sfInit(parallel=TRUE, cpus=nCore2Use4Outcome, type='SOCK')
  sfSource("functions/funs_RF.R")
  sfSource("functions/manualStratify.R")
  sfExport('nCore2Use4Grp', 'nCore2Use4Eval', 'kFoldsEval', 'outcomeNames',
           'outcomeNamesAll', 'cohort', 'data', 'newGrpVarsLst')
  sfExport('createCurve', 'grid_search_v2')
  sfClusterEval(library("glmnet"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("dplyr"))
  temp <- sfClusterApplyLB(outcomeNames, runRF_outcome
                           , data, newGrpVarsLst, cohort, resultDir)
  sfStop()
  outcomes_grp_auc_tr_ts <- ldply(temp, rbind)
  names(outcomes_grp_auc_tr_ts) <- c("Outcome", 'Group', "AUC_on_training", "AUC_on_test")
  
  write.table(outcomes_grp_auc_tr_ts
              , paste0(resultDirPerCohort, 'outcomes_grp_auc_tr_ts.csv')
              , sep=','
              , row.names = F)

}
