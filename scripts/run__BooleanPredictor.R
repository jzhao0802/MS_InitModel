rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- detectCores() - 1

main.arglist$kFoldsEval <- 5
main.arglist$kFoldsVal <- 5
main.arglist$alphaVals <- seq(0,1,length.out=21)
main.arglist$log_lambda_seq <- seq(log(1e-4),log(1e4),length.out=100)
main.arglist$bClassWeights <- T
main.arglist$n_repeats <- 1

main.arglist$data_dir <- "F:/Lichao/work/Projects/MultipleSclerosis/Results/2016-07-12/2016-07-12 15.43.48/"
main.arglist$outcomeNames <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                           "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")
# main.arglist$outcomes <- c("relapse_fu_any_01")

main.arglist$cohortNames <- c("Cmp")

main.arglist$idColName <- c("record_num")

main.arglist$bTransferLearn <- F
main.arglist$bManualCV <- F

main.arglist$dataFileSuffix <- '4Model_VarChanged.csv'

main.arglist$bTopVarsOnly <- T
main.arglist$nTopVars <- 10
main.arglist$initEnetDir <- 
  "F:/Lichao/work/Projects/MultipleSclerosis/Results/2016-07-14/2016-07-14 12.30.14/"

global.seed <- 1
set.seed(global.seed)

main(main.arglist)
