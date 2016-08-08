rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- detectCores() - 1

main.arglist$kFoldsEval <- 5
main.arglist$kFoldsVal <- 5
main.arglist$alphaVals <- seq(0,1,length.out=21)
main.arglist$log_lambda_seq <- c(seq(log(1e-20),log(1e4),length.out=100))
main.arglist$bClassWeights <- T
main.arglist$n_repeats <- 1

main.arglist$data_dir <- "F:/Jie/MS/03_Result/2016-08-08/2016-08-08 05.39.00/"
main.arglist$outcomeNames <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                           "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")
# main.arglist$outcomeNames <- c("relapse_fu_any_01", "edssconf3")
main.arglist$AllOutcomes4Remove <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                           "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")

# main.arglist$cohortNames <- c("BConti", "B2B", "B2Fir", "B2Sec")
main.arglist$cohortNames <- c("BConti", "B2Fir", "B2B", 'B2Sec')

main.arglist$idColName <- c("record_num")

main.arglist$bTransferLearn <- F
main.arglist$bManualCV <- F

main.arglist$dataFileSuffix <- '4Model.csv'

main.arglist$bTopVarsOnly <- F
main.arglist$nTopVars <- 10
main.arglist$initEnetDir <- 
  "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.15.57/"

global.seed <- 1
set.seed(global.seed)

Btest=F

if(Btest){
  main.arglist$log_lambda_seq <- main.arglist$log_lambda_seq[1:5]
  main.arglist$alphaVals <- main.arglist$alphaVals[1:2]
}
main(main.arglist)
