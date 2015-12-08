rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- detectCores() - 1

main.arglist$kFoldsEval <- 3
main.arglist$kFoldsVal <- 3
main.arglist$alphaVals <- c(0)
main.arglist$log_lambda_seq <- seq(log(1e-4),log(1e4),length.out=3)
main.arglist$bClassWeights <- 0
main.arglist$n_repeats <- 1

main.arglist$data_dir <- "C:/Work/Projects/MultipleSclerosis/Results/2015-10-06/2015-10-06 17.22.37/"
main.arglist$cohort_names <- c("continue", "B2F")
main.arglist$outcome_names <- c("relapse_fu_any_01")

main.arglist$bTransferLearn <- T

main(main.arglist)