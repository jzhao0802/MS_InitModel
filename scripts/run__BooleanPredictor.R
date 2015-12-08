rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F

main.arglist$kFoldsEval <- 3
main.arglist$kFoldsVal <- 3
main.arglist$alphaVals <- c(0)
main.arglist$log_lambda_seq <- seq(log(1e-4),log(1e4),length.out=3)
main.arglist$bClassWeights <- 0
main.arglist$n_repeats <- 1

main.arglist$data_dir <- "c:/Work/Projects/MultipleSclerosis/Results/2015-10-17/2015-10-17 19.53.29/"
# main.arglist$data_names <- c("continue_edssprog",
#                              "continue_edssconf3",
#                              "continue_relapse_fu_any_01",
#                              "continue_confrelapse",
#                              "continue_progrelapse",
#                              "B2B_edssprog",
#                              "B2B_relapse_fu_any_01",
#                              "B2B_progrelapse",
#                              "B2F_edssprog",
#                              "B2F_relapse_fu_any_01",
#                              "B2F_progrelapse",
#                              "B2S_edssprog",
#                              "B2S_edssconf3",
#                              "B2S_relapse_fu_any_01",
#                              "B2S_confrelapse",
#                              "B2S_progrelapse")
main.arglist$data_names <- c("B2F_relapse_fu_any_01")

main.arglist$bTransferLearn <- F
main.arglist$bManualCV <- F

main(main.arglist)