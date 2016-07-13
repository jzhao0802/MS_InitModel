rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- detectCores() - 1

main.arglist$kFoldsEval <- 5
main.arglist$kFoldsVal <- 5
main.arglist$alphaVals <- seq(0,1,length.out=20)
main.arglist$log_lambda_seq <- seq(log(1e-4),log(1e4),length.out=100)
main.arglist$bClassWeights <- T
main.arglist$n_repeats <- 1

# main.arglist$data_dir <- "F:\\Lichao\\work\\Projects\\MultipleSclerosis\\Results\\2016-07-11\\2016-07-11 20.07.54\\mergeCateg\\"
main.arglist$data_dir <- "F:\\Jie\\MS\\02_Code\\MS_Cohort4Model\\modelDt4InvestNewVarGroups\\2016-07-13 09.00.02\\"
main.arglist$outcomes <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                           "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")
# main.arglist$outcomes <- c("relapse_fu_any_01")

main.arglist$data_names <- c("Cmp")

main.arglist$idColName <- c("record_num")

main.arglist$bTransferLearn <- F
main.arglist$bManualCV <- F

global.seed <- 1

bInvesNewVarsGroups <- T

# newVars <- "Sym"

# newVars <- "Relapse_pre0_psev3"

newVars <- "Pre0_qdeg_disab"

set.seed(global.seed)

main(main.arglist)
