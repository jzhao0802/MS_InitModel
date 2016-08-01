rm(list=ls())

source("functions/main.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- c(n.grp=30, n.outcome=3, n.evlFolds=5)

main.arglist$kFoldsEval <- 5
main.arglist$kFoldsVal <- 5
main.arglist$n_repeats <- 1

main.arglist$wt <- c("0"=1, "1"=1)
main.arglist$ntree <- 100
main.arglist$mtry <- 8

main.arglist$data_dir <- "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.08.00/"
main.arglist$outcomeNamesAll <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                               "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")

main.arglist$outcomeNames <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                           "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")
# main.arglist$outcomeNames <- c("edssconf3")


# main.arglist$cohortNames <- c("BConti", "B2B", "B2Fir", "B2Sec")
main.arglist$cohortName <- c("Cmp")

main.arglist$idColName <- c("record_num")


main.arglist$dataFileSuffix <- '4Model.csv'

main.arglist$bTopVarsOnly <- F
main.arglist$nTopVars <- 10
main.arglist$initEnetDir <- 
  "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.08.00/"

main.arglist$newGrpVarsLst <- list(
  c("base variables"
    , c(""))
  
  c("Pre-baseline EDSS total score"
    , grep("^pre\\d_edss_score$", vars, value=T, ignore.case = T))
  
  , c("3 Month confirmation of progression of pre-baseline EDSS score"
      , grep("^pre\\d_edssconf3$", vars, value=T, ignore.case = T))
  
  , c("Last EDSS FS subscore 0-360 days pre-index"
      , grep("^pre\\d_edss_fs\\d$", vars, value=T, ignore.case = T))
  
  , c("Familial MS"
      , grep("^familial_MS$", vars, value=T, ignore.case = T))
  
  , c("Number of biological children"
      , grep("^num_children$", vars, value=T, ignore.case = T))
  
  , c("Objective severity reported"
      , grep("^relapse_pre\\dd_psev\\d$", vars, value=T, ignore.case = T))
  
  , c("Subjective severity reported"
      , grep("^relapse_pre\\d_ssev\\d$", vars, value=T, ignore.case = T))
  
  , c("Number of relapses treated"
      , grep("^relapse_pre\\d_trty$", vars, value=T, ignore.case = T))
  
  , c("Number of relapses not treated"
      , grep("^relapse_pre\\d_trtn$", vars, value=T, ignore.case = T))
  
  , c("Visit reasons "
      , grep("^pre\\d_visreas_\\w+$", vars, value=T, ignore.case = T))
  
  , c("Degree of disability recorded"
      , grep("^pre_\\d_\\w{0,1}deg_disab$", vars, value=T, ignore.case = T))
  
  , c("Whether or not the drug on a list of interest has been used"
      , grep("^pre\\ddrug[2-42]$", vars, value=T, ignore.case = T))
  
  , c("Cerebrospinal fluid examination"
      , grep("^pre\\d_csf_[cellcnt|olig|prot|path]$", vars, value=T, ignore.case = T))
  
  , c("Medication-related diagnostics"
      , grep("^pre\\d_med_[jcv|lsec|nab|vzv|acnat]$", vars, value=T, ignore.case = T))
  
  
)

global.seed <- 1
set.seed(global.seed)

main(main.arglist)
