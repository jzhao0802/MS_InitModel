rm(list=ls())

source("functions/main_RF.R")

# input arguments for the main function

main.arglist <- list()
main.arglist$bParallel <- F
main.arglist$nCores2Use <- c(n.outcome=1, n.grp=7, n.evlFolds=5)

main.arglist$kFoldsEval <- 5

main.arglist$wt <- c("0"=1, "1"=1)
main.arglist$ntree <- 100
main.arglist$mtry <- 8

main.arglist$data_dir <- "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.08.00/"
main.arglist$outcomeNamesAll <- c("relapse_fu_any_01", "edssprog", "edssconf3",
                               "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")

# main.arglist$outcomeNames <- c("relapse_fu_any_01", "edssprog", "edssconf3")
main.arglist$outcomeNames <- c("edssconf3")


# main.arglist$cohortNames <- c("BConti", "B2B", "B2Fir", "B2Sec")
main.arglist$cohortName <- c("Cmp")

main.arglist$idColName <- c("record_num")


main.arglist$dataFileSuffix <- '4Model.csv'


raw_data <- read.table("F:/Jie/MS/01_Data/MS_decsupp_analset_20160701.csv"
                       , sep=','
                       , header = T
                       , stringsAsFactors = T)
vars <- names(raw_data)
# 
# Qc
# QC <- lapply(main.arglist$newGrpVarsLst[-1], function(vs){
#   dt <- raw_data[, vs[-1]]
#   cls <- ifelse(ncol(dt)>1, unique(apply(dt, 2, class)), class(dt))
#   return(cls)
# })
# names(QC) <- unlist(lapply(main.arglist$newGrpVarsLst[-1], function(x)x[1]))

raw_data$lookback_days <- as.numeric(as.Date(as.character(raw_data$idx_dt), format="%Y/%m/%d")-
  as.Date(as.character(raw_data$firstdt), format="%Y/%m/%d"))

main.arglist$newGrpVarsLst <- list(
  c("base variables"
    , c(""))
  
  , c("lookback days"
    , c('lookback_days'))
  
  , c("init_symptom"
    , grep("^init_\\w+$", vars, value=T, ignore.case = T))
  
  , c("adherence variable"
      , grep("^pre\\d_adh_[given|needsaid|no]", vars, value=T, ignore.case = T))
  
  , c("participation variable"
      , grep("^pre\\d_part_[completely|notpossible|restricted]", vars, value=T, ignore.case = T))
  
  , c("Maximum level of care recorded"
      , grep("^pre\\d_loc$", vars, value=T, ignore.case = T))
  
  , c("Last EDSS FS subscore 0-360 days pre-index"
      , grep("^pre\\d_edss_fs\\d$", vars, value=T, ignore.case = T))
  
  , c("Familial MS"
      , grep("^familial_MS$", vars, value=T, ignore.case = T))
  
  , c("Number of biological children"
      , grep("^num_children$", vars, value=T, ignore.case = T))
  
  , c("Objective severity reported"
      , grep("^relapse_pre\\d_psev\\d$", vars, value=T, ignore.case = T))
  
  , c("Subjective severity reported"
      , grep("^relapse_pre\\d_ssev\\d$", vars, value=T, ignore.case = T))
  
  , c("Number of relapses treated"
      , grep("^relapse_pre\\d_trty$", vars, value=T, ignore.case = T))
  
  , c("Number of relapses not treated"
      , grep("^relapse_pre\\d_trtn$", vars, value=T, ignore.case = T))
  
  , c("Visit reasons"
      , grep("^pre\\d_visreas_\\w+$", vars, value=T, ignore.case = T))
  
  , c("Degree of disability recorded"
      , grep("^pre\\d_\\w{0,1}deg_disab$", vars, value=T, ignore.case = T))
  
  , c("Whether or not the drug on a list of interest has been used"
      , grep("^pre\\d_drug\\d+$", vars, value=T, ignore.case = T))
  
  , c("Cerebrospinal fluid examination"
      , grep("^pre\\d_csf_[cellcnt|olig|prot|path]", vars, value=T, ignore.case = T))
  
  , c("Medication-related diagnostics"
      , grep("^pre\\d_med_[jcv|lsec|nab|vzv|acnat]", vars, value=T, ignore.case = T))
  
  
)

# saveRDS(main.arglist$newGrpVarsLst
#         , paste0("F:/Jie/MS/01_Data/newGrpVarsLst.RDS"))

global.seed <- 1
set.seed(global.seed)

main(main.arglist)
