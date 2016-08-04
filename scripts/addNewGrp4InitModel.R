# for the three basse outcomes, add the selected group variables from  RF and create the new model data which
#  to put into InitModel to fit the elastic net.
rm(list=ls())
library(dplyr)
library(plyr)

data_dir <- "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.08.00/"
cohort <- 'Cmp'
dataFileSuffix <- '4Model.csv'
data <- read.csv(paste0(data_dir, cohort, dataFileSuffix), 
                 header=TRUE, sep=",", check.names=FALSE)

raw_data <- read.table("F:/Jie/MS/01_Data/MS_decsupp_analset_20160701.csv"
                       , sep=','
                       , header = T
                       , stringsAsFactors = T)

vars <- names(raw_data)
initVars <- grep('^init_', vars, value = T)

dt_init_symptom <- raw_data[data$record_num, initVars]

dt_init_symptom[is.na(dt_init_symptom)] <- 0

# qdegDegVars <- grep("^pre\\d_\\w{0,1}deg_disab$", vars, value=T, ignore.case = T)

drugListVars <- grep("^pre\\d_drug\\d+$", vars, value=T, ignore.case = T)

edssFsVars <-  grep("^pre\\d_edss_fs\\d$", vars, value=T, ignore.case = T)

# dt_qdegDeg <- raw_data[data$record_num, qdegDegVars]
dt_EdssFs <- raw_data[data$record_num, edssFsVars]

# dt_addQdegDegInitSymp <- dt_qdegDeg %>%
#   mutate(
#     pre0_qdeg_disab__0_1 = ifelse(is.na(pre0_qdeg_disab), 0, pre0_qdeg_disab %in% c(0, 1)),
#     pre0_qdeg_disab__2_4 = ifelse(is.na(pre0_qdeg_disab), 0,  between(pre0_qdeg_disab, 2, 4)),
#     pre1_qdeg_disab__0_1 = ifelse(is.na(pre1_qdeg_disab), 0, pre1_qdeg_disab %in% c(0, 1)),
#     pre1_qdeg_disab__2_4 = ifelse(is.na(pre1_qdeg_disab), 0,  between(pre1_qdeg_disab, 2, 4)),
#     pre2_qdeg_disab__0_1 = ifelse(is.na(pre2_qdeg_disab), 0, pre2_qdeg_disab %in% c(0, 1)),
#     pre2_qdeg_disab__2_4 = ifelse(is.na(pre2_qdeg_disab), 0,  between(pre2_qdeg_disab, 2, 4)),
#     pre3_qdeg_disab__0_1 = ifelse(is.na(pre3_qdeg_disab), 0, pre3_qdeg_disab %in% c(0, 1)),
#     pre3_qdeg_disab__2_4 = ifelse(is.na(pre3_qdeg_disab), 0,  between(pre3_qdeg_disab, 2, 4)),
#     
#     pre0_deg_disab__0_50 = ifelse(is.na(pre0_deg_disab), 0, between(pre0_deg_disab, 0, 50)),
#     pre0_deg_disab__60_100 = ifelse(is.na(pre0_deg_disab), 0,  between(pre0_deg_disab, 60, 100)),
#     pre1_deg_disab__0_50 = ifelse(is.na(pre1_deg_disab), 0, between(pre1_deg_disab, 0, 50)),
#     pre1_deg_disab__60_100 = ifelse(is.na(pre1_deg_disab), 0,  between(pre1_deg_disab, 60, 100)),
#     pre2_deg_disab__0_50 = ifelse(is.na(pre2_deg_disab), 0, between(pre2_deg_disab, 0, 50)),
#     pre2_deg_disab__60_100 = ifelse(is.na(pre2_deg_disab), 0,  between(pre2_deg_disab, 60, 100)),
#     pre3_deg_disab__0_50 = ifelse(is.na(pre3_deg_disab), 0, between(pre3_deg_disab, 0, 50)),
#     pre3_deg_disab__60_100 = ifelse(is.na(pre3_deg_disab), 0,  between(pre3_deg_disab, 60, 100))
#     
#   ) %>%
#   select(-one_of(qdegDegVars)) %>%
#   bind_cols(dt_init_symptom) %>%
#   bind_cols(data)

# generate the data for EDSS FS subscore
getBinary4EdssFs <- function(v){
  vct <- raw_data[data$record_num, v]
  v_0 <- paste0(v, '__0')
  v_other <- paste0(v, '__Gt0')
  eval(parse(text=paste0(v_0, " <- ifelse(is.na(vct), 0, vct==0)")))
  eval(parse(text=paste0(v_other, " <- ifelse(is.na(vct), 0, vct>0)")))
  eval(parse(text=paste0("dt <- data.frame(", v_0, "=", v_0, ", ", v_other, "=", v_other, ")")))
  return(dt)
}

getBinary4AllEdssFs <- function(vars){
  i <- 1
  for(v in vars){
    if(i==1){
      df <- getBinary4EdssFs(v)
    }else{
      df <- cbind(df, getBinary4EdssFs(v))
    }
    i <- i+1
  }
  return(df)
}
dt_edssFs <- getBinary4AllEdssFs(edssFsVars)

# QC
tableLst <- lapply(dt_edssFs, table)
sort(unlist(lapply(tableLst, min)))[1:10] #56

# pre drug list
table(unlist(lapply(raw_data[data$record_num, drugListVars], function(x)any(is.na(x)))))
# there is no na for drug list variables
dt_drugList <- raw_data[data$record_num, drugListVars]

dt_addInitSymEdssFsDrugList <- cbind(dt_init_symptom, dt_edssFs, dt_drugList, data)
# unlist(lapply(dt_qdegDeg_dummy, sum))
timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

write.table(dt_addInitSymEdssFsDrugList
            , paste0(resultDir, cohort, dataFileSuffix)
            , sep=','
            , row.names = F)



