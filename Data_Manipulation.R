# clear memory
rm(list = ls())

library(frequencyConnectedness)
library(parallel)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape)
library(purrr)
library(tidyverse)



list_files <- c("Y_EquityIndex.RData","BK_EquityIndex.RData","Y_Equity.RData","BK_Equity.RData","Y_DebtIndex.RData","BK_DebtIndex.RData","Y_Debt.RData","BK_Debt.RData")
list_variable <- c("EquityIndex","Equity","DebtIndex","Debt")
nominal_index <- read_csv(file = "RawFiles/NominalIndex.csv",na= "#N/A")
nominal_index$period <- as.Date(as.POSIXct(nominal_index$period,format='%d/%m/%Y'))
temp <- diff(log(nominal_index$nominal_index), lag=1)
nominal_index$log_index <- append(NA, temp)



i=1
j=1
df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(df) <- c("period","DY", "ST_Decomp", "MT_Decomp", "LT_Decomp","Country","ST_Spill","MT_Spill","LT_Spill")

for (var in list_variable){
  load(list_files[i])
  i=i+1
  load(list_files[i])
  i=i+1
  
  create_subset <- function (num, dataname,type){
    if (type==0){
      temp <- overall(dataname)
    }
    if (type==1) {
      temp <- overall(dataname)[[num]]
    }
    if (type==2){
      temp <- net(dataname)[[num]]
    }
    temp_df <- data.frame(temp)
    temp_new <- cbind(period = rownames(temp_df), temp_df)
    rownames(temp_new) <- 1:nrow(temp_new)
    if (type==2){
      temp_new <- temp_new %>% pivot_longer(!period, names_to = "country", values_to = "Spill")
    }
    return(temp_new)
  }
  
  
  overall_DY <- create_subset(0,sp,0)
  group_1_overall <- create_subset(1,spbk,1)
  group_2_overall <- create_subset(2,spbk,1)
  group_3_overall <- create_subset(3,spbk,1)
  net_1_overall <- create_subset(1,spbk,2)
  net_2_overall <- create_subset(2,spbk,2)
  net_3_overall <- create_subset(3,spbk,2)
  
  
  net_list <- list(net_1_overall,net_2_overall, net_3_overall)
  net_list <- net_list %>%  reduce(inner_join, by=c("period","country"))
  df_list <- list(overall_DY,group_1_overall,group_2_overall,group_3_overall, net_list)
  main <- df_list %>%  reduce(inner_join, by="period")
  colnames(main) <- c("period","DY", "ST_Decomp", "MT_Decomp", "LT_Decomp","Country","ST_Spill","MT_Spill","LT_Spill")
  main$period <- as.Date(main$period)
  main$type <- list_variable[j]
  j=j+1
  print("Done")
  df <- rbind(df,main)
 
}

df <- left_join(df,nominal_index, by="period")
df
write.csv(df, "Final.csv")

