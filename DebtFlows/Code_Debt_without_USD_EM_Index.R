# clear memory
rm(list = ls())

# close graphs
graphics.off()

library(frequencyConnectedness)
library(parallel)
library(zoo)
data <- read.csv("DebtFlows/DebtFlows_Short_Without_USD_EM_Index.csv",header=T,sep=",",stringsAsFactors = FALSE)
data.z <- read.zoo(data, format = "%d/%m/%Y")
nCores <- detectCores() - 1
cl <- makeCluster(nCores)
bounds <- c(pi+0.00001, pi/5, pi/20, 0)
# Get the rolling window estimates
params_est = list(p = 2, type = "const")
clusterExport(cl, c("params_est", "data.z"))
sp <- spilloverRollingDY12(data.z, n.ahead = 100, no.corr = T, "VAR", params_est = params_est, window = 100, cluster=cl)
Overallsp <- plotOverall(sp)
clusterExport(cl, c("params_est", "data.z","bounds"))
spbk <- spilloverRollingBK12(data.z, n.ahead = 100, no.corr = T, "VAR", partition=bounds, params_est = params_est, window = 100, cluster=cl)
Overallspbk <- plotOverall(spbk)
stopCluster(cl)



### Yassier's manipulation

create_subset <- function (num, dataname){
  temp <- overall(dataname)[[num]]
  temp_df <- data.frame(temp)
  temp_new <- cbind(period = rownames(temp_df), temp_df)
  rownames(temp_new) <- 1:nrow(temp_new)
  return(temp_new)
  
}

overall_zoo <- overall(sp)
overall_df <- data.frame(overall_zoo)
overall_main <- cbind(period = rownames(overall_df), overall_df)
rownames(overall_main) <- 1:nrow(overall_main)


group_1_overall <- create_subset(1,spbk)
group_2_overall <- create_subset(2,spbk)
group_3_overall <- create_subset(3,spbk)


overall_merged_main <- merge(group_1_overall,group_2_overall,by="period")
overall_merged_main <- merge(overall_merged_main,group_3_overall,by="period")
overall_merged_main <- merge(overall_merged_main,overall_main,by="period")

head(overall_merged_main)
colnames(overall_merged_main) <- c("period","Short Term", "Medium Term", "Long Term", "Total")
  
temp_2 <- melt(overall_merged_main, id="period")

string <- "2014-07-01 08:00:00"
temp_2$new_period <- as.POSIXct(temp$period, format="%Y-%m-%d %H:%M:%S", tz="UTC")
head(temp_2)
write.csv(temp_2, "WithoutIndex.csv")

figure_2 <- ggplot(temp_2, aes(x=period, y=value, colour=variable)) + 
  geom_line(position='stack',size=2)

figure_2
