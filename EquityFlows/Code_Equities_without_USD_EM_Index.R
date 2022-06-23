# clear memory
rm(list = ls())

# close graphs
graphics.off()

library(frequencyConnectedness)
library(parallel)
library(zoo)
setwd("C:/Users/victor/OneDrive - The SEACEN Centre/Capital Flows Project SEACEN Centre/FrequencyConnectedness")
data <- read.csv("EquityFlows_Short_without_USD_EM_Index.csv",header=T,sep=",",stringsAsFactors = FALSE)
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
