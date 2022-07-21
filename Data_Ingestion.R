# clear memory
rm(list = ls())

library(frequencyConnectedness)
library(parallel)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape)

##list_file <- c("DebtFlows/DebtFlows_Short_Without_USD_EM_Index.csv", "DebtFlows/DebtFlows_Short_With_USD_EM_Index.csv","EquityFlows/EquityFlows_Short_Without_USD_EM_Index.csv","EquityFlows/EquityFlows_Short_Without_USD_EM_Index.csv")
###list_title <- c("DebtWithIndex")

data <- read.csv("DebtFlows/DebtFlows_Short_Without_USD_EM_Index.csv",header=T,sep=",",stringsAsFactors = FALSE)
data.z <- read.zoo(data, format = "%d/%m/%Y")
nCores <- detectCores() - 1
cl <- makeCluster(nCores)
bounds <- c(pi+0.00001, pi/5, pi/20, 0)

# Get the rolling window estimates
params_est = list(p=2, type = "const")
clusterExport(cl, c("params_est", "data.z"))
sp <- spilloverRollingDY12(data.z, n.ahead = 100, "VAR", params_est = params_est, no.corr = T, window = 100, cluster=cl)
Overallsp <- plotOverall(sp)

clusterExport(cl, c("params_est", "data.z","bounds"))
spbk <- spilloverRollingBK12(data.z, n.ahead = 100, no.corr = T, "VAR", partition=bounds, params_est = params_est, window = 100, cluster=cl)
Overallspbk <- plotOverall(spbk)
stopCluster(cl)
Netspbk <- plotNet(spbk)


save(sp, file = "Y_Debt.RData")
save(spbk, file = "BK_Debt.RData")


### Yassier's manipulation


data <- read.csv("DebtFlows/DebtFlows_Short_With_USD_EM_Index.csv",header=T,sep=",",stringsAsFactors = FALSE)
data.z <- read.zoo(data, format = "%d/%m/%Y")
nCores <- detectCores() - 1
cl <- makeCluster(nCores)
bounds <- c(pi+0.00001, pi/5, pi/20, 0)

# Get the rolling window estimates
params_est = list(p=2, type = "const")
clusterExport(cl, c("params_est", "data.z"))
sp <- spilloverRollingDY12(data.z, n.ahead = 100, "VAR", params_est = params_est, no.corr = T, window = 100, cluster=cl)
Overallsp <- plotOverall(sp)

clusterExport(cl, c("params_est", "data.z","bounds"))
spbk <- spilloverRollingBK12(data.z, n.ahead = 100, no.corr = T, "VAR", partition=bounds, params_est = params_est, window = 100, cluster=cl)
Overallspbk <- plotOverall(spbk)
stopCluster(cl)
Netspbk <- plotNet(spbk)


save(sp, file = "Y_DebtIndex.RData")
save(spbk, file = "BK_DebtIndex.RData")
