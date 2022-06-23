library(frequencyConnectedness)
library(parallel)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape)

data_1 <- read.csv("WithIndex.csv")
data_2 <- read.csv("WithoutIndex.csv")


head(data_2)
main <- rbind(data_1, data_2)
head(main)

main$time <- as.POSIXct(main$period, format="%Y-%m-%d %H:%M:%S", tz="UTC")

figure_4 <- ggplot(main %>% filter(variable %in% c("Long Term","Short Term", "Medium Term")), aes(x=time, y=value, colour=variable)) + 
  geom_line(size=1)

figure_4


head(main)


str(main)
