rm(list=ls())

library(frequencyConnectedness)
library(parallel)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)


main <- read.csv("Final.csv")

mytheme = list(
  theme_classic()+
    theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_blank(),legend.position= c(0.7,0.9), legend.box = "horizontal", strip.text = element_text(face="bold", size=9),
          axis.text=element_text(face="bold", size=7),axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)

# Yilmaz Chart 1 Debt vs Debt USD, log differebce of broad index

plot_data <- main %>% filter(type %in% c("DebtIndex","Debt")) %>%  select(period,DY,type)

plot_data %>%  ggplot(aes(x=period, y=DY)) +  geom_line(aes(colour=type)) + mytheme


# Yilmaz Chart 2 Equity vs Equity USD

# Decomp Chart 3 Debt with Fre

# DEcomp Chart 4 Equity with Fre

# Net spillover with/without USD 


plot_data <- main %>% filter(type %in% c("DebtIndex")) 
plot_data
important_date <- data.frame(event=c("2nd 2015 ECB rate hikes", "due to Eurozone crisis", "China's PMI decelerated","Covid-19 outbreak", "Ukraine-Russia tension", "Brexit"),
                             date = c(as.Date("2011-07-01"),as.Date("2011-07-01"),as.Date("2015-01-01"),as.Date("2020-01-01"), as.Date("2022-01-01"),as.Date("2018-01-01")))
figure_1 <- plot_data %>% 
  ggplot(aes(x=as.Date(period), y=value)) + 
  geom_line(aes(colour=variable)) +
  ggtitle("Total Debt Connectedness") +
  scale_x_date(date_breaks = "6 month",date_labels = "%b%y") +
  annotate("text", size=3,x = important_date$date, 
           y = c(24,23.5, 25,30,30,30), label = important_date$event, hjust=0.5)+
  annotate("segment", x = important_date$date, xend = important_date$date,
           y = c(13,13, 15,12,30,30), yend = c(23,23, 24,29,29,29),
           colour = "black",arrow = arrow(length = unit(.2,"cm"))) 
figure_1

mytheme = list(
  theme_classic()+
    theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_blank(),legend.position= c(0.7,0.9), legend.box = "horizontal", strip.text = element_text(face="bold", size=9),
          axis.text=element_text(face="bold", size=7),axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)



figure_1 <- figure_1 +  mytheme

figure_1

png("DebtFlows/Overall_WithoutIndex.png")
print(figure_1)
dev.off()


#country "Indonesia.1""India""Thailand""South_Africa" "Hungary""Mexico""Poland"    

## Chart 2


plot_data <- main %>% filter(variable %in% c("Long Term","Short Term", "Medium Term")) %>% 
  group_by(period) %>%  summarise(total=sum(value)) %>%  mutate(period=as.Date(period)) %>%  mutate(group = "No Index")

plot_data_2 <- main %>% filter(variable %in% c("Long Term W Index","Short Term W Index", "Medium Term W Index")) %>% 
  group_by(period) %>%  summarise(total=sum(value)) %>%  mutate(period=as.Date(period)) %>%  mutate(group = "With Index")

plot_data_main <- rbind(plot_data,plot_data_2)
head(plot_data_main)

important_date <- data.frame(event=c("2nd 2015 ECB rate hikes", "due to Eurozone crisis", "China's PMI decelerated","Covid-19 outbreak"),
                             date = c(as.Date("2011-07-01"),as.Date("2011-07-01"),as.Date("2015-01-01"),as.Date("2020-01-01")))

figure_1 <- plot_data_main %>% 
  ggplot(aes(x=as.Date(period), y=total)) + 
  ggtitle("Total Connectedness") +
  geom_line(size=0.8, aes(linetype=group)) +
  scale_x_date(date_breaks = "6 month",date_labels = "%b%y") +
  annotate("text", size=3,x = important_date$date, 
           y = c(24,23, 25,30), label = important_date$event, hjust=0.5)+
  annotate("segment", x = important_date$date, xend = important_date$date,
           y = c(13,13, 15,12), yend = c(23,23, 24,29),
           colour = "black",arrow = arrow(length = unit(.2,"cm"))) 

mytheme = list(
  theme_classic()+
    theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_blank(),legend.position= c(0.7,0.9), legend.box = "horizontal", strip.text = element_text(face="bold", size=9),
          axis.text=element_text(face="bold", size=7),axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)



figure_1 <- figure_1 +  mytheme

figure_1



