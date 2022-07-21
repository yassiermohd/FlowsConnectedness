rm(list=ls())

library(frequencyConnectedness)
library(parallel)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape)
library(scales)
library(gridExtra)


main <- read.csv("Final.csv") %>%  mutate(period=as.Date(`period`))
mytheme = list(
  theme_classic()+
    theme(panel.background = element_blank(),strip.background = element_rect(colour=NA, fill=NA),panel.border = element_rect(fill = NA, color = "black"),
          legend.title = element_blank(),legend.position= c(0.7,0.9), legend.box = "horizontal", strip.text = element_text(face="bold", size=9),
          axis.text=element_text(face="bold", size=7),axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(face = "bold", hjust = 0.5,size=13))
)

# Yilmaz Chart 1 Debt vs Debt USD, log differebce of broad index

plot_data <- main %>% filter(type %in% c("DebtIndex","Debt")) %>%  select(period, DY, type, log_index, nominal_index) %>% 
  group_by(period, type) %>%  summarise_at(vars(DY,log_index,nominal_index), mean)
plot_data
annotate_data <- main %>% filter(type %in% c("DebtIndex","Debt")) %>%  select(period, log_index) %>%  group_by(period) %>%  summarise_at(vars(log_index), mean) %>%  filter(log_index >0)
annotate_data

ggplot() +
  geom_line(data=plot_data, aes(x=period, y=DY, linetype=type)) + 
  geom_bar(data=annotate_data,aes(x=period, y=log_index*1), stat="identity") + mytheme

plot_data <- main %>% filter(type %in% c("DebtIndex","Debt")) %>%  select(period, DY, type, log_index, nominal_index) %>% 
  group_by(period, type) %>%  summarise_at(vars(DY,log_index,nominal_index), mean) %>%  pivot_wider(names_from=type, values_from=DY) %>% 
  mutate("Diff" = DebtIndex-Debt)
plot_data





# Yilmaz Chart 2 Equity vs Equity USD

plot_data <- main %>% filter(type %in% c("EquityIndex","Equity")) %>%  select(period, DY, type, log_index) %>% 
  group_by(period, type) %>%  summarise_at(vars(DY,log_index), mean)


ggplot() +
  geom_line(data=plot_data, aes(x=period, y=DY, linetype=type)) + mytheme

# Decomp Chart 3 Debt with Fre

plot_data <- main %>% filter(type %in% c("DebtIndex")) %>%  select(period, ST_Decomp, MT_Decomp, LT_Decomp, type, log_index) %>% 
  group_by(period, type) %>%  summarise_at(vars(ST_Decomp, MT_Decomp, LT_Decomp), mean)

p1 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=ST_Decomp)) +mytheme +ggtitle("Short Term")
p2 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=MT_Decomp)) +mytheme +ggtitle("Medium Term")
p3 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=LT_Decomp)) +mytheme +ggtitle("Long Term")

p <- grid.arrange(p1,p2,p3, nrow=3,ncol=1)


#convert into one

# DEcomp Chart 4 Equity with Fre

plot_data <- main %>% filter(type %in% c("EquityIndex","Equity")) %>%  select(period, ST_Decomp, MT_Decomp, LT_Decomp, type, log_index) %>% 
  group_by(period, type) %>%  summarise_at(vars(ST_Decomp, MT_Decomp, LT_Decomp), mean)

p1 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=ST_Decomp, linetype=type)) +mytheme +ggtitle("Short Term")
p2 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=MT_Decomp, linetype=type)) +mytheme +ggtitle("Medium Term")
p3 <- ggplot() +
  geom_line(data=plot_data, aes(x=period, y=LT_Decomp, linetype=type)) +mytheme +ggtitle("Long Term")

p <- grid.arrange(p1,p2,p3, nrow=3,ncol=1)



# 



# Net spillover Debt

plot_data <- main %>%  filter(type == "DebtIndex") %>%  select(period, Country, ST_Spill,MT_Spill,LT_Spill)

p1 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="Hungary"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="Hungary"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="Hungary"), aes(x=period, y=LT_Spill)) + ggtitle("Hungary") +mytheme

p2 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="Indonesia.1"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="Indonesia.1"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="Indonesia.1"), aes(x=period, y=LT_Spill)) + ggtitle("Indonesia") +mytheme

p3 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="India"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="India"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="India"), aes(x=period, y=LT_Spill)) + ggtitle("India") +mytheme


p4 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="Thailand"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="Thailand"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="Thailand"), aes(x=period, y=LT_Spill)) + ggtitle("Thailand") +mytheme


p5 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="South_Africa"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="South_Africa"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="South_Africa"), aes(x=period, y=LT_Spill)) + ggtitle("South Africa") +mytheme

p6 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="Mexico"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="Mexico"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="Mexico"), aes(x=period, y=LT_Spill)) + ggtitle("Mexico") +mytheme

p7 <- ggplot() + 
  geom_line(data=plot_data %>% filter(Country=="Poland"), aes(x=period, y=ST_Spill)) + 
  geom_line(data=plot_data %>% filter(Country=="Poland"), aes(x=period, y=MT_Spill)) +
  geom_line(data=plot_data %>% filter(Country=="Poland"), aes(x=period, y=LT_Spill)) + ggtitle("Poland") +mytheme

p <- grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow=4,ncol=2)




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



