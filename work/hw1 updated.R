library(readxl)
library(ggplot2)
library(tidyverse)

epl <- read_excel("C:/Users/asus/Desktop/all_data.xlsx")
data<-as.data.frame(epl[,1:56])
str(data)
data[,-c(1,2,3,4,5,8,11,12)]<-apply(data[,-c(1,2,3,4,5,8,11,12)],2,as.numeric)
data[,8]<-as.factor(data[,8])
task1<-data[,6:7]
task1$HG_AG<-task1$FTHG-task1$FTAG


#Home Goals Histogram
ggplot(task1, aes(FTHG)) +
  geom_histogram(binwidth= 1, color="black", fill="plum") +
  xlab("Home Goals") + ylab("Number of Games") +
  scale_x_discrete(limits=c(0:max(task1$FTHG)))

#Home Goals Histogram with Density

hg<-task1 %>% 
  group_by(FTHG) %>% 
  summarise(obs=n())

hg$exp_pois<-dpois(c(0:max(task1$FTHG)),lambda= mean(task1$FTHG))*nrow(task1)

ggplot(hg, aes(FTHG,obs)) +
  geom_col(binwidth= 1, color="black", fill="plum")+
  geom_line(aes(y=exp_pois))+
  scale_x_discrete(limits=c(0:max(task1$FTAG)))


#Away Goals Histogram 
ggplot(task1, aes(FTAG)) +
  geom_histogram(binwidth= 1, color="black", fill="lavender") +
  xlab("Away Goals") + ylab("Number of Games")+
  scale_x_discrete(limits=c(0:max(task1$FTAG)))

#Away Goals Histogram with Density
ggplot(task1, aes(FTAG)) +
  geom_histogram(aes(y=..density..),binwidth= 1, color="black", fill="lavender") +
  xlab("Away Goals") + ylab("Number of Games") +
  scale_x_discrete(limits=c(0:max(task1$FTAG)))+
  geom_line(aes(y=dpois(FTAG,lambda= mean(FTAG))), colour="purple")


#Home-Away Goals Histogram
ggplot(task1, aes(HG_AG)) +
  geom_histogram(binwidth= 1, color="black", fill="bisque") +
  xlab("Home-Away Goals") + ylab("Number of Games")+
  scale_x_discrete(limits=c(min(task1$HG_AG):max(task1$HG_AG)))

#Calculation
m<-mean(task1$HG_AG)
sdev<-sd(task1$HG_AG)

#Home-Away Goals Histogram with Density-red shows the normal distribution
ggplot(task1, aes(HG_AG)) +
  geom_histogram(aes(y=..density..),binwidth= 1, color="black", fill="bisque") +
  xlab("Home-Away Goals") + ylab("Number of Games") +geom_density() + 
  stat_function(color="red",fun = dnorm, args = list(mean = m, sd = sdev)) +
  scale_y_continuous(breaks = NULL)

#####TASK 2#####
data.frame(colnames(data))

#BET 365

b365<-data[,c(1:8,25:27)]
b365$is_draw<-ifelse(b365$FTR=="D",1,0)

b365$P_home<-1/b365$B365H
b365$P_draw<-1/b365$B365D
b365$P_away<-1/b365$B365A
b365$sum_prob<-b365$P_home + b365$P_draw + b365$P_away

b365$NP_home<-b365$P_home/b365$sum_prob
b365$NP_draw<-b365$P_draw/b365$sum_prob
b365$NP_away<-b365$P_away/b365$sum_prob

b365$ph_pa<-b365$P_home-b365$P_away
b365$nph_npa<-b365$NP_home-b365$NP_away


ggplot(b365, aes(ph_pa, P_draw)) + 
  geom_point()+ xlab("P(Home Win) – P(Away Win)") + 
  ylab("P (Tie)") +  ggtitle("Bet 365")+
  geom_smooth()

b365$p_range<-cut(b365$ph_pa,breaks = seq(-1,1,by=0.05))

#b365$p_d_range<-cut(b365$P_draw,breaks = seq(0,1,by=0.02))


# ggplot(b365, aes(p_range)) +
#   geom_bar(color="black", fill="plum")+
#   xlab("P(Home Win) – P(Away Win)") + ylab("Number of Games")
# 
# 
# ggplot(b365, aes(p_range,fill=FTR)) +
#   geom_bar(color="black")+
#   xlab("P(Home Win) – P(Away Win)") + ylab("Number of Games")
# 
# a<-b365 %>%
#   group_by(p_d_range) %>%
#   summarise(draw_num=sum(is_draw), obs=n())
# 
# a$draw_real_prob<-a$draw_num/a$obs
# 
# ggplot(a, aes(p_d_range, draw_real_prob)) +
#   geom_col()


b<-b365 %>% 
  group_by(p_range) %>% 
  summarise(draw_num=sum(is_draw), obs=n())

b$ort<-seq(-0.825,0.925,by=0.05)

b$draw_real_prob<-b$draw_num/b$obs

ggplot(b, aes(p_range, draw_real_prob)) + 
  geom_col()

ggplot() + 
  geom_point(data=b365, aes(x=ph_pa, y=P_draw))+ geom_smooth(data=b365, aes(x=ph_pa, y=P_draw))+
  geom_point(data=b,aes(x=ort,y=draw_real_prob,colour="red")) +geom_smooth(data=b,aes(x=ort,y=draw_real_prob,colour="red"),se=FALSE)




#BET AND WIN

bw<-data[,c(1:8,28:30)]
bw$P_home<-1/bw$BWH
bw$P_draw<-1/bw$BWD
bw$P_away<-1/bw$BWA
bw$sum_prob<-bw$P_home+bw$P_draw+bw$P_away

bw$NP_home<-bw$P_home/bw$sum_prob
bw$NP_draw<-bw$P_draw/bw$sum_prob
bw$NP_away<-bw$P_away/bw$sum_prob

bw$ph_pa<-bw$P_home-bw$P_away
bw$nph_npa<-bw$NP_home-bw$NP_away


ggplot(bw, aes(ph_pa, P_draw)) + 
  geom_point()+ xlab("P(Home Win) – P(Away Win)") +
  ylab("P (Tie)") + ggtitle("Bet and Win")

#SOME BOOKMAKER

iw<-data[,c(1:8,31:33)]
iw$P_home<-1/iw$IWH
iw$P_draw<-1/iw$IWD
iw$P_away<-1/iw$IWA
iw$sum_prob<-iw$P_home+iw$P_draw+iw$P_away

iw$NP_home<-iw$P_home/iw$sum_prob
iw$NP_draw<-iw$P_draw/iw$sum_prob
iw$NP_away<-iw$P_away/iw$sum_prob

iw$ph_pa<-iw$P_home-iw$P_away
iw$nph_npa<-iw$NP_home-iw$NP_away

ggplot(iw, aes(ph_pa, P_draw)) + 
  geom_point()+ xlab("P(Home Win) – P(Away Win)") +
  ylab("P (Tie)") + ggtitle("Some Bookmaker")
