# Zoë Shribman
# Ecological Analysis
# Test script
install.packages("gitcreds")
library(gitcreds)
gitcreds_set() #enter your personal access token

# testing adding changes to my script

# continue working here//// 

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotrix)

#Import data
dat <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = T)
dat<-dat %>%
  mutate(Year=as.factor(Year))

# Biomass over time by site and transect
Native<-dat %>%
  filter(Transect=="Native")

Phrag<-dat %>%
  filter(Transect=="Phragmites")

Transition <- dat %>%
  filter(Transect=="Transition")

#mean biomass at each site by transect
mymeans<-dat %>%
  group_by(Site,Transect,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

# ggplot(mymeans,aes(x=Transect,y=mean,fill=Site))+
#   labs(x = "Transect",y="Biomass")+
#   geom_bar(stat="identity") +
#   geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
#   facet_wrap(~Site, scales = "free")
# 
# ggplot(mymeans,aes(x=Transect,y=mean,fill=Site))+
#   labs(x = "Transect",y="Biomass")+
#   geom_bar(stat="identity") +
#   geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
#   facet_wrap(~Site, scales = "free")
# 
# 
# ggplot(mymeans,aes(x=Year,y=mean,fill=Site))+
#   labs(x = "Year",y="Biomass")+
#   geom_point() +
#   geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)

# biomass unit is g per 400 cm2


# ggplot(mymeans,aes(x=Year,y=mean,fill=Site))+
#   labs(x = "Year",y="Biomass")+
#   geom_bar(stat="identity") +
#   geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
#   facet_wrap(~Site, scales = "free")

### by transect

Native_mymeans<-Native %>%
  group_by(Site,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

ggplot(Native_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "free")+
  ggtitle("Native Transects")

Phragmites_mymeans<-Phrag %>%
  group_by(Site,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

ggplot(Phragmites_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "free")+
  ggtitle("Phragmites Transects")

Transition_mymeans<-Transition %>%
  group_by(Site,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

ggplot(Transition_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "free")+
  ggtitle("Transition Transects")
