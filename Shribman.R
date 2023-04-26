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
library(viridis)
library(nlme)

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
dat2 <- dat%>%
  filter(dat$Site %in% c("Barataria", "Bayou Sauvage","Big Branch", "Pearl River"))

mymeans<-dat2 %>%
  group_by(Site,Transect,Year)%>%
  summarize(mean=mean(Biomass,na.rm=T),se=std.error(Biomass))

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
  facet_wrap(~Site, scales = "fixed")+
  ggtitle("Native Transects")

Phragmites_mymeans<-Phrag %>%
  group_by(Site,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

ggplot(Phragmites_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "fixed")+
  ggtitle("Phragmites Transects")

Transition_mymeans<-Transition %>%
  group_by(Site,Year)%>%
  summarize(mean=mean(Biomass),se=std.error(Biomass))

ggplot(Transition_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "fixed")+
  ggtitle("Transition Transects")



### 
ggplot(Native_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_point() +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  facet_wrap(~Site, scales = "fixed")+
  ggtitle("Native Transects")

ggplot(Native_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_point(aes(colour = factor(Site))) +
  geom_line()+
  scale_color_viridis(discrete=TRUE) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se, colour = factor(Site)),width=.2)+
  ggtitle("Native Transects")

ggplot(Native_mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_point()+
  geom_line(aes(x=Year))+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.2)+
  ggtitle("Native Transects")



ggplot(mymeans,aes(x=Year,y=mean,fill=Site))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_point(aes(colour = factor(Site))) +
  geom_line(stat="smooth",method = "lm",size=.8)+
  scale_color_viridis(discrete=TRUE) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se, colour = factor(Site)),width=.2)+
  ggtitle("Native Transects")

## still can't get the line added
ggplot(mymeans,aes(x=Year,y=mean,col=Transect))+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymax= mean+se,ymin= mean-se),width=.2)+
  facet_wrap(~Site, scales = "fixed")


labs(x = "Year",y="Biomass (g/400cm2)")+
  +
  ggtitle("A")

ggplot(mymeans,aes(x=Year,y=mean,fill=Transect))+
  labs(x = "Year",y="Biomass (g/400cm2)")+
  geom_point(aes(colour = factor(Transect))) +
  geom_line(stat="smooth",method = "lm",size=.8)+
  scale_color_viridis(discrete=TRUE) +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se, colour = factor(Site)),width=.2)+
  ggtitle("B")+
  facet_wrap(~Site, scales = "fixed")


#### model testing ####

# effect of year (factor) and transect and year*transect on phrag abundance

m <- lm(Phragmites.australis ~ Year + Transect + Year*Transect, data = dat)
anova(m,type="marginal") # not sure why this isn't working, trying to run Type III anova
summary(m)

options(contrasts=c("contr.helmert","contr.poly"))
m1 <- gls(Phragmites.australis ~ Year + Transect + Year*Transect, data = dat)
# not working: Error in na.fail.default(list(Phragmites.australis = c(3L, 6L, 7L, 2L,  : 
# missing values in object

anova(m1,type="marginal") # not sure why this isn't working, trying to run Type III anova
summary(m1)




# effect of year (linear) and transect and year*transect on phrag abundance


