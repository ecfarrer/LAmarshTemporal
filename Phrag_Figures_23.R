###Redoing Phrag Figures with 2023 data
##2/16/24
#Coleman Benedict

library(tidyverse)
library(plotrix)
library(ggthemes)

options(contrasts=c("contr.helmert","contr.poly"))

dat23<-read.csv("/Users/coleman/Documents/R/LAMARSH/Data/PhragSurvey2017to2023.csv",stringsAsFactors = T,row.names=1)
View(dat23)

dat_23<-dat23%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))
dat_23$Transect<-factor(dat_23$Transect,levels=c("Native","Transition","Phragmites"))
dat_23$Plot<-factor(dat_23$Plot)
dat_23$Site<-factor(dat_23$Site,levels=c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))


###PHRAG ABUNDANCE
dat23p<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Phragmites.australis,na.rm=T),se=std.error(Phragmites.australis))
data.frame(dat23p)
ggplot(dat23p,aes(x=Year,y=mean,col=Transect))+
  labs(y="Phragmites australis abundance") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)

###Litter
dat23l<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Litter,na.rm=T),se=std.error(Litter))
data.frame(dat23l)
ggplot(dat23l,aes(x=Year,y=mean,col=Transect))+
  labs(y="Average Litter") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)



###Biomass
dat23b<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Biomass,na.rm=T),se=std.error(Biomass))
data.frame(dat23b)
ggplot(dat23b,aes(x=Year,y=mean,col=Transect))+
  labs(y="Average Biomass") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###Shannon
dat23s<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Shannon,na.rm=T),se=std.error(Shannon))
data.frame(dat23s)
ggplot(dat23s,aes(x=Year,y=mean,col=Transect))+
  labs(y="Average Shannon Diversity") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###Salinity
dat23sal<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Salinity15cmppt,na.rm=T),se=std.error(Salinity15cmppt))
data.frame(dat23sal)
ggplot(dat23sal,aes(x=Year,y=mean,col=Transect))+
  labs(y="Average Salinity") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###Native Abundance
dat23a<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(NatAbun,na.rm=T),se=std.error(NatAbun))
data.frame(dat23a)
ggplot(dat23a,aes(x=Year,y=mean,col=Transect))+
  labs(y="Native Abundance") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###Evenness
dat23e<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Evenness,na.rm=T),se=std.error(Evenness))
data.frame(dat23e)
ggplot(dat23e,aes(x=Year,y=mean,col=Transect))+
  labs(y="Evenness") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###pH
dat23ph<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(pH,na.rm=T),se=std.error(pH))
data.frame(dat23ph)
ggplot(dat23ph,aes(x=Year,y=mean,col=Transect))+
  labs(y="pH") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)

###water depth
dat23dep<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(WaterDepthcm,na.rm=T),se=std.error(WaterDepthcm))
data.frame(dat23dep)
ggplot(dat23dep,aes(x=Year,y=mean,col=Transect))+
  labs(y="Average Water Depth (cm)") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###Richness
dat23r<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Richness,na.rm=T),se=std.error(Richness))
data.frame(dat23r)
ggplot(dat23r,aes(x=Year,y=mean,col=Transect))+
  labs(y="Native Richness") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)


###dead phrag

dat23dp<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(DeadPhragStems,na.rm=T),se=std.error(DeadPhragStems))
data.frame(dat23dp)
ggplot(dat23dp,aes(x=Year,y=mean,col=Transect))+
  labs(y="Dead Phrag Stems") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)

##spartina
dat23spa<-dat_23%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Spartina.cynosuroides,na.rm=T),se=std.error(Spartina.cynosuroides))
data.frame(dat23spa)
ggplot(dat23spa,aes(x=Year,y=mean,col=Transect))+
  labs(y="Spartina C") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)

