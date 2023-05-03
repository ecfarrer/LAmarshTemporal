install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
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

library(plotrix)
library(ggplot2)
library(dplyr)
library(tidyr)





## Regressions of Phrag or native species abundance vs. salinity

dat <- read.csv("PhragSurvey2017to2022.csv")
head(dat)


tidydat <- pivot_longer(dat,names_to="Species",
                        values_to="Abundance",Phragmites.australis:Spartina.patens)

head(tidydat)

tidydat$fTransect <- factor(tidydat$Transect)


mymeans<-dat %>%
  mutate(Site=factor(Site,
                        levels=c("Pearl River", "Big Branch","Barataria","Bayou Sauvage"
                                 ))) %>%
  mutate(Transect=factor(Transect,
                     levels=c("Phragmites", "Transition","Native"
                     ))) %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean=mean(Phragmites.australis, na.rm = TRUE),se=std.error(Phragmites.australis)) %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Transect))


ggplot(mymeans,aes(x=Year,y=mean,color=Transect))+
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(x = "Year",y="Abundance")+
  facet_wrap(~Site, scales="free")

###########################################################################

##Biomass over Time, based on site


dat <- read.csv("PhragSurvey2017to2022.csv")
head(dat)


tidydat <- pivot_longer(dat,names_to="Species",
                        values_to="Abundance",Phragmites.australis:Spartina.patens)

head(tidydat)

tidydat$fTransect <- factor(tidydat$Transect)


mymeans<-dat %>%
  mutate(Site=factor(Site,
                     levels=c("Pearl River", "Big Branch","Barataria","Bayou Sauvage"
                     ))) %>%
  mutate(Transect=factor(Transect,
                         levels=c("Phragmites", "Transition","Native"
                         ))) %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean=mean(Biomass, na.rm = TRUE),se=std.error(Biomass)) %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Transect))


ggplot(mymeans,aes(x=Year,y=mean,color=Transect))+
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(x = "Year",y="Biomass")+
  facet_wrap(~Site, scales="free")

#############################################################################################
## Litter over Time, Based on site

dat <- read.csv("PhragSurvey2017to2022.csv")
head(dat)


tidydat <- pivot_longer(dat,names_to="Species",
                        values_to="Abundance",Phragmites.australis:Spartina.patens)

head(tidydat)

tidydat$fTransect <- factor(tidydat$Transect)


mymeans<-dat %>%
  mutate(Site=factor(Site,
                     levels=c("Pearl River", "Big Branch","Barataria","Bayou Sauvage"
                     ))) %>%
  mutate(Transect=factor(Transect,
                         levels=c("Phragmites", "Transition","Native"
                         ))) %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean=mean(Litter, na.rm = TRUE),se=std.error(Litter)) %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Transect))


ggplot(mymeans,aes(x=Year,y=mean,color=Transect))+
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(x = "Year",y="Litter")+
  facet_wrap(~Site, scales="free")

###############################################################################
library(nlme)

options(contracts = c("contr.helmert","contr.poly"))

dat <- read.csv("PhragSurvey2017to2022.csv")
head(dat)

SpatialData <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)

SpatialData2 <- SpatialData %>%
  filter(!SpatialData$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Litter))

SpatialData2$fYear <- factor(SpatialData2$Year)


#Regular
model1 <- gls(Litter ~ Site + fYear + Transect + fYear*Transect + Site*fYear+ Site*Transect, 
               data = SpatialData2)

#temporal model
model1TA <- lme(
  Litter ~ Site + fYear + Transect + fYear*Transect + Site*fYear,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = SpatialData2)

#Spatial model
model1SA <- gls(
  Litter ~ Site + fYear + Transect + fYear*Transect + Site*fYear,
  correlation=corSpher(form = ~ Lat+Long|Year, nugget=T),
  data = SpatialData2)
#nugget only for spherical model

anova(model1, model1TA, model1SA)

anova(model1, type = "marginal")
