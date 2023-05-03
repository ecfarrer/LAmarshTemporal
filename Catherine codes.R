library(dplyr)
library(tidyr)
library("plotrix")
library(ggplot2)

###Plots
##A) EXERCISE 1:-Phragmites Abundance  Over Time by Transect and Site
dat <- read.csv("PhragSurvey2017to2022.csv")
mymeans<-dat %>%
  mutate(Site=factor(Site, levels=c("Pearl River", "Big Branch","Barataria","Bayou Sauvage"))) %>%
  mutate(Transect=factor(Transect, levels=c("Phragmites", "Transition","Native"))) %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean=mean(Phragmites.australis, na.rm = TRUE),se=std.error(Phragmites.australis)) %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Transect))

ggplot(mymeans,aes(x=Year,y=mean,color=Transect))+
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(x = "Year",y="Abundance")+
  facet_wrap(~Site, scales="fixed")


##B) EXERCISE 2
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(pH), !is.na(Salinity15cmppt)) 

#pH Over Time by Transect and Site
Phrag_pH <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(pH), se = std.error(pH)) 

ggplot(Phrag_pH, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "pH Over Time by Transect and Site", x = "Time",y="pH")+
  facet_wrap(~ Site, scales="fixed")

#Salinity Over Time by Transect and Site
Phrag_Salinity <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Salinity15cmppt), se = std.error(Salinity15cmppt)) 

ggplot(Phrag_Salinity, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Sainity Over Time by Transect and Site", x = "Time",y="Salinity")+
  facet_wrap(~ Site, scales="fixed")


##C) EXERCISE 3
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(Richness))

#Richness Over Time by Transect and Site
Phrag_Richness <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Richness), se = std.error(Richness)) 

ggplot(Phrag_Richness, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Richness Over Time by Transect and Site", x = "Time",y="Richness")+
  facet_wrap(~ Site, scales="fixed")

#S. patens abundance Over Time by Transect and Site
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(Spartina.patens))
Spartina.patens_abund <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Spartina.patens), se = std.error(Spartina.patens)) 

ggplot(Spartina.patens_abund, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Spartina.patens_abund Over Time by Transect and Site", x = "Time",y="Spartina.patens Abundance")+
  facet_wrap(~ Site, scales="fixed")

#Native Richness Over Time by Transect and Site
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(NatRichness))
Phrag_NatRichness <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(NatRichness), se = std.error(NatRichness)) 

ggplot(Phrag_NatRichness, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Native Richness Over Time by Transect and Site", x = "Time",y="Native Richness")+
  facet_wrap(~ Site, scales="fixed")

#Overall Shannon Diversity Over Time by Transect and Site
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(Shannon))
Phrag_OverallShannon <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Shannon), se = std.error(Shannon)) 

ggplot(Phrag_OverallShannon, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Diversity (Shannon index) Over Time by Transect and Site", x = "Time",y="Diversity")+
  facet_wrap(~ Site, scales="fixed")

#Overall Evenness Over Time by Transect and Site
Phragmites <- read.csv("C:/Users/mac/Downloads/LAmarshTemporal/PhragSurvey2017to2022.csv",stringsAsFactors = T)
Phrag_TIME <- Phragmites %>%
  filter(!Phragmites$Site %in% c("Turtle Cove", "Fontainebleau", "LUMCON 1", "LUMCON 2"),
         !is.na(Evenness))
Phrag_OverallEvenness <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Evenness), se = std.error(Evenness)) 

ggplot(Phrag_OverallEvenness, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Species Evenness Over Time by Transect and Site", x = "Time",y="Evenness")+
  facet_wrap(~ Site, scales="fixed")

### Models (Evenness)
library(dplyr)
library(tidyr)
library("plotrix")
library(ggplot2)
library(nlme)

phragmain <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)
phragy <-phragmain %>% mutate(Year.factor = factor(Year)) %>% filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Evenness))
options(contrasts=c("contr.helmert","contr.poly"))
##1) All
#Reg Model
model1 <- lme(Evenness ~ Site + Year.factor + Transect + Year.factor*Transect + Site*Year.factor + Transect*Site+ Site*Year.factor*Transect, random = ~1|Plot, data = phragy)

#temporal model
model1TA <- lme(
  Evenness ~ Site + Year.factor + Transect + Year.factor*Transect + Site*Year.factor + Transect*Site+ Site*Year.factor*Transect,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = phragy)

#spatial model
model1SA <- gls(
  Evenness ~ Site + Year.factor + Transect + Year.factor*Transect + Site*Year.factor + Transect*Site+ Site*Year.factor*Transect,
  correlation=corSpher(form = ~ Lat+Long|Year.factor),
  data = phragy)

anova(model1, model1TA, model1SA)
#model1TA is the best model, making spatial correlation more important

#Type III Anova
anova(model1TA, type ="marginal")
