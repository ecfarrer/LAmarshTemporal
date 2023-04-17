install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
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

library(gitcreds)
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


mymeans<-tidydat %>%
  mutate(Site=factor(Site,
                        levels=c("Pearl River", "Big Branch","Barataria","Bayou Sauvage"
                                 ))) %>%
  mutate(Transect=factor(Transect,
                     levels=c("Phragmites", "Transition","Native"
                     ))) %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean=mean(Abundance),se=std.error(Abundance)) %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Transect))


ggplot(mymeans,aes(x=Year,y=mean,color=Transect))+
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(x = "Year",y="Abundance")+
  facet_wrap(~Site, scales="free")
