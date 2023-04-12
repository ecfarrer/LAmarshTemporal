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


plot(dat$Phragmites.australis ~ dat$Salinity15cmppt, xlab = "salinity", ylab = "abundance")

tidydat <- pivot_longer(dat,names_to="Species",
                        values_to="Abundance",Phragmites.australis:Spartina.patens)

tidydat <- tidydat %>%
  filter(!is.na(Salinity15cmppt)) %>%
  filter(!is.na(Abundance))

head(tidydat)

tidydat$fTransect <- factor(tidydat$Transect)


mymeans<-tidydat %>%
  mutate(Species=factor(Species,
                        levels=c("Phragmites.australis", "Eleocharis.sp.","Ipomoea.sagittata","Schoenoplectus.americanus"
                                 ,"Spartina.patens", "Sagittaria.lancifolia"))) %>%
  group_by(Species, Salinity15cmppt)%>%
  summarise(mean=mean(Abundance),se=std.error(Abundance)) %>%
  filter(!is.na(Species))


ggplot(mymeans,aes(x=Salinity15cmppt,y=mean,color=Species))+
  geom_point(stat="identity") + 
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  facet_wrap(~Species, scales="free")
