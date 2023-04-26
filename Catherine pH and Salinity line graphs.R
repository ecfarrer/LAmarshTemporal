library(dplyr)
library(tidyr)
library("plotrix")
library(ggplot2)
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
  facet_wrap(~ Site, scales="free")

#Salinity Over Time by Transect and Site
Phrag_Salinity <- Phrag_TIME %>%
  group_by(Site, Transect, Year)%>%
  summarise(mean = mean(Salinity15cmppt), se = std.error(Salinity15cmppt)) 

ggplot(Phrag_Salinity, aes(x = Year, y = mean, color = Transect)) +
  geom_point(stat="identity") + 
  geom_line()+
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25)+
  labs(title = "Sainity Over Time by Transect and Site", x = "Time",y="Salinity")+
  facet_wrap(~ Site, scales="free")
