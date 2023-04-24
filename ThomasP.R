install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
library(tidyverse)
library(dplyr)
library(plotrix)
library(nlme)
library(MASS)
library(gridExtra)


#Litter over time
phragmain <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)

phragx <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Litter))

litterOT <- phragx %>%
  ggplot(aes(x = Year, y = Litter, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragx$Transect)

litterOT  

litterOT2 <- phragx %>%
  ggplot(aes(x = Year, y = Litter, fill = Transect)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragx$Site)

litterOT2

grid.arrange(litterOT,litterOT2, ncol = 2)



#Diversity over Time

phragy <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Richness))

diversityOT <- phragy %>%
  ggplot(aes(x = Year, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragy$Transect)


diversityOT2 <- phragy %>%
  ggplot(aes(x = Year, y = Richness, fill = Transect)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragy$Site)

grid.arrange(diversityOT,diversityOT2, ncol = 2)





diversityOT <- phragy %>%
  ggplot(aes(x = Year, y = Richness, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragy$Transect)


diversityOT2 <- phragy %>%
  ggplot(aes(x = Year, y = Richness, fill = Transect)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(phragy$Site)

grid.arrange(diversityOT,diversityOT2, ncol = 2)





#Line Graphs: LitterOT and DiversityOT


#Litter
phragxtransect <- phragx %>%
  group_by(Site, Year, Transect) %>%
  summarise(means = mean(Litter), se = std.error(Litter))

litterOT <- phragxtransect %>%
  ggplot(aes(x = Year, y = means, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), color = "black", width = 0.1) +
  facet_wrap(phragxtransect$Transect)

litterOT

phragxsite <- phragx %>%
  group_by(Site, Year, Transect) %>%
  summarise(means = mean(Litter), se = std.error(Litter))

litterOT2 <- phragxsite %>%
  ggplot(aes(x = Year, y = means, color = Transect)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), color = "black", width = 0.1) +
  facet_wrap(phragxsite$Site)

litterOT2

grid.arrange(litterOT,litterOT2, ncol = 2)

#Diversity
phragy <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Richness))

phragy <- phragy %>%
  group_by(Site, Year, Transect) %>%
  summarise(means = mean(Richness), se = std.error(Richness))


diversityOT <- phragy %>%
  ggplot(aes(x = Year, y = means, color = Site)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), color = "black", width = 0.1) +
  facet_wrap(phragy$Transect)


diversityOT2 <- phragy %>%
  ggplot(aes(x = Year, y = means, color = Transect)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = means + se, ymin = means - se), color = "black", width = 0.1) +
  facet_wrap(phragy$Site)

grid.arrange(diversityOT,diversityOT2, ncol = 2)
