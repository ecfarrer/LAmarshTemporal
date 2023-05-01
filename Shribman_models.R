# Model work

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotrix)
library(nlme)
library(MASS)

#Import & organize data
dat <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = T)

dat$Year <- as.factor(dat$Year)
dat$Plot <- as.factor(dat$Plot)

phragy <- dat %>%
  filter(!dat$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Richness))%>%
  mutate(Year_lin = as.integer(Year))

#Regular model
model1 <- lme(Richness ~ Site + Year + Transect + Year*Transect + Site*Year + Site*Transect, 
              random = ~1|Plot, data = phragy)

#temporal model --- 2 way
model1TA <- lme(Richness ~ Site + Year + Transect + Year*Transect + Site*Year + Site*Transect,
  correlation = corAR1(form =~ Year_lin|Plot),
  random = ~1|Plot, data = phragy)

#temporal model --- 3 way
model1TA2 <- lme(Richness ~ Site + Year + Transect + Year*Transect + Site*Year + Site*Transect +Site*Transect*Year,
                correlation = corAR1(form =~ Year_lin|Plot),
                random = ~1|Plot, data = phragy)

#Spatial model
model1SA <- gls(Richness ~ Site + Year + Transect + Year*Transect + Site*Year + Site*Transect,
  correlation=corSpher(form = ~ Lat+Long|Year, nugget=T),
  data = phragy)#nugget only for spherical model

options(contrasts=c("contr.helmert","contr.poly"))

anova(model1, model1TA, model1SA)

anova(model1TA, type = "marginal") # 2 way effects

anova(model1TA2, type = "marginal") # 3 way effects
