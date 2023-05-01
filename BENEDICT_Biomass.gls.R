library(tidyverse)
library(dplyr)
library(plotrix)
library(nlme)
library(MASS)
library(gridExtra)

#Run models testing the effect of year (factor) and transect and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect and year*transect on phrag abundance

#Run models testing the effect of year (factor) and transect (and phrag abundance) and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect (and phrag abundance) and year*transect on phrag abundance


phragmain <- read.csv("/Users/coleman/Documents/R/LAMARSH/Data/PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)
phragx <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Biomass))

View(phragx)

#gls with year and transect
gls1 <- gls(Biomass ~ Site + Year + Transect +Year*Transect + Site*Year, data = phragx)


lme1 <- lme(Biomass ~ Site + Year + Transect + Year*Transect + Site*Year, 
              random = ~1|Plot, data = phragx)

lmeTA1 <- lme(
  Biomass ~ Site + Year + Transect + Year*Transect + Site*Year,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = phragx)

glsSA1 <- gls(
  Biomass ~ Site + Year + Transect + Year*Transect + Site*Year,
  correlation=corSpher(form = ~ Lat+Long|Year, nugget=T),
  data = phragx)

anova(gls1, glsSA1,lmeTA1,lme1)


gls2 <- gls(Biomass ~ Site + Year + Transect +Year*Transect + Site*Year + Site*Transect, data = phragx)


lme2 <- lme(Biomass ~ Site + Year + Transect + Year*Transect + Site*Year + Site*Transect, 
            random = ~1|Plot, data = phragx)



anova(lme2,gls2)

anova(type='marginal', gls2)


