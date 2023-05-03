library(tidyverse)
library(dplyr)
library(plotrix)
library(nlme)
library(MASS)
library(gridExtra)

#Run models testing the effect of year (factor) and transect and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect and year*transect on phrag abundance

#Run models testing the effect of year (factor) and transect (and phrag abundance) and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect (and phrag abundance) and year*transect on phrag abundance


phragmain <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)
phragx <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Phragmites.australis))

#gls with year and transect
gls.yt <- gls(Phragmites.australis ~ Year + Transect + Year*Transect, data = phragx)
summary(gls.yt)



#glm with year and transect
phragy <- read.csv("DeleteX.csv", stringsAsFactors = TRUE)
phragy <- phragy %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Phragmites.australis))

glm.yt <- glm(Litter ~ Year + Transect + Year*Transect,data = phragy)
summary(glm.yt)


#lm with year and transect

lm.yt <- lm(Litter ~ Year + Transect + Year*Transect,data = phragy)
summary(lm.yt)
