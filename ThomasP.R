#install.packages("gitcreds")
library(gitcreds)
#gitcreds_set()
library(tidyverse)
library(dplyr)
library(plotrix)
library(nlme)
library(MASS)
library(gridExtra)

options(contrasts = c("contr.helmert","contr.poly"))

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



#Model Work

#Run models testing the effect of year (factor) and transect and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect and year*transect on phrag abundance

#Run models testing the effect of year (factor) and transect (and phrag abundance) and year*transect on phrag abundance. AND a model with the effect of year (linear) and transect (and phrag abundance) and year*transect on phrag abundance


phragmain <- read.csv("PhragSurvey2017to2022.csv", stringsAsFactors = TRUE)
phragx <- phragmain %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Phragmites.australis))

#gls with year and transect
gls.syt <- gls(Phragmites.australis ~ Site + Year + Transect + Year*Transect, data = phragx)
summary(gls.syt)


#glm with year and transect
phragy <- read.csv("DeleteX.csv", stringsAsFactors = TRUE)
phragy <- phragy %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Phragmites.australis))

glm.syt <- glm(Phragmites.australis ~ Site + Year + Transect + Year*Transect,data = phragy)
summary(glm.syt)


#lm with year and transect

lm.syt <- lm(Phragmites.australis ~ Site + Year + Transect + Year*Transect,data = phragy)
summary(lm.syt)
anova(lm.syt)



# Random effects: Plot, 
#lm with random

lme.sytr <- lme(Phragmites.australis ~ Site + Year + Transect + Year*Transect, random = ~1|Plot, data = phragy)
anova(lme.sytr, type = "marginal")
summary(lme.sytr)

anova(lme.sytr, lm.syt)
#lme.sytr is better

model1 <- lme.sytr



#ModelValidation Notes
#plot(data = phragy, phragy$Year, resid(phragy$Phragmites.australis))

#Diversity is the new model
#model validation needed

#Look at lecture on correlation on autoregressive correlation on temporal autocorrelation
#Lecture 8
#Look at spatial correlation (Never did that)
#look at la marsh repository (other one) that has the code for spatial autocorrelation
# -> LamarshGradient2 :Phragabunbiomasslitter (Corsphere latlong) (Spatial correlation)
#Example
#Final stats model for manuscript
#m1<-lme(phraus~MarshClassV*Transect2,random=~1|Site,correlation=corSpher(form = ~ Lat+Long),weights=varIdent(form=~1|MarshClassV.Transect),data=dat17phrag)
#m2<-lme(phraus~MarshClassV*Transect2,random=~1|Site,correlation=corSpher(form = ~ Lat+Long),data=dat17phrag)#
#anova(m1,m2) #var ident not significant
#anova(m2,type="margin")
#m1m<-as.data.frame(summary(emmeans(m2,~Transect2|MarshClassV)))




#ModelValidation
par(mfrow = c(1,2))
fm1 <- fitted(model1)
resid1 <- resid(model1, type = "normalized")
hist(resid(model1, type = "normalized"))

plot(fm1, resid1)
plot(phragy$Transect, resid1)
#non-normal distribution of residuals


model1res <- resid(model1)
model1res <- model1res[is.na(model1res)==F]
acf(model1res)
#High auto correlation?

model1TA <- lme(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  correlation = corAR1(form =~ Year),
  random = ~1|Plot, data = phragy)

summary(model1TA)
#results still significant even factoring for temporal autocorrelation

#Testing spatial correlation
#model1SA <- lme(
#  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
#  correlation=corSpher(form = ~ Lat+Long),
#  random = ~1|Plot, data = phragy)
# is there latitude longitude data somewhere? Ask for specific location or use SITE?

model1TApois <- glm(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  family = poisson, data = phragy)

summary(model1TApois)

#residual deviance = ~5 (Bad, should be ~1)

#Do you have to analyze part of the data at once? (Not correct)
#ggplot(data = phragy, aes(x = Year, y = Phragmites.australis) +
#  geom_point()+
#  geom_smooth(method='glm',method.args=list(family='poisson'))


model1TApois <- glm(
    Phragmites.australis ~ Site + Year + Transect + Year*Transect,
    family = poisson, data = phragy)

drop1(model1TApois,.~., test = "Chi")

model1TAquasi<- glm(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  family = quasipoisson, data = phragy)

summary(model1TAquasi)

#Over dispersion still about 5 


model1TAneg<- glm.nb(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  link = "log", data = phragy)
summary(model1TAneg)
#over dispersion is about ~1 now. This is the best one to use.
#use package lme4 package. Add random effects to negative distributions

hist()



glm.nb(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  link = "log", data = phragy)
negplot <- resid(model1TAneg, type = "deviance")
negplotpredict <- predict(model1TAneg, type = "link")

par(mfrow = c(1,3))
plot(negplot)
plot(negplotpredict)
#weird!



#Actual Test for Spatial Correlation
SpatialData <- read.csv("latlongPhragSurvey2017to2022.csv", stringsAsFactors = TRUE)

SpatialData2 <- SpatialData %>%
  filter(!phragmain$Site %in% c('LUMCON 1', 'LUMCON 2', 'Fontainebleau', 'Turtle Cove'), !is.na(Phragmites.australis))



model1SA <- gls(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  correlation=corSpher(form = ~ Lat+Long|Year),
  data = SpatialData2)

summary(model1SA)

anova(model1SA, model1TA)

model1resSA <- resid(model1SA)
model1resSA <- model1res[is.na(model1res)==F]


acf(model1resSA)
#lag = meters
    
#use package lme4 package. Add random effects to negative distributions
#figure out the negative bionimail distrbution


#compare regular, spatial model, temporal model together

#Reg Model
model1 <- lme(Phragmites.australis ~ Site + Year + Transect + Year*Transect, random = ~1|Plot, data = phragy)

#temporal model
model1TA <- lme(
  Phragmites.australis ~ Site + Year + Transect + Year*Transect,
  correlation = corAR1(form =~ Year),
  random = ~1|Plot, data = phragy)

#spatial model



