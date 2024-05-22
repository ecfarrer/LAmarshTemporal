####Phragmites Stats 2023 data
###2/16/24
##Coleman Benedict

library(tidyverse)
library(plotrix)
library(nlme)


options(contrasts=c("contr.helmert","contr.poly"))

dat23<-read.csv("/Users/coleman/Documents/R/LAMARSH/Data/PhragSurvey2017to2023.csv",stringsAsFactors = T,row.names=1)


dat_23<-dat23%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))
dat_23$Transect<-factor(dat_23$Transect,levels=c("Native","Transition","Phragmites"))
dat_23$Plot<-factor(dat_23$Plot)
dat_23$Site<-factor(dat_23$Site,levels=c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))


View(datdat_23)
is.factor(dat_23$Yearfac)

###### lme models ######

##PHRAG ABUNDANCE 

datp23<-dat_23%>%
  filter(!is.na(Phragmites.australis))

dim(datp23)


model1TA23 <- lme(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp23)


summary(model1TA23)
anova(model1TA23,type="marginal")


plot(fitted(model1TA23),resid(model1TA23,type="normalized"))
plot(datp23$Year,resid(model1TA23,type="normalized"))
plot(datp23$Site,resid(model1TA23,type="normalized"))
plot(datp23$Transect,resid(model1TA23,type="normalized"))
hist(resid(model1TA23,type="normalized"))

##LITTER

datl23<-dat_23%>%
  filter(!is.na(Litter))

model2TA23 <- lme(
  Litter ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datl23)

summary(model2TA23)
anova(model2TA23,type="marginal")

plot(fitted(model2TA23),resid(model2TA23,type="normalized"))
plot(datl23$Year,resid(model2TA23,type="normalized"))
plot(datl23$Site,resid(model2TA23,type="normalized"))
plot(datl23$Transect,resid(model2TA23,type="normalized"))
hist(resid(model2TA23,type="normalized"))

##BIOMASS

datb23<-dat_23%>%
  filter(!is.na(Biomass))

model3TA23 <- lme(
  Biomass ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datb23)

summary(model3TA23)
anova(model3TA23,type="marginal")

plot(fitted(model3TA23),resid(model3TA23,type="normalized"))
plot(datb23$Year,resid(model3TA23,type="normalized"))
plot(datb23$Site,resid(model3TA23,type="normalized"))
plot(datb23$Transect,resid(model3TA23,type="normalized"))
hist(resid(model3TA23,type="normalized"))

###SHANNON
dats23<-dat_23%>%
  filter(!is.na(Shannon))

model4TA23 <- lme(
  Shannon ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = dats23)

summary(model4TA23)
anova(model4TA23,type="marginal")

plot(fitted(model4TA23),resid(model4TA23,type="normalized"))
plot(dats23$Year,resid(model4TA23,type="normalized"))
plot(dats23$Site,resid(model4TA23,type="normalized"))
plot(dats23$Transect,resid(model4TA23,type="normalized"))
hist(resid(model4TA23,type="normalized"))


###RICHNESS
datr23<-dat_23%>%
  filter(!is.na(Richness))

model5TA23 <- lme(
  Richness ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datr23)

summary(model5TA23)
anova(model5TA23,type="marginal")

plot(fitted(model5TA23),resid(model5TA23,type="normalized"))
plot(datr23$Year,resid(model5TA23,type="normalized"))
plot(datr23$Site,resid(model5TA23,type="normalized"))
plot(datr23$Transect,resid(model5TA23,type="normalized"))
hist(resid(model5TA23,type="normalized"))

###NATIVE ABUNDANCE
datn23<-dat_23%>%
  filter(!is.na(NatAbun))

model6TA23 <- lme(
  NatAbun ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datn23)

summary(model6TA23)
anova(model6TA23,type="marginal")

plot(fitted(model6TA23),resid(model6TA23,type="normalized"))
plot(datn23$Year,resid(model6TA23,type="normalized"))
plot(datn23$Site,resid(model6TA23,type="normalized"))
plot(datn23$Transect,resid(model6TA23,type="normalized"))
hist(resid(model6TA23,type="normalized"))

####EVENNESS
date23<-dat_23%>%
  filter(!is.na(Evenness))

model7TA23 <- lme(
  Evenness ~ Site + Yearfac + Transect + Yearfac*Transect + Site*Yearfac+Transect*Site+Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = date23)

summary(model7TA23)
anova(model7TA23,type="marginal")

plot(fitted(model7TA23),resid(model7TA23,type="normalized"))
plot(date23$Year,resid(model7TA23,type="normalized"))
plot(date23$Site,resid(model7TA23,type="normalized"))
plot(date23$Transect,resid(model7TA23,type="normalized"))
hist(resid(model7TA23,type="normalized"))

