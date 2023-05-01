##### Emily's code #####


options(contrasts=c("contr.helmert","contr.poly"))

dat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/PhragSurvey2017to2022.csv",stringsAsFactors = T,row.names=1)

dat2<-dat%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))
dat2$Transect<-factor(dat2$Transect,levels=c("Native","Transition","Phragmites"))
dat2$Plot<-factor(dat2$Plot)
dat2$Site<-factor(dat2$Site,levels=c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))


##### Species abundances over time ######

#Phragmites
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Phragmites.australis,na.rm=T),se=std.error(Phragmites.australis))
data.frame(dat3)

ggplot(dat3,aes(x=Year,y=mean,col=Transect))+
  labs(y="Phragmites australis abundance") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)#,scales="free"

#pretty fig
pdf("phragabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Phragmites australis abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
  #geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()



#Spartina patens
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Spartina.patens,na.rm=T),se=std.error(Spartina.patens))
data.frame(dat3)

pdf("spapatabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Spartina patens abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


#Schoenoplectus americanus
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Schoenoplectus.americanus,na.rm=T),se=std.error(Schoenoplectus.americanus))
data.frame(dat3)

pdf("schameabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Schoenoplectus americanus abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


#Eleocharis spp.
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Eleocharis.sp.,na.rm=T),se=std.error(Eleocharis.sp.))
data.frame(dat3)

pdf("elesppabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Eleocharis spp. abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


#Sagittaria lancifolia
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Sagittaria.lancifolia,na.rm=T),se=std.error(Sagittaria.lancifolia))
data.frame(dat3)

pdf("saglanabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Sagittaria lancifolia abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()




###### Diversity #####

#Native Richness
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(NatRichness,na.rm=T),se=std.error(NatRichness))
data.frame(dat3)

pdf("natrichness.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Native Richness") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


#Shannon
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Shannon,na.rm=T),se=std.error(Shannon))
data.frame(dat3)

pdf("Shannon.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Shannon diversity") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()






##### Plotting Ordinations #####

#See the "RestrictedPermutationsforOrdinations.R" file for the statistical tests

##### Ordination - Barataria #####

##Barataria all years
datBP<-dat2%>%
  filter(Site=="Barataria"&Year%in%c(2017,2018,2019,2020,2021,2022))
speBP<-datBP%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Barataria
speBPb<-speBP[colSums(speBP>0) > 0]
envBP<-datBP%>%
  select(Year:DeadPhragStems,Yearfac)

dbrdaBP <- capscale(speBPb ~ Transect*Yearfac, distance="bray",data = envBP)

#plotting
site_scoresBP <- data.frame(cbind(envBP,scores(dbrdaBP)$sites,labels=rownames(scores(dbrdaBP)$sites)))

#export using png default
ggplot(site_scoresBP) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (6.4%)") +
  ylab("RDA 2 (1.6%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect),level=.95)+
  facet_wrap(~Year)

#Plot with facet wrap by transect, color by year
ggplot(site_scoresBP) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  geom_point(aes(x=CAP1, y=CAP2,color=Year),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Yearfac),level=.95)+
  facet_wrap(~Transect)




##### Ordination - Bayou Sauvage #####
##Bayou Sauvage all years, needed to take out plots that had zero plants
datBS<-dat2%>%
  filter(Site=="Bayou Sauvage"&Year%in%c(2017,2018,2019,2020,2021,2022))%>%
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
speBS<-datBS%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Bayou Sauvage
speBSb<-speBS[colSums(speBS>0) > 0]
envBS<-datBS%>%
  select(Year:DeadPhragStems,Yearfac)

#include year?
dbrdaBS<-capscale(speBSb~Transect*Yearfac,distance="bray",data=envBS)
anova(dbrdaBS,by="margin")

#got an error here that the design wasn't balanced. so trying to reduce the dataset to make it balanced. don't have 112, 113 in 2018, no 113 in 2019, 120 in 2020, 111 in 2022.
#107,108,109,110,111,112,113 Phrag
#114,115,116,117,118,119,120 Trans
#121,122,123,124,125,126,127 Nat
#107,108,109,110 Phrag
#114,115,116,117,118,119 Trans
#121,122,123,124,125,126,127 Nat
ind<-which(envBS$Plot%in%c(107,108,109,110,114,115,116,117,118,119,121,122,123,124,125,126,127))
#ind<-which(envBS$Plot%in%c(107,108,109,110,114,115,116,117,121,122,123,124))
speBSc<-speBSb[ind,]
envBSb<-envBS[ind,]

dbrdaBSb <- capscale(speBSc ~ Yearfac + Yearfac:Transect + Condition(Plot), data = envBSb)
(h <- how(within = Within(type = "none"), plots = Plots(strata = envBSb$Plot, type = "free"),nperm=999))
anova(dbrdaBSb, permutations = h, model = "reduced") #reduced permutes the residuals of the model after Condition() is applied
anova(dbrdaBSb, permutations = h, model = "reduced",by="terms") #I'm not sure about this, I'm not sure if we are testing the effect of year based on the permutation scheme

site_scoresBS <- data.frame(cbind(envBS,scores(dbrdaBS)$sites,labels=rownames(scores(dbrdaBS)$sites)))

ggplot(site_scoresBS) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect),level=.95)+
  facet_wrap(~Year)



### old ordination stuff
head(dat2)

datBP2017<-dat2%>%
  filter(Site=="Barataria"&Year==2017)
speBP2017<-datBP2017%>%
  select(Phragmites.australis:Vigna.luteola)
speBP2017b<-speBP2017[colSums(speBP2017>0) > 0]
envBP2017<-datBP2017%>%
  select(Year:DeadPhragStems)

dbrdaBP2017<-capscale(speBP2017b~Transect,distance="bray",data=envBP2017)
site_scoresBP2017 <- data.frame(cbind(envBP2017,scores(dbrdaBP2017)$sites,labels=rownames(scores(dbrdaBP2017)$sites)))
#species_scores<-data.frame(scores(mydbrda)$species,labels=rownames(scores(mydbrda)$species))
#reg_scores<-data.frame(scores(mydbrda,display="bp"),labels=rownames(scores(mydbrda,display="bp")))

ggplot(site_scoresBP2017) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  coord_fixed(ratio=1)+
  geom_point(data=site_scoresBP2017, aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect),level=.95)


datBP2018<-dat2%>%
  filter(Site=="Barataria"&Year==2018)
speBP2018<-datBP2018%>%
  select(Phragmites.australis:Vigna.luteola)
speBP2018b<-speBP2018[colSums(speBP2018>0) > 0]
envBP2018<-datBP2018%>%
  select(Year:DeadPhragStems)

dbrdaBP2018<-capscale(speBP2018b~Transect,distance="bray",data=envBP2018)
site_scoresBP2018 <- data.frame(cbind(envBP2018,scores(dbrdaBP2018)$sites,labels=rownames(scores(dbrdaBP2018)$sites)))

ggplot(site_scoresBP2018) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  coord_fixed(ratio=1)+
  geom_point(data=site_scoresBP2018, aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect),level=.95)



# theme(legend.position = "none")+
#   scale_color_manual(values = c("#56ae6c", "#8960b3"),labels = c("Native", "Phragmites"),name = "Invasion")+
#   scale_fill_manual(values = c("#56ae6c", "#8960b3"),labels = c("Native", "Phragmites"),name = "Invasion")+








###### lme models ######
#distances are in lat/long degrees so 0.0001 is about 10m

datp<-dat2%>%
  filter(!is.na(Phragmites.australis))
# datp<-dat2%>%
#   filter(Transect%in%c("Phragmites","Transition"))

model1 <- gls(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Transect*Site,
  data = datp)
summary(model1)

modelR <- lme(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Transect*Site,
  random=~1|Plot,data = datp)
summary(modelR)

modelT <- lme(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Transect*Site,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(modelT)

modelS <- gls(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Transect*Site,
  correlation=corExp(form = ~ Lat+Long|Yearfac,nugget=T),#
  data = datp)
summary(modelS)
plot(Variogram(modelS))

#to see if coefficients are NA if you are getting a singularity error
# test<-lm(Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect+Site*Yearfac+Site*Transect,data=dat2)
# coef(test)

anova(model1,modelR,modelT,modelS)

anova(modelT,type="marginal")

modelT2 <- lme(
  Phragmites.australis ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Transect*Site + Transect*Site*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(modelT2)
anova(modelT2,type="marginal")



#can't do this, plot is completely determined by lat/long so redundant?
# modelS <- lme(
#   Phragmites.australis ~ Site + Year + Transect + Year*Transect,
#   correlation=corSpher(form = ~ Lat+Long|Year),#,nugget=T
#   random=~1|Plot,data = datp)




#sanity check, using 2017 only
datp2<-dat2%>%
  filter(is.na(Phragmites.australis)==F&Year==2017&Transect%in%c("Phragmites","Transition"))#&Site=="Big Branch"

modelE <- gls(
  Phragmites.australis ~ Transect*Site,
  correlation=corExp(c(1.3,.4),form = ~ Lat+Long,nugget=T),#,nugget=T
  data = datp2)
summary(modelE)
plot(Variogram(modelE),ylim=c(0,1.3), main = "Exponential Correlation")

model1<-gls(
  Phragmites.australis ~ Site  + Transect,
  data=datp2)

anova(model1,modelE)







datp<-dat2%>%
  filter(!is.na(Shannon))

model1 <- gls(
  NatShannon ~ Site + Yearfac + Transect + Yearfac*Transect+ Yearfac*Site + Transect*Site,
  data = datp)
summary(model1)

modelR <- lme(
  NatShannon ~ Site + Yearfac + Transect + Yearfac*Transect+ Yearfac*Site + Transect*Site,
  random=~1|Plot,data = datp)

modelS <- gls(
  NatShannon ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Site*Transect,
  correlation=corExp(form = ~ Lat+Long|Yearfac,nugget=T),#
  data = datp)
summary(modelS)
plot(Variogram(modelS),ylim=c(0,1.2), main = "Spherical Correlation")

modelT <- lme(
  NatShannon ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Site*Transect,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(modelT)

anova(model1,modelR,modelS,modelT)

anova(modelT,type="marginal")

#  weights = varExp(form =~ Year),
modelT2 <- lme(
  Shannon ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Site*Transect+Site*Transect*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(modelT2)
anova(modelT2,type="marginal")

plot(fitted(modelT2),resid(modelT2,type="normalized"))
plot(datp$Year,resid(modelT2,type="normalized"))
plot(datp$Site,resid(modelT2,type="normalized"))
plot(datp$Transect,resid(modelT2,type="normalized"))
hist(resid(modelT2,type="normalized"))

#Tukey posthoc tests, need to take out unused levels of Site (see very top of code)
library(multcomp)
modelT2 <- lme(
  Shannon ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Site*Transect+Site*Transect*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(glht(modelT2,linfct=mcp(Site="Tukey")))




