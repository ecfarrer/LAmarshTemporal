##### Emily's code #####

library(tidyverse)
library(plotrix)
library(ggthemes)
library(vegan)

options(contrasts=c("contr.helmert","contr.poly"))

save.image("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/LAmarshTemporal/workspace.Rdata")  # 
load("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/LAmarshTemporal/workspace.Rdata")  # 


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
#to get the panels to each have an x andy axis you can also use library(lemon) And change facet_grid(Spher~effSize) to facet_rep_grid(Spher~effSize); https://stackoverflow.com/questions/24161073/show-x-axis-and-y-axis-for-every-plot-in-facet-grid

pdf("phragabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Phragmites australis abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1),axis.line = element_line())+#,legend.position = "none"
  #theme_tufte()+
  geom_hline(yintercept=-Inf,linewidth=.4)+
  geom_vline(xintercept=-Inf,linewidth=.4)+
  coord_cartesian(clip="off")+
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"
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

#Native abundance
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(NatAbun,na.rm=T),se=std.error(NatAbun))
data.frame(dat3)

pdf("natrichness.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Native Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_hline(yintercept=-Inf,linewidth=.4)+
  geom_vline(xintercept=-Inf,linewidth=.4)+
  coord_cartesian(clip="off")+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top")#
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


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
  geom_hline(yintercept=-Inf,linewidth=.4)+
  geom_vline(xintercept=-Inf,linewidth=.4)+
  coord_cartesian(clip="off")+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top")#
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()

dat3<-dat2%>%
  group_by(Site, Transect)%>%
  summarise(mean=mean(Richness,na.rm=T),se=std.error(Richness))
data.frame(dat3)

ggplot(data=dat3, aes(x=Site,y = mean,color=Transect))+
  labs(y="Native Richness") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Transect),strip.position = "top")#,scales="free"


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
  facet_wrap(vars(Site),strip.position = "top")
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()


#Evenness
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Evenness,na.rm=T),se=std.error(Evenness))
data.frame(dat3)

#pdf("Shannon.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Shannon diversity") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top")
#dev.off()



#Litter
dat3<-dat2%>%
  group_by(Year, Site)%>%
  summarise(mean=mean(Litter,na.rm=T),se=std.error(Litter))
data.frame(dat3)

#pdf("Shannon.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Site))+
  labs(y="Shannon diversity") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)
#  facet_wrap(vars(Site),strip.position = "top")
#dev.off()


##### Salinity over time ######

#Salinity
dat3<-dat2%>%
  group_by(Year, Transect, Site)%>%
  summarise(mean=mean(Salinity15cmppt,na.rm=T),se=std.error(Salinity15cmppt,na.rm=T))
data.frame(dat3)

ggplot(dat3,aes(x=Year,y=mean,col=Transect))+
  labs(y="Salinity (ppt)") +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.15,linewidth=.8) +
  facet_wrap(~Site)#,scales="free"

#pretty fig
#to get the panels to each have an x andy axis you can also use library(lemon) And change facet_grid(Spher~effSize) to facet_rep_grid(Spher~effSize); https://stackoverflow.com/questions/24161073/show-x-axis-and-y-axis-for-every-plot-in-facet-grid

pdf("phragabun.pdf",width=6.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Salinity (ppt)") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1),axis.line = element_line())+#,legend.position = "none"
  #theme_tufte()+
  geom_hline(yintercept=-Inf,linewidth=.4)+
  geom_vline(xintercept=-Inf,linewidth=.4)+
  coord_cartesian(clip="off")+
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"
#geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()






##### Plotting Ordinations #####

#See the "RestrictedPermutationsforOrdinations.R" file for the statistical tests

temp<-dat2%>%
  select(Phragmites.australis:Vigna.luteola)%>%
  filter(!is.na(Phragmites.australis))
min(temp)
max(temp)

##### Ordination - Barataria #####

##Barataria all years
#questions I need to decide on: 
  #relative abundance vs absolute abundance (I used relative abun before)
  #bray vs jaccard (I used jaccard before)
  #dbRDA vs CCA vs NMDS
datBP<-dat2%>%
  filter(Site=="Barataria"&Year%in%c(2017,2018,2019,2020,2021,2022))
speBP<-datBP%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Barataria
speBPb<-speBP[colSums(speBP>0) > 0]
envBP<-datBP%>%
  select(Year:DeadPhragStems,Yearfac)
#take out eleocharis
#speBPb<-select(speBPb,-Eleocharis.sp.)
#calculate relative abundance
speBPc<-decostand(speBPb,method="log",logbase=10)
#speBPd<-speBPc/rowSums(speBPc)

#capscale and dbrda are doing the same thing.
dbrdaBP <- capscale(vegdist(speBPc,method="bray",binary=F) ~ Transect*Yearfac,data = envBP)
#dbrdaBP <- capscale(speBPc ~ Transect*Yearfac,distance="bray",data = envBP) #these are the same
summary(dbrdaBP)
#plot(dbrdaBP)

#plotting
site_scoresBP <- data.frame(cbind(envBP,scores(dbrdaBP)$sites,labels=rownames(scores(dbrdaBP)$sites)))

#Plot across years, use this
pdf("OrdinationBarataria.pdf",width=7,height=4)
ggplot(site_scoresBP) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (15.1%)") +  # 
  ylab("RDA 2 (6.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()

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
  filter(Site=="Bayou Sauvage"&Year%in%c(2017,2018,2019,2020,2021,2022))%>% #
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
speBS<-datBS%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Bayou Sauvage
speBSb<-speBS[colSums(speBS>0) > 0]
envBS<-datBS%>%
  select(Year:DeadPhragStems,Yearfac)
speBSc<-decostand(speBSb,method="log",logbase=10)
#speBSd<-speBSc/rowSums(speBSc)

#for the analysis we might need to subset the data so we have all years for eachplot, not sure that we need to do this for the figure and it actually doesn't change much anyway so I will not do it now.
#got an error here that the design wasn't balanced. so trying to reduce the dataset to make it balanced. don't have 112, 113 in 2018, no 113 in 2019, 120 in 2020, 111 in 2022.
#107,108,109,110,111,112,113 Phrag
#114,115,116,117,118,119,120 Trans
#121,122,123,124,125,126,127 Nat
#107,108,109,110 Phrag
#114,115,116,117,118,119 Trans
#121,122,123,124,125,126,127 Nat
#ind<-which(envBS$Plot%in%c(107,108,109,110,114,115,116,117,118,119,121,122,123,124,125,126,127)) #these are all plots that are present in all years
#ind<-which(envBS$Plot%in%c(107,108,109,110,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127)) #these are all plots that are present in 2017 and 2022
#ind<-which(envBS$Plot%in%c(107,108,109,110,112,113,114,115,116,117,119,120,121,122,123,124,126,127)) #these are all plots that are present in 2017 and 2022 and making it balanced, so deleting a plot in T and N. I deleted the plots in the same line as 111 (118 and 152)
#ind<-which(envBS$Plot%in%c(107,108,109,110,114,115,116,117,121,122,123,124))
#speBSd<-speBSc[ind,]
#envBSb<-envBS[ind,]

#capscale
dbrdaBS <- capscale(vegdist(speBSc,method="bray",binary=F) ~ Transect*Yearfac,data = envBS)
summary(dbrdaBS)

site_scoresBS <- data.frame(cbind(envBS,scores(dbrdaBS)$sites,labels=rownames(scores(dbrdaBS)$sites)))

pdf("OrdinationBayouSauvage.pdf",width=7,height=4)
ggplot(site_scoresBS) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (33.6%)") +
  ylab("RDA 2 (10.3%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()




##### Ordination - Big Branch all years #####
datBB<-dat2%>%
  filter(Site=="Big Branch"&Year%in%c(2017,2018,2019,2020,2021,2022))
speBB<-datBB%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Big Branch
speBBb<-speBB[colSums(speBB>0) > 0]
envBB<-datBB%>%
  select(Year:DeadPhragStems,Yearfac)
speBBc<-decostand(speBBb,method="log",logbase=10)
#doing relative abundance after log transformation hardly changes anything in the ordination plot. doing log transformation on raw data and comparing to raw data plot is SUPER different - the rel abun ordination looks like the log transformed ordination, so the original raw ordination is basically showing differences in abundance
#speBBd<-speBBc/rowSums(speBBc)

dbrdaBB <- capscale(vegdist(speBBc,method="bray",binary=F) ~ Transect*Yearfac,data = envBB)
summary(dbrdaBB)

site_scoresBB <- data.frame(cbind(envBB,scores(dbrdaBB)$sites,labels=rownames(scores(dbrdaBB)$sites)))

pdf("OrdinationBigBranch.pdf",width=7,height=4)
ggplot(site_scoresBB) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (23.6%)") +
  ylab("RDA 2 (8.4%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()


##### Ordination - Pear River all years #####
#2019 is weird b/c all the phrag plots had lots of eleocharis and none of the other plots did
datPR<-dat2%>%
  filter(Site=="Pearl River"&Year%in%c(2017,2018,2019,2020,2021,2022))
spePR<-datPR%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in PR
spePRb<-spePR[colSums(spePR>0) > 0]
envPR<-datPR%>%
  select(Year:DeadPhragStems,Yearfac)
spePRc<-decostand(spePRb,method="log",logbase=10)
#spePRd<-spePRc/rowSums(spePRc)
  
dbrdaPR <- capscale(vegdist(spePRc,method="bray",binary=F) ~ Transect*Yearfac,data = envPR)
summary(dbrdaPR)

site_scoresPR <- data.frame(cbind(envPR,scores(dbrdaPR)$sites,labels=rownames(scores(dbrdaPR)$sites)))

pdf("OrdinationPearlRiver.pdf",width=7,height=4)
ggplot(site_scoresPR) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (17.9%)") +
  ylab("RDA 2 (9.8%)") +  
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()




##### Ordination - All sites/all transects just 2017 and 2022 #####

datTY<-dat2%>%
  filter(Year%in%c(2017,2022))%>% 
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
speTY<-datTY%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist 
speTYb<-speTY[colSums(speTY>0) > 0]
envTY<-datTY%>%
  select(Year:DeadPhragStems,Yearfac)
envTY$SiteTransect<-paste(envTY$Site,envTY$Transect,sep=".")
speTYc<-decostand(speTYb,method="log",logbase=10)
#calculate relative abundance
#speBPd<-speBPc/rowSums(speBPc)

#capscale and dbrda are doing the same thing.
dbrdaTY <- capscale(vegdist(speTYc,method="bray",binary=F)~ Site*Transect*Yearfac,data = envTY,add="lingoes")
#dbrdaTY <- capscale(sqrt(vegdist(speTYc,method="bray",binary=F)) ~ Site*Transect*Yearfac,data = envTY) #trying sqrt of bray so that it is metric and no negative eiganvalues, this is very very similar to just the regular bray output
#dbrdaTY <- capscale(speTYc ~ Site*Transect*Yearfac,distance="bray",data = envTY,add="lingoes") #this is the same as the first model, ,add="lingoes"
summary(dbrdaTY)
#plot(dbrdaBP)

#explains that lingeos adds random variation to the dissimilarities: https://rdrr.io/cran/vegan/man/varpart.html

#Plotting
site_scoresTY <- data.frame(cbind(envTY,scores(dbrdaTY)$sites,labels=rownames(scores(dbrdaTY)$sites)))

hullTY <- site_scoresTY %>%
  group_by(Transect,Site,Year) %>%
  slice(chull(CAP1,CAP2))

#using lingoes USE THIS
pdf("Ordination1722lingoeshulls.pdf",width=6,height=6)
ggplot(site_scoresTY) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1") +  # 
  ylab("RDA 2") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  #stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_polygon(data=hullTY,aes(x=CAP1,y=CAP2, fill=Transect,colour = Transect),alpha=.2)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect), size=2)+
  #facet_wrap(~Site*Year)
  facet_grid(rows=vars(Site),cols=vars(Year))
dev.off()

#using lingeos with ellipses
pdf("Ordination1722lingoesellipses.pdf",width=6,height=6)
ggplot(site_scoresTY) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1") +  # 
  ylab("RDA 2") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  #geom_polygon(data=hullTY,aes(x=CAP1,y=CAP2, fill=Transect,colour = Transect),alpha=.2)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect), size=2)+
  #facet_wrap(~Site*Year)
  facet_grid(rows=vars(Site),cols=vars(Year))
dev.off()

#using raw bray curtis
pdf("Ordination1722.pdf",width=6,height=6)
ggplot(site_scoresTY) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (26.1%)") +  # 
  ylab("RDA 2 (17.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  #stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect,color=Transect),level=.95)+
  geom_polygon(data=hullTY,aes(x=CAP1,y=CAP2, fill=Transect,colour = Transect),alpha=.2)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect), size=2)+
  #facet_wrap(~Site*Year)
  facet_grid(rows=vars(Site),cols=vars(Year))
dev.off()


#Variance explained by axes using legendre and legendre chapter 9 2012
#this is just checking some of the equations that were in the legendre and legendre chapter 9 2012 book
temp<-dbrda(vegdist(speTYc,method="bray",binary=F)~ Site*Transect*Yearfac,data = envTY)
dtrans<-sqrt((vegdist(speTYc,method="bray",binary=F))^2+2*0.794282) #the lingoes transformation with .7942 as the constant
temp3<-dbrda(dtrans~ Site*Transect*Yearfac,data = envTY)
temp2<-dbrda(vegdist(speTYc,method="bray",binary=F)~ Site*Transect*Yearfac,data = envTY,add="lingoes")
min(temp$CA$eig)=0.794282
(15.432+1*0.794282)/(46.7927+(167-1)*0.794282) #equation page 506, this just gives you the "new" variance explained using raw bray curtis, and converts it to the lingoes version. this is not that useful b/c we can just run the lingoes model adn get this same number. but it made me understand exactly how the constant is added to the distance matrix
17.054/316.0547

#from https://github.com/vegandevs/vegan/blob/0ddfa4e52444de119bf5baae200eef0c05b12c23/R/GowerDblcen.R#L40
#This does what it says it does, and gives the c constant that is used in add="lingoes" 1.622) but it is odd that we can add a smaller constant (the max negative eigenvalue from a dbrda of the original data, 0.794) and still get a solution that does not have negative eigenvalues. maybe it is because this is doing a pcoa (unconstrained) whereas the dbrda is obviously constrained! yes! that is exactly what it is doing! see temp code right below
temp<-dbrda(vegdist(speTYc,method="bray",binary=F)~ 1,data = envTY)
min(temp$CA$eig)

#these two functions were found on the github vegan respository. 
GowerDblcen <- function(x, na.rm = TRUE)
{
  cnt <- colMeans(x, na.rm = na.rm)
  x <- sweep(x, 2L, cnt, check.margin = FALSE)
  cnt <- rowMeans(x, na.rm = na.rm)
  sweep(x, 1L, cnt, check.margin = FALSE)
}

addLingoes <- function(d)
{
  ## smallest negative eigenvalue (or zero)
  d <- -GowerDblcen(d^2)/2
  e <- eigen(d, symmetric = TRUE, only.values = TRUE)$values
  out <- min(e)
  max(-out, 0)
}
d<-as.matrix(vegdist(speTYc,method="bray",binary=F))
temp2<-addLingoes(as.matrix(vegdist(speTYc,method="bray",binary=F)))








##### Phrag transects all years #####
datPhrag<-dat2%>%
  filter(Transect=="Phragmites"&Year%in%c(2017,2022))%>% 
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
spePhrag<-datPhrag%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist 
spePhragb<-spePhrag[colSums(spePhrag>0) > 0]
envPhrag<-datPhrag%>%
  select(Year:DeadPhragStems,Yearfac)
spePhragc<-decostand(spePhragb,method="log",logbase=10)
#calculate relative abundance
#speBPd<-speBPc/rowSums(speBPc)

#capscale and dbrda are doing the same thing.
dbrdaPhrag <- capscale(vegdist(spePhragc,method="bray",binary=F) ~ Yearfac,data = envPhrag)
#dbrdaBP <- capscale(speBPc ~ Transect*Yearfac,distance="bray",data = envBP) #these are the same
summary(dbrdaPhrag)
#plot(dbrdaBP)

#plotting
site_scoresPhrag <- data.frame(cbind(envPhrag,scores(dbrdaPhrag)$sites,labels=rownames(scores(dbrdaPhrag)$sites)))

#Plot across years, use this
#pdf("OrdinationPhrag.pdf",width=7,height=4)
ggplot(site_scoresPhrag) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (15.1%)") +  # 
  ylab("RDA 2 (6.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=MDS1,fill=Site,color=Site),level=.95)+
  geom_point(aes(x=CAP1, y=MDS1,color=Site),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()

ggplot(site_scoresPhrag) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (15.1%)") +  # 
  ylab("RDA 2 (6.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=MDS1,fill=Yearfac,color=Yearfac),level=.95)+
  geom_point(aes(x=CAP1, y=MDS1,color=Yearfac),alpha=0.7, size=2)+
  facet_wrap(~Site)




##### Transition transects all years #####
datTrans<-dat2%>%
  filter(Transect=="Transition"&Year%in%c(2017,2018,2019,2020,2021,2022))%>% 
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
speTrans<-datTrans%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist 
speTransb<-speTrans[colSums(speTrans>0) > 0]
envTrans<-datTrans%>%
  select(Year:DeadPhragStems,Yearfac)
speTransc<-decostand(speTransb,method="log",logbase=10)
#calculate relative abundance
#speBPd<-speBPc/rowSums(speBPc)

#capscale and dbrda are doing the same thing.
dbrdaTrans <- capscale(vegdist(speTransc,method="bray",binary=F) ~ Transect*Yearfac,data = envTrans)
#dbrdaBP <- capscale(speBPc ~ Transect*Yearfac,distance="bray",data = envBP) #these are the same
summary(dbrdaTrans)
#plot(dbrdaBP)

#plotting
site_scoresTrans <- data.frame(cbind(envTrans,scores(dbrdaTrans)$sites,labels=rownames(scores(dbrdaTrans)$sites)))

#Plot across years, use this
pdf("OrdinationTrans.pdf",width=7,height=4)
ggplot(site_scoresTrans) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (15.1%)") +  # 
  ylab("RDA 2 (6.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Site,color=Site),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Site),alpha=0.7, size=2)+
  facet_wrap(~Year)
dev.off()

ggplot(site_scoresTrans) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (15.1%)") +  # 
  ylab("RDA 2 (6.7%)") +  # 
  theme_classic()+
  theme(strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),panel.border = element_rect(fill = NA))+
  #coord_fixed(ratio=1)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Yearfac,color=Yearfac),level=.95)+
  geom_point(aes(x=CAP1, y=CAP2,color=Yearfac),alpha=0.7, size=2)+
  facet_wrap(~Site)



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
anova(modelT,type="marginal")

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



#native abundance
#  weights = varExp(form =~ Year),
modelT2 <- lme(
  NatAbun ~ Site + Yearfac + Transect + Yearfac*Transect + Yearfac*Site + Site*Transect+Site*Transect*Yearfac,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = datp)
summary(modelT2)
anova(modelT2,type="marginal")

