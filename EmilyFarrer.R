##### Emily's code #####

dat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/PhragSurvey2017to2022.csv",stringsAsFactors = T,row.names=1)

dat2<-dat%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))
dat2$Transect<-factor(dat2$Transect,levels=c("Native","Transition","Phragmites"))
  



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






##### Ordinations #####

##Barataria all years
datBP<-dat2%>%
  filter(Site=="Barataria"&Year%in%c(2017,2018,2019,2020,2021,2022))
speBP<-datBP%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Barataria
speBPb<-speBP[colSums(speBP>0) > 0]
envBP<-datBP%>%
  select(Year:DeadPhragStems,Yearfac)

#include year?
dbrdaBP<-capscale(speBPb~Transect*Yearfac,distance="bray",data=envBP)
anova(dbrdaBP,by="margin")

site_scoresBP <- data.frame(cbind(envBP,scores(dbrdaBP)$sites,labels=rownames(scores(dbrdaBP)$sites)))

ggplot(site_scoresBP) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  coord_fixed(ratio=1)+
  geom_point(aes(x=CAP1, y=CAP2,color=Transect),alpha=0.7, size=2)+
  stat_ellipse(geom = "polygon", type="t", alpha=0.2, aes(x=CAP1, y=CAP2,fill=Transect),level=.95)+
  facet_wrap(~Year)


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

site_scoresBS <- data.frame(cbind(envBS,scores(dbrdaBS)$sites,labels=rownames(scores(dbrdaBS)$sites)))

ggplot(site_scoresBS) + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (%)") +
  ylab("RDA 2 (%)") +  
  theme_classic()+
  coord_fixed(ratio=1)+
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









