##### Emily's code #####

dat2<-dat%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))
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
