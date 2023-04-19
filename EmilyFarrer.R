##### Emily's code #####

dat2<-dat%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))

##### Species abundances over time ######
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
pdf("phragabun.pdf",width=4.5,height=4.5)
ggplot(data=dat3, aes(x=Year,y = mean,color=Transect))+
  labs(y="Phragmites australis abundance") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),legend.position = "none",strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#
  geom_point(size=1.8)+
  geom_line()+
  geom_errorbar(aes(ymax = mean+se, ymin=mean-se),width=.25,size=.5)+
  facet_wrap(vars(Site),strip.position = "top",scales="free")
  #geom_text(aes(y = survivalpercent+se, label = c(" "," "," "," ","a","ab","b","ab"),x = Treatment),colour="black", size=3,vjust = -1)
dev.off()

