#Regressions

library(tidyverse)
library(nlme)
library(lemon)

dat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/PhragSurvey2017to2023.csv",stringsAsFactors = T,row.names=1)

dat2<-dat%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))%>%
  mutate(Sitelab=case_match(Site,"Barataria"~"a) Barataria","Pearl River"~"b) Pearl River","Bayou Sauvage"~"c) Bayou Sauvage","Big Branch"~"d) Big Branch",.ptype = factor(levels = c("a) Barataria","b) Pearl River","c) Bayou Sauvage","d) Big Branch"))))
dat2$Transect<-factor(dat2$Transect,levels=c("Native","Transition","Phragmites"))
dat2$Plot<-factor(dat2$Plot)
dat2$Site<-factor(dat2$Site,levels=c("Barataria","Pearl River","Bayou Sauvage","Big Branch"))


##### Figures #####

#Raw data, axes logged
#Use this, I like this

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/nativevsphrag.pdf",width=4.8,height=4)
ggplot(data=dat2, aes(x=Phragmites.australis+1,y = NatAbun+1,linetype=Sitelab))+#,color=Transect    
  #labs(x=expression(paste(italic("Phragmites")," density (stems"~m^{-2},")")),y="Native stem density") +
  labs(x=expression(italic("Phragmites")~density~(stems/m^{2})),y=expression(Native~density~(stems/m^{2})))+
  #labs(y=expression(paste('flux ','(g ',CO[2]~m^{-2}~h^{-1},')')))+
  #labs(y=expression(flux*phantom(x)*(g~CO[2]~m^{-2}~h^{-1})))+
  theme_classic()+
  scale_x_log10()+
  scale_y_log10()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line(),legend.position="none")+#,axis.text.x = element_text(angle = 35, vjust=1, hjust=1)
  geom_point(size=.8,color=gray(.4))+
  geom_smooth(method="lm",se=F,color="black",size=.5)+
  scale_linetype_manual(values=c("dashed","dashed","solid","solid"))+
  facet_wrap(vars(Sitelab),strip.position = "top")#,scales="free"
dev.off()

#using cowplot, but then you have to create 4 separate figures
#plot_grid(ordlistp[[1]],ordlistp[[2]],ordlistp[[3]],ordlistp[[4]],ordlistp[[5]],ordlistp[[6]],ordlistp[[7]],ordlistp[[8]],legend, nrow = 1)



#ln scale

head(dat2)

pdf("natabunvsphrag.pdf",width=6.5,height=4.5)
ggplot(data=dat2, aes(x=log(Phragmites.australis+1),y = log(NatAbun+1)))+#,color=Transect
  labs(x="Phragmites stem density (log scale)",y="Native stem density (log scale)") +
  #ylim(20,110)+
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  scale_linetype_manual(values=c("dashed","dashed","solid","solid"))+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"
dev.off()



##### Stats #####

#Full model 

head(dat2)
dat2$NatAbunlog<-log10(dat2$NatAbun+1)
dat2$Phragmites.australislog<-log10(dat2$Phragmites.australis+1)

m1 <- lme(
  NatAbunlog ~ Site + Phragmites.australislog + Site*Phragmites.australislog+Yearfac + Yearfac*Site ,
  correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = dat2,na.action=na.omit)
summary(m1)
anova(m1,type="marginal")

hist(resid(m1,type="normalized"))
plot(dat2$Phragmites.australislog[is.na(dat2$Phragmites.australislog)==F],resid(m1,type="normalized"))


#Separate regressions
BPreg<-dat2%>%
  filter(Site=="Barataria")
m1<-lme(NatAbunlog ~ Phragmites.australislog + Yearfac,
        correlation = corAR1(form =~ Year|Plot),
        random=~1|Plot, data=BPreg, na.action=na.omit)
#if I take out fixed effect of year, than phrag is significant
# m1<-lme(NatAbunlog ~ Phragmites.australislog ,
#         correlation = corAR1(form =~ Year|Plot),
#         random=~1|Plot, data=BPreg, na.action=na.omit)
#summary(m1)
anova(m1,type="marginal")

PRreg<-dat2%>%
  filter(Site=="Pearl River")
m1<-lme(NatAbunlog ~ Phragmites.australislog + Yearfac,
        correlation = corAR1(form =~ Year|Plot),
        random=~1|Plot, data=PRreg, na.action=na.omit)
# m1<-lme(NatAbunlog ~ Phragmites.australislog,
#         correlation = corAR1(form =~ Year|Plot),
#         random=~1|Plot, data=PRreg, na.action=na.omit)
#summary(m1)
anova(m1,type="marginal")

BSreg<-dat2%>%
  filter(Site=="Bayou Sauvage")
m1<-lme(NatAbunlog ~ Phragmites.australislog + Yearfac,
        correlation = corAR1(form =~ Year|Plot),
        random=~1|Plot, data=BSreg, na.action=na.omit)
#summary(m1)
anova(m1,type="marginal")

BBreg<-dat2%>%
  filter(Site=="Big Branch")
m1<-lme(NatAbunlog ~ Phragmites.australislog + Yearfac,
        correlation = corAR1(form =~ Year|Plot),
        random=~1|Plot, data=BBreg, na.action=na.omit)
#summary(m1)
anova(m1,type="marginal")


