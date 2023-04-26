library(tidyverse)
library(lme4)
install.packages("gitcreds")
library(gitcreds)
gitcreds_set(ghp_Ddfb4AucVntkvuxDwwNIwV10bKOiah0bUYco) #enter your personal access token
library(plotrix)
library(vegan)

#Regressions of Phrag vs. pH

Phrag <- read.csv("PhragSurvey2017to2022.csv", header = TRUE)

PhragN <- Phrag %>% filter(!Phrag$Site %in% c('LUMCON1', 'LUMCON2', 'Fontainebleau', 'Turtle Cove'), !is.na(pH))

hist(PhragN$pH)
boxplot(PhragN$pH)
plot(PhragN$pH, PhragN$Phragmites.australis)

pHreg <- lm(Phragmites.australis ~ pH, data = PhragN)
summary(pHreg)

plot(PhragN$pH, PhragN$Phragmites.australis, xlab = "pH", ylab = "Phragmites Abundance")
abline(pHreg)

pHm <- lm(Phragmites.australis ~ pH + Year + pH*Year, data = PhragN)

##### Ordinations #####

Dat <- Phrag %>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))

Dat$Transect <- factor(Dat$Transect,levels=c("Native","Transition","Phragmites"))

##Pearl River

datPR <- Dat %>%
  filter(Site=="Pearl River"&Year%in%c(2017,2018,2019,2020,2021,2022))

spePR <- datPR %>%
  select(Phragmites.australis:Vigna.luteola)

#Take out Species that didn't exist in Pearl River

spePRb <- spePR[colSums(spePR>0) > 0]

envPR <- datPR %>%
  select(Year:Litter,Yearfac)

#include year?

dbrdaPR <- capscale(spePRb ~ Transect * Yearfac, distance="bray",data=envPR)
anova(dbrdaPR,by="margin")

site_scoresPR <- data.frame(cbind(envPR,scores(dbrdaPR)$sites,labels=rownames(scores(dbrdaPR)$sites)))

ggplot(site_scoresPR) + 
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


## Big Branch remove NA from Dead Phrag Stems

datBB <- Dat %>%
  filter(Site=="Big Branch"&Year%in%c(2017,2018,2019,2020,2021,2022))%>%
  filter(!is.na(DeadPhragStems))

speBB <- datBB %>%
  select(Phragmites.australis:Vigna.luteola)

#Take out Species that didn't exist in Big Branch

speBBb <- speBB[colSums(speBB>0) > 0]

envBB <- datBB %>%
  select(Year:DeadPhragStems,Yearfac)

#include year?
dbrdaBB <- capscale(speBBb~Transect*Yearfac,distance="bray",data=envBB)
anova(dbrdaBB,by="margin")

#Year interaction not significant
dbrdaBB2 <- capscale(speBBb~Transect,distance="bray",data=envBB)
anova(dbrdaBB2,by="margin")

site_scoresBB <- data.frame(cbind(envBB,scores(dbrdaBB2)$sites,labels=rownames(scores(dbrdaBB2)$sites)))

ggplot(site_scoresBB) + 
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


##Pearl River

datPR <- Dat %>%
  filter(Site=="Pearl River"&Year%in%c(2017,2018,2019,2020,2021,2022))

spePR <- datPR %>%
  select(Phragmites.australis:Vigna.luteola)

#Take out Species that didn't exist in Pearl River

spePRb <- spePR[colSums(spePR>0) > 0]

envPR <- datPR %>%
  select(Year:Litter,Yearfac)

#include year?

dbrdaPR <- capscale(spePRb ~ Transect * Yearfac, distance="bray",data=envPR)
anova(dbrdaPR,by="margin")

site_scoresPR <- data.frame(cbind(envPR,scores(dbrdaPR)$sites,labels=rownames(scores(dbrdaPR)$sites)))

ggplot(site_scoresPR) + 
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




