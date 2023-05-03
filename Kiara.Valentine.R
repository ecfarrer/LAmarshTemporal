library(tidyverse)
library(lme4)
library(plotrix)
library(vegan)

install.packages("gitcreds")
library(gitcreds)
gitcreds_set("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") #enter your personal access token

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

##### Restricted permutation tests for repeated measures ordinations #####

#the above models are sketchy because we have repeated measures data and they aren't independent. this is taken from Gavin Simpson's powerpoint and gihub page: https://github.com/gavinsimpson/advanced-vegan-webinar-july-2020



# Going through the tutorial in https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#this is downloaded as "Applied Multivariate Statistics in R" book

#check of permutations, this screws with everything, need to reload envBP
# envBP<-envBP%>%unite("Plot.Year",c(Plot,Yearfac),remove=F)%>%arrange(Plot)
# (h <- how(within = Within(type = "none"), plots = Plots(strata = envBP$Plot, type = "free"),nperm=999))
# envBP[shuffle(nrow(envBP), control = h), c("Plot.Year","Plot", "Yearfac", "Transect")]
# check(envBP,h)

#testing the effect of transect
#note that adonis2 and capscale give slightly different results, so just pick one or the other, using add="lingoes" or "cailliez" to deal with negative eigenvalues changes the results a bit too. if you do lingoes or cailliez capscale and adonis2 give the same result
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBP$Plot, type = "free"),
         nperm=999)
dbrdaBPb <- capscale(speBPb ~ Transect+Plot, distance="bray",data = envBP)#add="lingoes",
anova(dbrdaBPb, permutations = h,by="terms") #model="reduced" permutes the residuals of the model after Condition() is applied 
adonis2(speBPb~Transect+Plot, method="bray",permutations = h,by="terms",data=envBP)#add="lingoes",
#see page 13 in https://cran.r-hub.io/web/packages/permute/vignettes/permutations.pdf
#also see https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#the anova function calculates the F value= variance/Df of the factor divided by the residual variance/df
(2.9642/2)/(22.5167/105)=6.91 #anova
#but this is not correct b/c we want to use not the residual variance but rather the unexplained variation among plots as the denominator, so do the following
(2.9642/2)/(5.7185/18)=4.665 #anova
(2.9115/2)/(5.2919/18)=4.951624#adonis2

#adonis: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBP),shuffleSet(n = nrow(envBP), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 4)
colnames(results) <- c("Transect", "Plot", "Residual","Total")
for (i in 1:nrow(perms)) {
  temp.data <- envBP[perms[i, ], ]
  temp <- adonis2(speBPb ~ Transect + Plot,
                  data = temp.data,
                  method = "bray",
                  permutations = 0)
  results[i, ] <- t(temp$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Transect = (Transect/2)/(Plot/18))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: F=4.95, P=0.003
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))

#capscale: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBP),shuffleSet(n = nrow(envBP), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 3)
colnames(results) <- c("Transect", "Plot", "Residual")
for (i in 1:nrow(perms)) {
  temp.data <- envBP[perms[i, ], ]
  temp<-capscale(speBPb ~ Transect + Plot, data = temp.data, distance = "bray")
  temp2<-anova(temp,permutations = 0,by="terms")
  results[i, ] <- t(temp2$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Transect = (Transect/2)/(Plot/18))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: F=4.95, P=0.003
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))
