# Jacob Dixon's code
setwd("C:/Users/The_D/OneDrive/Documents/LAmarshTemporal")

install.packages("gitcreds")
library(gitcreds)
gitcreds_set()

# Directory is through the OneDrive
# install and/or load necessary packages
install.packages("tidyverse") # for piping protocols (%>%)
library(tidyverse)
library(ggplot2)
library(vegan)
library(gridExtra)
library(nlme)

# Read in data
PhragData <- read.csv("PhragSurvey2017to2022.csv")

#### Comparison of Phragmites abundance and water depth ----
# Remove NAs and Abandoned sites
PhragData_narm <- PhragData %>%
  filter(!is.na(WaterDepthcm), !is.na(Phragmites.australis),
         !is.na(Site), !is.na(Transect), !is.na(pH),
         Site!="LUMCON 1", Site!="LUMCON 2", 
         Site!="Fontainebleau", Site!="Turtle Cove")

# Summarize based on Site and Transect. We will ignore year for now since
# I am mostly interested in the relationship independent of year. 
#install.packages("plotrix") # for std.error() function
library("plotrix")

# Having trouble with mutate, change Site & transect to a factor manually
PhragData_narm$Site <- as.factor(PhragData_narm$Site)
PhragData_narm$Transect <- as.factor(PhragData_narm$Transect)

# Summarize Phrag abundance and water Depth by Site and Transect across all years
PhragData_meanPhrag <- PhragData_narm %>%
  group_by(Site,Transect) %>%
  summarise(mean=mean(Phragmites.australis), se=std.error(Phragmites.australis))

PhragData_meansWatDep <- PhragData_narm %>%
  group_by(Site,Transect) %>%
  summarise(mean=mean(WaterDepthcm), se=std.error(WaterDepthcm))


# Plot creation and plot combination
plot1 <- ggplot(data = PhragData_meanPhrag, aes(x=Transect,y=mean, fill=Site)) +
  geom_bar(stat = "identity") +
  labs(x = "Transect",y="Phrag Abundance") +
  ylim(0,17) + theme(legend.position = "none")  +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25) +
  facet_wrap(~Site,scales = "free_x")

plot2 <- ggplot(data = PhragData_meansWatDep, 
  aes(x=Transect,y=mean, fill=Site)) +
  geom_bar(stat = "identity") +
  labs(x = "Transect",y="Water Depth (cm)")+
  ylim(0,23) + theme(legend.position = "none")  +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25) +
  facet_wrap(~Site,scales = "free_x")

# install.packages("gridExtra") # to combine ggplots into one
library(gridExtra)

grid.arrange(plot1, plot2, ncol=2)


#### Ordination Plot (all species) ----
# Let's do Constrained Ordination - we want to test for the effect of a 
# treatment on species composition. We'll probably do a dbRDA.

# We can use PhragData_narm - we'll need a data frame with just the species
# as a row with column of abundance data.
# Columns 11-74 are species (some unknown)

spec <- PhragData_narm[,11:74]
envir <- PhragData_narm[,1:10]

#install.packages("vegan")
library(vegan)

Phrag_dbrda <- capscale(spec~pH+Salinity15cmppt+WaterDepthcm+Biomass+Litter, 
                     data=envir,distance="bray",na.action = na.exclude)

summary(Phrag_dbrda)

# Let's Plot
# "Ugly" Plot
plot(Phrag_dbrda)

# Time for serious plotting. Following Lecture14script from class.
library(ggplot2)
library(ggrepel)

# Looks like PhragData_narm but with dbRDA1&2 scores
site_scores <- data.frame(cbind(envir,scores(Phrag_dbrda)$site,
                                labels=rownames(scores(Phrag_dbrda)$site)))

# CAP 1 & 2 scores for each species
species_scores <- data.frame(scores(Phrag_dbrda)$species,
                           labels=rownames(scores(Phrag_dbrda)$species))

# dbRDA values for each environmental variable
reg_scores <- data.frame(scores(Phrag_dbrda,display="bp"),
                       labels=rownames(scores(Phrag_dbrda,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 ()") +
  ylab("RDA 2 ()") +  
  geom_text(data=site_scores, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scores, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scores, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scores, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scores,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scores, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)

# There is a lot of data here - too many species. See the Ordination plot where
# we use the 13 most abundant species 


#### Ordination: Extra Stuff ----
# Adjusted R2 vales for the dbRDA
RsquareAdj(Phrag_dbrda)

# Permutation tests (PERMANOVA)
anova(Phrag_dbrda, permutations = 1000)

anova(Phrag_dbrda, by = "margin",permutations=1000)

 
adonis2(spec~pH+Salinity15cmppt+WaterDepthcm+Biomass+Litter,
        data=envir,distance="bray", by="margin", permutations=1000, 
        na.action = na.fail) # Error here, not sure how to fix.


# Forward selection
Phrag_dbrda <- capscale(spec~pH+Salinity15cmppt+WaterDepthcm+Biomass+Litter, 
                        data=envir,distance="bray",na.action = na.exclude)

stepf <- ordistep(capscale(spec~1,distance="bray",data=envir), 
                  scope=formula(Phrag_dbrda),direction="both", 
                  Pin=.01, Pout=.05, permutations=1000, data=envir) 
            # More errors, back to the drawing board


#### Assessing Abundance for all Species ----
spec <- PhragData_narm[,11:74]  # all of our species
spec_sort <- spec               # create copy of data frame to use in for loop

for(i in 1:ncol(spec)) {
  spec_sort[,i] <- sort(spec[,i], decreasing = T)
}

top_abun <- spec_sort[1,]    # Selects the top scores and names
top_abun1 <- pivot_longer(top_abun, names_to="Species", values_to="Abundance",
             Phragmites.australis:Vigna.luteola)  # pivot for easier sorting
top_abun1 <- arrange(top_abun1, Abundance) # sort by Abundance (ascending)
top_spec <- top_abun1[52:64,]   # select the top ones by row number

# Probably a smoother way to do this but some was also for visualization

#### Ordination Plot of 13 Most Abundant Species ----
envir <- PhragData_narm[,1:10]
spec <- PhragData_narm[,11:74]    # again, take all of the species
top_species_string <- top_spec[[1]]   # here is our top species in a string
spec.top <- select(spec, all_of(top_species_string)) 
                                    # use string to select from all species

#"The issue is that you have some plots with no plants in them" - E. Farrer 
ind<-rowSums(spec.top)>0
spec.top2<-spec.top[ind,]
envir2<-envir[ind,]
#then use spec.top2 and envir2 in the ordination

library(vegan)

Phrag_dbrda.top <- capscale(spec.top2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                            Litter, data=envir2,distance="bray",
                            na.action = na.exclude)

# Summary and Plot
summary(Phrag_dbrda.top)
plot(Phrag_dbrda.top)

library(ggplot2)
library(ggrepel)

site_scores2 <- data.frame(cbind(envir2,scores(Phrag_dbrda.top,scaling = 2)$site,
                                labels=rownames(scores(Phrag_dbrda.top)$site)))
species_scores2 <- data.frame(scores(Phrag_dbrda.top, scaling = 2)$species,
                             labels=rownames(scores(Phrag_dbrda.top)$species))
reg_scores2 <- data.frame(scores(Phrag_dbrda.top,display="bp", scaling = 2),
                         labels=rownames(scores(Phrag_dbrda.top,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (14.4%)") +
  ylab("RDA 2 (1.7%)") +  
  #ylim(c(-2.5,2.5)) +
  #xlim(c(-2.5,6)) +
  geom_text(data=site_scores2, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scores2, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scores2, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scores2, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scores2,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scores2, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)


#### PERMANOVA on Ordination ----
# Can we drop some non-important variables?

anova(Phrag_dbrda.top, by="margin", permutations = 1000)
           # all variables are significant! 

#### Ordination Plot of 12 Most frequently Observed Species ----

spec_f <- spec[colSums(spec>0) > 20] # thanks Emily!
ind<-rowSums(spec_f)>0
spec_f2 <- spec_f[ind,]
envirF<-envir[ind,]

Phrag_dbrda.freq <- capscale(spec_f2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                             Litter, data=envirF,distance="bray",
                             na.action = na.exclude)

# Quick peek and onto big plot
summary(Phrag_dbrda.freq) # CAP1 - 14.9%; CAP2 - 1.6%
plot(Phrag_dbrda.freq)

library(ggplot2)
library(ggrepel)

site_scoresF <- data.frame(cbind(envirF,scores(Phrag_dbrda.freq,scaling = 2)$site,
                                 labels=rownames(scores(Phrag_dbrda.freq)$site)))
species_scoresF <- data.frame(scores(Phrag_dbrda.freq, scaling = 2)$species,
                              labels=rownames(scores(Phrag_dbrda.freq)$species))
reg_scoresF <- data.frame(scores(Phrag_dbrda.freq,display="bp", scaling = 2),
                          labels=rownames(scores(Phrag_dbrda.freq,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (14.9%)") +
  ylab("RDA 2 (1.6%)") +  
  #ylim(c(-2.5,2.5)) +
  #xlim(c(-2.5,6)) +
  geom_text(data=site_scoresF, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresF, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresF, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresF, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresF,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresF, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)


## PERMANOVA check
anova(Phrag_dbrda.freq, by="margin", permutations = 1000) 
# all variables significant


#### Ordination by site across years (2017-2022) ---- 
# We will do most frequently observed species in each site

PhragData_P <- PhragData_narm[grep("Pearl River", PhragData_narm$Site),]
PhragData_BB <- PhragData_narm[grep("Big Branch", PhragData_narm$Site),]
PhragData_Bar <- PhragData_narm[grep("Barataria", PhragData_narm$Site),]
PhragData_BS <- PhragData_narm[grep("Bayou Sauvage", PhragData_narm$Site),]


### Pearl River (P) ----

spec_P <- PhragData_P[,14:77]
env_P <- PhragData_P[,1:12]

# 126 observations in six years - 7 obs = 5.6% presence
spec_P1 <- spec_P[colSums(spec_P>0) > 7]  
ind <- rowSums(spec_P1)>0
spec_P2 <- spec_P1[ind,]
env_P2 <- env_P[ind,]

Phrag_dbrda.P <- capscale(spec_P2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                            Litter, data=env_P2,distance="bray",
                            na.action = na.exclude)
## PERMANOVA check
anova(Phrag_dbrda.P, by="margin", permutations = 1000) 
# Only Salinity and Biomass Are Significant

# Quick peek and onto big plot
summary(Phrag_dbrda.P) # CAP1 - 7.5%; CAP2 - 1.7%
plot(Phrag_dbrda.P)

library(ggplot2)
library(ggrepel)

site_scoresP <- data.frame(cbind(env_P2,scores(Phrag_dbrda.P,scaling = 2)$site,
                                 labels=rownames(scores(Phrag_dbrda.P)$site)))
species_scoresP <- data.frame(scores(Phrag_dbrda.P, scaling = 2)$species,
                              labels=rownames(scores(Phrag_dbrda.P)$species))
reg_scoresP <- data.frame(scores(Phrag_dbrda.P,display="bp", scaling = 2),
                          labels=rownames(scores(Phrag_dbrda.P,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.5%)") +
  ylab("RDA 2 (1.7%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Pearl River") +
  geom_text(data=site_scoresP, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresP, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresP, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresP, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresP,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresP, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)
  


#### Big Branch (BB) ----

spec_BB <- PhragData_BB[,14:77]
env_BB <- PhragData_BB[,1:12]

# 100 observations in five years (no 2019) - 6 obs = 6% presence
spec_BB1 <- spec_BB[colSums(spec_BB>0) > 6]  
ind <- rowSums(spec_BB1)>0
spec_BB2 <- spec_BB1[ind,]
env_BB2 <- env_BB[ind,]

Phrag_dbrda.BB <- capscale(spec_BB2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                           Litter, data=env_BB2,distance="bray",
                           na.action = na.exclude)
## PERMANOVA check
anova(Phrag_dbrda.BB, by="margin", permutations = 1000) 
# pH, Salinity, Litter are significant

# Quick peek and onto big plot
summary(Phrag_dbrda.BB) # CAP1 - 8.3%; CAP2 - 5.8%
plot(Phrag_dbrda.BB)

library(ggplot2)
library(ggrepel)

site_scoresBB <- data.frame(cbind(env_BB2,scores(Phrag_dbrda.BB,scaling = 2)$site,
                                 labels=rownames(scores(Phrag_dbrda.BB)$site)))
species_scoresBB <- data.frame(scores(Phrag_dbrda.BB, scaling = 2)$species,
                              labels=rownames(scores(Phrag_dbrda.BB)$species))
reg_scoresBB <- data.frame(scores(Phrag_dbrda.BB,display="bp", scaling = 2),
                          labels=rownames(scores(Phrag_dbrda.BB,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (8.3%)") +
  ylab("RDA 2 (5.8%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Big Branch") +
  geom_text(data=site_scoresBB, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBB, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBB, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBB, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBB,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBB, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)


#### Barataria (Bar) ----

spec_Bar <- PhragData_Bar[,14:77]
env_Bar <- PhragData_Bar[,1:12]

# 122 observations in six years - 7 obs = 5.7% presence
spec_Bar1 <- spec_Bar[colSums(spec_Bar>0) > 7]  
ind <- rowSums(spec_Bar1)>0
spec_Bar2 <- spec_Bar1[ind,]
env_Bar2 <- env_Bar[ind,]

Phrag_dbrda.Bar <- capscale(spec_Bar2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                             Litter, data=env_Bar2,distance="bray",
                           na.action = na.exclude)
## PERMANOVA check
anova(Phrag_dbrda.Bar, by="margin", permutations = 1000) 
# Salinity and Litter are significant (pH & Water Depth kinda close) 

# Quick peek and onto big plot
summary(Phrag_dbrda.Bar) # CAP1 - 7.3%; CAP2 - 1.9%
plot(Phrag_dbrda.Bar)

library(ggplot2)
library(ggrepel)

site_scoresBar <- data.frame(cbind(env_Bar2,scores(Phrag_dbrda.Bar,scaling = 2)$site,
                                  labels=rownames(scores(Phrag_dbrda.Bar)$site)))
species_scoresBar <- data.frame(scores(Phrag_dbrda.Bar, scaling = 2)$species,
                               labels=rownames(scores(Phrag_dbrda.Bar)$species))
reg_scoresBar <- data.frame(scores(Phrag_dbrda.Bar,display="bp", scaling = 2),
                           labels=rownames(scores(Phrag_dbrda.Bar,display="bp")))


ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.3%)") +
  ylab("RDA 2 (1.9%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Barataria") +
  geom_text(data=site_scoresBar, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBar, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBar, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBar, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBar,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBar, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)


#### Bayou Sauvage (BS) ----

spec_BS <- PhragData_BS[,14:77]
env_BS <- PhragData_BS[,1:12]

# 54 observations in three years (only 17,19,21) - 3 obs = 5.6% presence
spec_BS1 <- spec_BS[colSums(spec_BS>0) > 3]  
ind <- rowSums(spec_BS1)>0
spec_BS2 <- spec_BS1[ind,]
env_BS2 <- env_BS[ind,]

Phrag_dbrda.BS <- capscale(spec_BS2~pH+Salinity15cmppt+WaterDepthcm+Biomass+
                              Litter, data=env_BS2,distance="bray",
                            na.action = na.exclude)
## PERMANOVA check
anova(Phrag_dbrda.BS, by="margin", permutations = 1000) 
# Water Depth and litter is significant 

# Quick peek and onto big plot
summary(Phrag_dbrda.BS) # CAP1 - 7.9%; CAP2 - 5.2%
plot(Phrag_dbrda.BS)

library(ggplot2)
library(ggrepel)

site_scoresBS <- data.frame(cbind(env_BS2,scores(Phrag_dbrda.BS,scaling = 2)$site,
                                   labels=rownames(scores(Phrag_dbrda.BS)$site)))
species_scoresBS <- data.frame(scores(Phrag_dbrda.BS, scaling = 2)$species,
                                labels=rownames(scores(Phrag_dbrda.BS)$species))
reg_scoresBS <- data.frame(scores(Phrag_dbrda.BS,display="bp", scaling = 2),
                            labels=rownames(scores(Phrag_dbrda.BS,display="bp")))

ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.9%)") +
  ylab("RDA 2 (5.2%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Bayou Sauvage") +
  geom_text(data=site_scoresBS, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBS, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBS, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBS, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBS,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBS, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)


#### Most Frequently Observed Species ----
# Visualize most frequently observed Species in the four sites for the dbRDA
spec_BSlong <- c(colnames(spec_BS2), rep(NA, 12 - length(colnames(spec_BS2))))
spec_Barlong <- c(colnames(spec_Bar2), rep(NA, 12 - length(colnames(spec_Bar2))))
spec_BBlong <- c(colnames(spec_BB2), rep(NA, 12 - length(colnames(spec_BB2))))
spec_Plong <- c(colnames(spec_P2), rep(NA, 12 - length(colnames(spec_P2))))

Mostfreq <- data.frame(Bayou_Sauvage5.6=spec_BSlong,Barataria5.7=spec_Barlong,
                   Big_Branch6.0=spec_BBlong,Pearl_River5.6=spec_Plong)

#### ggplot combine Sites (freq, all years) ----
plot1 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.5%)") +
  ylab("RDA 2 (1.7%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Pearl River") +
  geom_text(data=site_scoresP, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresP, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresP, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresP, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresP,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresP, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)
plot2 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (8.3%)") +
  ylab("RDA 2 (5.8%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Big Branch") +
  geom_text(data=site_scoresBB, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBB, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBB, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBB, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBB,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBB, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)
plot3 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.3%)") +
  ylab("RDA 2 (1.9%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Barataria") +
  geom_text(data=site_scoresBar, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBar, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBar, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBar, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBar,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBar, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)
plot4 <- ggplot() + 
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("RDA 1 (7.9%)") +
  ylab("RDA 2 (5.2%)") +  
  #ylim(c(-2,2)) +
  ggtitle("Bayou Sauvage") +
  geom_text(data=site_scoresBS, aes(x=CAP1, y=CAP2, label=labels), size=1) +
  geom_point(data=site_scoresBS, aes(x=CAP1, y=CAP2),alpha=0.7, size=0.5, 
             color="gray50") +
  geom_segment(data=species_scoresBS, 
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="red", size=0.2, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=species_scoresBS, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="red", size=3, max.overlaps = 100) +
  geom_segment(data=reg_scoresBS,
               aes(x=0, y=0, xend=CAP1, yend=CAP2), 
               colour="blue", size=0.7, arrow=arrow(length=unit(.15,"cm"))) +
  geom_text_repel(data=reg_scoresBS, 
                  aes(x=CAP1, y=CAP2, label=labels),
                  colour="black",size=3)+
  coord_fixed(ratio=1)

grid.arrange(plot1,plot2,plot3,plot4, nrow=1)


#### Pearl River Year Analysis ----
PhragData_P17 <- PhragData_P[grep("2017",PhragData_P$Year),]
PhragData_P18 <- PhragData_P[grep("2018",PhragData_P$Year),]
PhragData_P19 <- PhragData_P[grep("2019",PhragData_P$Year),]
PhragData_P20 <- PhragData_P[grep("2020",PhragData_P$Year),]
PhragData_P21 <- PhragData_P[grep("2021",PhragData_P$Year),]
PhragData_P22 <- PhragData_P[grep("2022",PhragData_P$Year),]

#
#### GLM on Shannon Index ----
PhragData_narm <- PhragData %>%
  filter(!is.na(Shannon), Site!="LUMCON 1", Site!="LUMCON 2", 
         Site!="Fontainebleau", Site!="Turtle Cove")

# Use year as a factor (but recall corAR1() requires year as an integer) 
PhragData_narm$Year.f <- as.factor(PhragData_narm$Year)


# Some Code From Thomas on Temporal and Spatial Models ---
#Reg Model
PhragShan1 <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f,
                  random = ~1|Plot, data = PhragData_narm)
summary(PhragShan1)

#temporal model
PhragShan1T <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm)
summary(PhragShan1T)

#spatial model 
PhragShan1S <- gls(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f,
                correlation=corSpher(form = ~ Lat+Long|Year.f),
                data = PhragData_narm)

anova(PhragShan1, PhragShan1T, PhragShan1S)
# Temporal Model is best (PhragShan1T)


# Set contrasts to helmert before doing type 3 ANOVA on best model
options(contrasts=c("contr.helmert","contr.poly"))

anova(PhragShan1T, type = "marginal")

# Test other interactive effects and the three way anova, separate and together
PhragShan1T <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm, method = "ML")

PhragShan2T <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f +
                             Site*Transect,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm, method = "ML")

PhragShan3T <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f +
                     Site*Transect*Year.f,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm, method = "ML")

PhragShan4T <- lme(Shannon ~ Site + Year.f + Transect + Year.f*Transect + Site*Year.f +
                     Site*Transect + Site*Transect*Year.f,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm, method = "ML")

anova(PhragShan1T,PhragShan2T,PhragShan3T,PhragShan4T)
# PhragShan3T and PhragShan4T are identical since three way interaction encompasses
# other two-way interactions. Let's use PhragShan3T since it is smaller.

# Change model to REML (default, needs no specification)
PhragShan3T <- lme(Shannon ~ Site + Year + Transect + Year*Transect + Site*Year +
                     Site*Transect*Year,
                   correlation = corAR1(form =~ Year|Plot),
                   random = ~1|Plot, data = PhragData_narm)

anova(PhragShan3T, type = "marginal")
# Year, transect, Site:Year, Site:Transect, Site:Year:Transect (not site...5/4/2023)
# See Catherine's figure (Fig. 3)

#### GLM Model Validation -
plot(fitted(PhragShan3T),resid(PhragShan3T,type="normalized"))
plot(PhragData_narm$Year.f,resid(PhragShan3T,type="normalized"))
plot(as.factor(PhragData_narm$Site),resid(PhragShan3T,type="normalized"))
plot(as.factor(PhragData_narm$Transect),resid(PhragShan3T,type="normalized"))
hist(resid(PhragShan3T,type="normalized"))



# Plot by site and year ---
PhragData_Shan <- PhragData_narm %>%
  group_by(Site, Year) %>%
  summarise(mean = mean(Shannon), se = std.error(Shannon))

# First we will plot. But later, needs work

ggplot(data = PhragData_Shan, aes(x=Year,y=mean, fill=Site)) +
  geom_line() +
  geom_point(stat = "identity", position = "dodge") +
  ylim(0.1,1.2) +
  labs(x = "Year",y="Shannon Diversity Index") +
  geom_errorbar(aes(ymax=mean+se,ymin=mean-se),width=.25) +
  facet_wrap(~Site,scales = "free") +
  theme(legend.position = "none")


#### Nugget Effect ----
# spherical or exponential models