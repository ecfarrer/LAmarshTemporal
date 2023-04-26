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
  labs(x = "Transect",y="Phrag Abundance")+
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


#### Ordination Plot ----
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


#!# Under Construction #!#

### Organize by site 

### Picking by frequency vs abundance 

#### PERMANOVA on Ordination ----
# Can we drop some non-important variables?