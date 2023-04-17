# Jacob Dixon's code

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
         !is.na(Site), !is.na(Transect),
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

plot2 <- ggplot(data = PhragData_meansWatDep, aes(x=Transect,y=mean, fill=Site)) +
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

# First, well need to pivot our plant species into one species column
# Columns 11-74 are species (some unknown)

PhragData_ord <- PhragData_narm %>% 
  pivot_longer(cols = Phragmites.australis:Vigna.luteola, 
               names_to = "Species", 
               values_to = "Abundance")

install.packages("vegan")
library(vegan)

Phrag_dbrda <- dbrda(PhragData_ord$Abundance~pH+Salinity15cmppt+WaterDepthcm+
                     Biomass+Litter,
                     distance="bray",na.action = na.exclude,data=PhragData_ord)


# Wot u plot, mate
library(ggplot2)
library(ggrepel)

