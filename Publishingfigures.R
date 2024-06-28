###Final Figure Formatting
##5/21/24
#Coleman Benedict

library(tidyverse)
library(plotrix)
library(ggthemes)

options(contrasts=c("contr.helmert","contr.poly"))

dat23<-read.csv("/Users/coleman/Documents/R/LAMARSH/Data/PhragSurvey2017to2023.csv",stringsAsFactors = T,row.names=1)
View(dat23)

dat_23<-dat23%>%
  filter(Site%in%c("Barataria","Bayou Sauvage","Big Branch","Pearl River"))%>%
  mutate(Yearfac=as.factor(Year))
dat_23$Transect<-factor(dat_23$Transect,levels=c("Native","Transition","Phragmites"))
dat_23$Plot<-factor(dat_23$Plot)
dat_23$Site<-factor(dat_23$Site,levels=c("Barataria", "Pearl River", "Bayou Sauvage", "Big Branch"))
dat_23$Sitelab<-case_match(dat_23$Site,"Barataria"~"a) Barataria","Pearl River"~"b) Pearl River","Bayou Sauvage"~"c) Bayou Sauvage","Big Branch"~"d) Big Branch",.ptype = factor(levels = c("a) Barataria","b) Pearl River","c) Bayou Sauvage","d) Big Branch")))
dat_23$Transectnew<-case_match(dat_23$Transect,"Native"~"Native","Transition"~"Codominant","Phragmites"~"Phragmites",.ptype = factor(levels = c("Native","Codominant","Phragmites")))


##PHRAG ABUNDANCE (stem counts)
#Summary statistics
dat23p <- dat_23 %>%
  group_by(Year, Transectnew, Sitelab) %>%
  summarise(
    mean = mean(Phragmites.australis, na.rm = TRUE),
    se = std.error(Phragmites.australis))

dat23p <- data.frame(dat23p)

#Plot
#note emily modified this and it still needs a little work
pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/phragdensity.pdf",width=4.8,height=4)
ggplot(dat23p, aes(x = Year, y = mean, color = Transectnew)) +
  labs(
    y = expression(italic("Phragmites")~density~(stems/m^{2})),
    x = "Year") +
  geom_point(size = 2.5) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.2, size = 0.8) +
  facet_wrap(~ Sitelab, ncol = 2) +  # Arrange facets in a 2x2 grid
  theme_classic(base_size = 14) +
  theme(
    #text = element_text(family = "serif"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),  # Increase spacing between panels
    axis.line = element_line(color = "gray30", size = .5),
    strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0))
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c("Native" = "#56ae6c", "Codominant" = "#6881d8", "Phragmites" = "#ba543d")) +
  scale_x_continuous(breaks = c(2017,2019,2021,2023))
dev.off()

##Native abundance (stem counts)
#Summary statistics
dat23N <- dat_23 %>%
  group_by(Year, Transect, Site) %>%
  summarise(
    mean = mean(NatAbun, na.rm = TRUE),
    se = std.error(NatAbun))

dat23N <- data.frame(dat23N)

#Plot
ggplot(dat23N, aes(x = Year, y = mean, color = Transect)) +
  labs(
    y = "Native Abundance (stem counts per square meter)",
    x = "Year") +
  geom_point(size = 2.5) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.2, size = 0.8) +
  facet_wrap(~ Site, ncol = 2) + 
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"), 
    axis.line = element_line(color = "gray30", size = .5)
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c("Native" = "#56ae6c", "Transition" = "#6881d8", "Phragmites" = "#ba543d"))



##Native Richness
#Summary statistics
dat23R <- dat_23 %>%
  group_by(Year, Transect, Site) %>%
  summarise(
    mean = mean(NatRichness, na.rm = TRUE),
    se = std.error(NatRichness))

dat23R <- data.frame(dat23R)

#Plot
ggplot(dat23R, aes(x = Year, y = mean, color = Transect)) +
  labs(
    y = "Native Richenss",
    x = "Year") +
  geom_point(size = 2.5) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.2, size = 0.8) +
  facet_wrap(~ Site, ncol = 2) + 
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 12),
    plot.subtitle = element_text(size = 12, face = "italic"),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"), 
    axis.line = element_line(color = "gray30", size = .5)
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c("Native" = "#56ae6c", "Transition" = "#6881d8", "Phragmites" = "#ba543d"))






