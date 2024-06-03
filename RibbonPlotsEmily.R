#Ribbon plots

#based on: https://search.r-project.org/CRAN/refmans/riverplot/html/riverplot-package.html

#install.packages("riverplot")
library(riverplot)
library(tidyverse)
library(cowplot)
library(gridGraphics)

#Checking biomass per stem data

biomassperstem<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/LAmarsh_Survey_2020_BiomassperStem.csv") #
bpsmean<-biomassperstem%>%
  filter(Species2!="Unknown grass #2",Species2!="Unknown plant 1",Species2!="Unknown plant 2",Species2!="Triangle stem unknown biomass")%>%
  group_by(Species2)%>%
  tally()
as.data.frame(bpsmean)


#River plot data

#rivdat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Phragbiomass2023.csv") #old with only bins 1-4
#rivdat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Phragnative1.csv") #old, no 2023

rivdat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Corrected_Biomass2023.csv") #

head(rivdat)
View(rivdat)

#Bin1: Value-based intervals with the cutoffs being 0 to 0.33, 0.33 to 0.66, and 0.66 to 1
#Bin2: Value-based intervals with the cutoffs being 0 to 0.25, 0.25 to 0.5, and 0.5 to 1
#Bin3: Percentile-based intervals based on the total dataset's 33rd and 66th percentile values acting as cutoffs
#Bin4: Percentile-based intervals sub-setted by Site using 33rd and 66th percentile values acting as cutoffs for site-specific groupings
#Bin5: Percentile-based intervals based on the total dataset's 33rd and 66th percentile values acting as cutoffs from the base year 2017
#Bin6: Percentile-based intervals sub-setted by Site using 33rd and 66th percentile values acting as cutoffs for site-specific groupings from the base year 2017. note some of these were "wrong" when I tried calculating them in R. they were probably just humna error in excel, so below I recalculate bin6 in R. I did not check the other bins for accuracy

#Bayou Sauvage weirdness
#Plot 112 and 113 are missing in year2 (2018) (not found so data not taken)
#in 2023 plot 109 had nothing in it (it was surveyed and had nothing) and 111 had only 2 stems of baccharis, no phrag. baccharis was not measured for biomass, but I can still call this plot a "low" phrag b/c it was 100% native

#Finding percentiles
#Coleman did it in terms of relative biomass not just Phrag biomass
quantile(rivdat$Phragmites.australis.Biomass.g., c(.33, .66)) 
#odd the 33rd percentile is 0, how did that go down? i think this is what happened and it is awkward
cbind(rivdat$Phragmites.australis.Biomass.g.,rivdat$Bin3)
length(which(rivdat$Bin3=="Low"))
length(which(rivdat$Bin3=="Mid"))
length(which(rivdat$Bin3=="High"))

##### Calculating bin 6 for each site for all years #####
BPbin6<-rivdat%>%
  filter(Site=="Barataria",Year=="2017")
#quantile(BPbin6$Phragmites.australis.Biomass.g., c(.33, .66)) 
quantile(BPbin6$Percentage.Phrag, c(.33, .66)) 
BPbin6b<-rivdat%>%
  filter(Site=="Barataria")%>%
  mutate(bin6b = cut(Percentage.Phrag, c(-1, 0.1259551, 0.5289319,1), labels=c("Low","Mid","High") ,right = T))
which(BPbin6b$Bin6!=BPbin6b$bin6b)

#write.csv(BPbin6b,"BPbin6b.csv")

BBbin6<-rivdat%>%
  filter(Site=="Big Branch",Year=="2017")
quantile(BBbin6$Percentage.Phrag, c(.33, .66)) 
BBbin6b<-rivdat%>%
  filter(Site=="Big Branch")%>%
  mutate(bin6b = cut(Percentage.Phrag, c(-1, 0.2457260, 0.9054701,1), labels=c("Low","Mid","High") ,right = T))
which(BBbin6b$Bin6!=BBbin6b$bin6b)

BSbin6<-rivdat%>%
  filter(Site=="Bayou Sauvage",Year=="2017")
quantile(BSbin6$Percentage.Phrag, c(.33, .66)) 
BSbin6b<-rivdat%>%
  filter(Site=="Bayou Sauvage")%>%
  mutate(bin6b = cut(Percentage.Phrag, c(-1, 0.4081905, 0.9775267,1), labels=c("Low","Mid","High") ,right = T))
#plot 122 is low for me, mid for Coleman
which(BSbin6b$Bin6!=BSbin6b$bin6b)
BSbin6b%>%
  filter(Plot%in%c(109,111))
BSbin6b%>%
  filter(Plot%in%c(113))
BSbin6b%>%
  filter(Plot%in%c(122))

PRbin6<-rivdat%>%
  filter(Site=="Pearl River",Year=="2017")
quantile(PRbin6$Percentage.Phrag, c(.33, .66)) 
PRbin6b<-rivdat%>%
  filter(Site=="Pearl River")%>%
  mutate(bin6b = cut(Percentage.Phrag, c(-1, 0.0550037, 0.4517804,1), labels=c("Low","Mid","High") ,right = T))
which(PRbin6b$Bin6!=PRbin6b$bin6b)
PRbin6b[c(14,89),]






##### Separating by site - Barataria 2023 #####

# Using Bin6
#rivdatBP<-rivdat%>%
#  filter(Site=="Barataria")%>%
BPbin6c<-BPbin6b%>%
  select(-Bin6)%>%
  dplyr::rename(Bin6=bin6b)
rivdatBP<-BPbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  mutate(y7=recode_factor(y7,"N"="N7","T"="T7","P"="P7"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6",
                                       "N6_N7", "N6_T7", "N6_P7", "T6_N7", "T6_T7", "T6_P7", "P6_N7", "P6_T7", "P6_P7")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBP)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L,19L,20L,21L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L, 7L, 7L, 7L),
    col = c("#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBP$N1,
    N2 = rivdatBP$N2,
    Value = rivdatBP$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -42L), class = "data.frame")#

RPBP <- makeRiver(nodesp, edgesp,node_labels = c("Native","Trans","Phrag",rep("",18)))

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/riverplotBP.pdf",width=4.8,height=3.2)
plot(RPBP, plot_area=0.9,srt=0) #srt=0 makes labels horizontal
text(x=seq(.08,1,by=.14),c(0,0),labels=c(2017:2023),cex=.8)
dev.off()

#plot(RPBP, plot_area=0.9)

#nice green color #729b57



##### Separating by site - Big Branch 2023 #####

#Using Bin6
#rivdatBB<-rivdat%>%
#  filter(Site=="Big Branch")%>%
BBbin6c<-BBbin6b%>%
  select(-Bin6)%>%
  dplyr::rename(Bin6=bin6b)
rivdatBB<-BBbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  mutate(y7=recode_factor(y7,"N"="N7","T"="T7","P"="P7"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6",
                                       "N6_N7", "N6_T7", "N6_P7", "T6_N7", "T6_T7", "T6_P7", "P6_N7", "P6_T7", "P6_P7")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBB)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L,19L,20L,21L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L,7L,7L,7L),
    col = c("#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d")),
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBB$N1,
    N2 = rivdatBB$N2,
    Value = rivdatBB$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -33L), class = "data.frame")#

RPBB <- makeRiver(nodesp, edgesp,node_labels = c("Native","Trans","Phrag",rep("",18)))
plot(RPBB, plot_area=0.9,srt=0)

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/riverplotBB.pdf",width=4.8,height=3.2)
plot(RPBB, plot_area=0.9,srt=0) #srt=0 makes labels horizontal
text(x=seq(.08,1,by=.14),c(0,0),labels=c(2017:2023),cex=.8)
dev.off()



##### Separating by site - Bayou Sauvage 2023 #####
# Using Bin6
#rivdatBS<-rivdat%>%
#  filter(Site=="Bayou Sauvage")%>%
BSbin6c<-BSbin6b%>%
  select(-Bin6)%>%
  dplyr::rename(Bin6=bin6b)
#add a row for 111 being bin6 Low
dim(BSbin6c)# 143 x 66
BSbin6d<-rbind(BSbin6c,rep(NA,66))
BSbin6d$Plot[144]<-111
BSbin6d$Bin6[144]<-"Low"
BSbin6d$Year[144]<-2023
BSbin6e<-BSbin6d%>%
  arrange(Year,Plot)%>%
  filter(Plot!=109,Plot!=112,Plot!=113)
BSbin6f<-BSbin6d%>% #This is for use below in the transitions dataset part
  arrange(Year,Plot)

rivdatBS<-BSbin6e%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  mutate(y7=recode_factor(y7,"N"="N7","T"="T7","P"="P7"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6",
                                       "N6_N7", "N6_T7", "N6_P7", "T6_N7", "T6_T7", "T6_P7", "P6_N7", "P6_T7", "P6_P7")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBS)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L,19L,20L,21L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L,7L,7L,7L),
    col = c("#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d")),
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBS$N1,
    N2 = rivdatBS$N2,
    Value = rivdatBS$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -35L), class = "data.frame")#

RPBS <- makeRiver(nodesp, edgesp, node_labels = c("Native","Trans","Phrag",rep("",18)))
plot(RPBS, plot_area=0.9)

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/riverplotBS.pdf",width=4.8,height=3.2)
plot(RPBS, plot_area=0.9,srt=0) #srt=0 makes labels horizontal
text(x=seq(.08,1,by=.14),c(0,0),labels=c(2017:2023),cex=.8)
dev.off()






##### Separating by site - Pearl River 2023 #####

#Using Bin6
#rivdatPR<-rivdat%>%
#  filter(Site=="Pearl River")%>%
PRbin6c<-PRbin6b%>%
  select(-Bin6)%>%
  dplyr::rename(Bin6=bin6b)
rivdatPR<-PRbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  mutate(y7=recode_factor(y7,"N"="N7","T"="T7","P"="P7"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6",
                                       "N6_N7", "N6_T7", "N6_P7", "T6_N7", "T6_T7", "T6_P7", "P6_N7", "P6_T7", "P6_P7")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatPR)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L,19L,20L,21L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L,7L,7L,7L),
    col = c("#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d","#56ae6c","#6881d8","#ba543d")),
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6","N7","T7","P7" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatPR$N1,
    N2 = rivdatPR$N2,
    Value = rivdatPR$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -30L), class = "data.frame")#

RPPR <- makeRiver(nodesp, edgesp,node_labels = c("Native","Trans","Phrag",rep("",18)))
plot(RPPR, plot_area=0.9)

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/riverplotPR.pdf",width=4.8,height=3.2)
plot(RPPR, plot_area=0.9,srt=0) #srt=0 makes labels horizontal
text(x=seq(.08,1,by=.14),c(0,0),labels=c(2017:2023),cex=.8)
dev.off()








##### Making a dataset of transitions from the riverplots #####
trBP<-BPbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  separate(change, c("State1","State2"), remove=F)%>%
  separate(change, c("State1","State2"), remove=F)%>%
  mutate(NN=ifelse(State1=="N"&State2=="N",1,0))%>%
  mutate(NT=ifelse(State1=="N"&State2=="T",1,0))%>%
  mutate(NP=ifelse(State1=="N"&State2=="P",1,0))%>%
  mutate(TT=ifelse(State1=="T"&State2=="T",1,0))%>%
  mutate(TN=ifelse(State1=="T"&State2=="N",1,0))%>%
  mutate(TP=ifelse(State1=="T"&State2=="P",1,0))%>%
  mutate(PP=ifelse(State1=="P"&State2=="P",1,0))%>%
  mutate(PN=ifelse(State1=="P"&State2=="N",1,0))%>%
  mutate(PT=ifelse(State1=="P"&State2=="T",1,0))
  
data.frame(trBP)
trBP$Site<-"Barataria"

trBB<-BBbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  separate(change, c("State1","State2"), remove=F)%>%
  separate(change, c("State1","State2"), remove=F)%>%
  mutate(NN=ifelse(State1=="N"&State2=="N",1,0))%>%
  mutate(NT=ifelse(State1=="N"&State2=="T",1,0))%>%
  mutate(NP=ifelse(State1=="N"&State2=="P",1,0))%>%
  mutate(TT=ifelse(State1=="T"&State2=="T",1,0))%>%
  mutate(TN=ifelse(State1=="T"&State2=="N",1,0))%>%
  mutate(TP=ifelse(State1=="T"&State2=="P",1,0))%>%
  mutate(PP=ifelse(State1=="P"&State2=="P",1,0))%>%
  mutate(PN=ifelse(State1=="P"&State2=="N",1,0))%>%
  mutate(PT=ifelse(State1=="P"&State2=="T",1,0))

data.frame(trBB)
trBB$Site<-"Big Branch"

#use BSbin6f if you want to include plots 109,112,113, use BSbin6e if you want all the years from those plots not included
trBS<-BSbin6f%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  #filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  separate(change, c("State1","State2"), remove=F)%>%
  separate(change, c("State1","State2"), remove=F)%>%
  filter(State1!="NA")%>%
  filter(State2!="NA")%>%
  mutate(NN=ifelse(State1=="N"&State2=="N",1,0))%>%
  mutate(NT=ifelse(State1=="N"&State2=="T",1,0))%>%
  mutate(NP=ifelse(State1=="N"&State2=="P",1,0))%>%
  mutate(TT=ifelse(State1=="T"&State2=="T",1,0))%>%
  mutate(TN=ifelse(State1=="T"&State2=="N",1,0))%>%
  mutate(TP=ifelse(State1=="T"&State2=="P",1,0))%>%
  mutate(PP=ifelse(State1=="P"&State2=="P",1,0))%>%
  mutate(PN=ifelse(State1=="P"&State2=="N",1,0))%>%
  mutate(PT=ifelse(State1=="P"&State2=="T",1,0))

data.frame(trBS)
trBS$Site<-"Bayou Sauvage"

trPR<-PRbin6c%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6","2023"="y7"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  unite("y6_y7",y6,y7,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6,y6_y7)%>%
  pivot_longer(y1_y2:y6_y7,names_to="years",values_to = "change")%>%
  separate(change, c("State1","State2"), remove=F)%>%
  separate(change, c("State1","State2"), remove=F)%>%
  mutate(NN=ifelse(State1=="N"&State2=="N",1,0))%>%
  mutate(NT=ifelse(State1=="N"&State2=="T",1,0))%>%
  mutate(NP=ifelse(State1=="N"&State2=="P",1,0))%>%
  mutate(TT=ifelse(State1=="T"&State2=="T",1,0))%>%
  mutate(TN=ifelse(State1=="T"&State2=="N",1,0))%>%
  mutate(TP=ifelse(State1=="T"&State2=="P",1,0))%>%
  mutate(PP=ifelse(State1=="P"&State2=="P",1,0))%>%
  mutate(PN=ifelse(State1=="P"&State2=="N",1,0))%>%
  mutate(PT=ifelse(State1=="P"&State2=="T",1,0))

data.frame(trPR)
trPR$Site<-"Pearl River"

#when i first did this, i forgot that i deleted all data from plots 109, 112, and 113 b/c I was missing one year in each of those plots and it was necessary for the ribbon plot. [plot 112 and 113 missing in 2018, plot 109 had nothing in it in 2023, so im missing 5 transitions total] but I can put it back for these analyses. I put them back in but then realized I do still need to delete the ones with NAs 2018-2019,2023, so I filtered on that below
tr<-rbind(trBP,trBS,trBB,trPR)
tr2<-tr%>%
  mutate(Year=as.numeric(recode(years,"y1_y2"="2018","y2_y3"="2019","y3_y4"="2020","y4_y5"="2021","y5_y6"="2022","y6_y7"="2023")))%>%
  full_join(biomass4)%>%
  filter(is.na(change)==F)%>%
  mutate(NtoTP=ifelse(State1=="N"&State2%in%c("T","P"),1,0))%>%
  mutate(NtoTPandTtoP=ifelse(NtoTP==1|TP==1,1,0))%>%
  mutate(PtoNT=ifelse(State1=="P"&State2%in%c("N","T"),1,0))%>%
  mutate(PtoNTandTtoN=ifelse(PtoNT==1|TN==1,1,0))%>%
  mutate(stasis=case_match(change,"N_N"~1,"T_T"~1,"P_P"~1,"N_T"~0,"N_P"~0,"T_N"~0,"T_P"~0,"P_N"~0,"P_T"~0))%>%
  mutate(Sitelab=case_match(Site,"Barataria"~"a) Barataria","Pearl River"~"b) Pearl River","Bayou Sauvage"~"c) Bayou Sauvage","Big Branch"~"d) Big Branch",.ptype = factor(levels = c("a) Barataria","b) Pearl River","c) Bayou Sauvage","d) Big Branch"))))

tr2$Site<-factor(tr2$Site,levels=c("Barataria","Pearl River","Bayou Sauvage","Big Branch"))

#ind<-which(is.na(tr2$NN)==T)
#View(tr2[ind,])

data.frame(tr2) #504 x 42, now 499 x 42
dim(tr2)
tail(data.frame(tr2))


##### Figures of transitions vs plot level salinity and water depth #####

#possible transitions to test. Like Stein et al 2016, we will subset data for plots that did not transition and plots that transitioned into one of the other categories:
#NN-NT
#NN-NP
#TT-TP
#TT-TN
#PP-PN
#PP-PT

#Making some quick datasets and plots of single transitions
#N going to T
NN_NT<-tr2%>%
  filter(change%in%c("N_N","N_T"))
data.frame(NN_NT)
ggplot(data = NN_NT, aes(x = Salinity15cmppt, y = NT)) +
  geom_point()+
  geom_smooth(method='glm',method.args=list(family='binomial'))+
  facet_wrap(~Site)

#N going to P
NN_NP<-tr2%>%
  filter(change%in%c("N_N","N_P"))
data.frame(NN_NP)
ggplot(data = NN_NP, aes(x = Salinity15cmppt, y = NP)) +
  geom_point()+
  geom_smooth(method='glm',method.args=list(family='binomial'))+
  facet_wrap(~Site)

#N going to either T or P
NN_NT_NP<-tr2%>%
  filter(change%in%c("N_N","N_T","N_P"))
data.frame(NN_NT_NP)
ggplot(data = NN_NT_NP, aes(x = Salinity15cmppt, y = NtoTP)) +
  geom_point()+
  geom_smooth(method='glm',method.args=list(family='binomial'))+
  facet_wrap(~Site)


#Making combined dataset of N going to either T or P and T going to P
NN_NT_NP_TT_TP<-tr2%>%
  filter(change%in%c("N_N","N_T","N_P","T_T","T_P"))
data.frame(NN_NT_NP_TT_TP)


#Making combined dataset of P going to N or T and T going to N
PP_PN_PT_TT_TN<-tr2%>%
  filter(change%in%c("P_P","P_N","P_T","T_T","T_N"))
data.frame(PP_PN_PT_TT_TN)



##### Salinity and Probability of going to more phrag #####

#pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/test.pdf",width=2.4,height=2)
salphrag<-ggplot(data = NN_NT_NP_TT_TP, aes(x = Salinity15cmppt, y = NtoTPandTtoP,color=Site,linetype=Site)) +
  theme_classic()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line(),legend.position="none")+
  theme(plot.margin = unit(c(17,5,5,5), "pt"))+#
  ylab("Probability of transition") +# toward more Phragmites
  xlab("Salinity (ppt)")+
  geom_point(size=.8)+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F,size=.5)+
  scale_linetype_manual(values = c("dashed", "solid", "solid", "dashed"))
#dev.off()

#Year is linear, years is a factor. For Bayou Sauvage we only have data from some plots in 2019 and from all plots in 2021 and 2023 (I think we have it for 2017 too but that is not included in the transition dataset)
#m1<-lme(NtoTPandTtoP~Site*Salinity15cmppt,random=~1|years,data = NN_NT_NP_TT_TP,na.action = na.omit,control =list(msMaxIter = 1000, msMaxEval = 1000)) #but this is normal
#m1<-gls(NtoTPandTtoP~Site*Salinity15cmppt+years*Site,data = NN_NT_NP_TT_TP,na.action = na.omit) #but this is normal, this doesn't converge b/c don't have salinity from some years from some sites (Bayou Sauvage)
#anova(m1,type="marginal")

#Full model with all sites
b1 <- glm(NtoTPandTtoP ~ Site*Salinity15cmppt, family = binomial(link="logit"), data = NN_NT_NP_TT_TP, na.action=na.exclude)
#summary(b1)
drop1(b1,test="Chisq",.~.)

#I could do this in glmer if I wanted to include a random effect of year, but I don't think I want to because the variation in salinity is over the years so we don't want year per se in the model
# library(lme4)
# b1<-glmer(NtoTPandTtoP ~ Site*Salinity15cmppt+(1|years),family=binomial(link="cloglog"),data = NN_NT_NP_TT_TP,na.action=na.exclude)#convergence issues, singular, not sure why
# drop1(b1,test="Chisq",.~.)

#Models by site
NN_NT_NP_TT_TP_BP<-NN_NT_NP_TT_TP%>%filter(Site=="Barataria")
NN_NT_NP_TT_TP_BS<-NN_NT_NP_TT_TP%>%filter(Site=="Bayou Sauvage")
NN_NT_NP_TT_TP_BB<-NN_NT_NP_TT_TP%>%filter(Site=="Big Branch")
NN_NT_NP_TT_TP_PR<-NN_NT_NP_TT_TP%>%filter(Site=="Pearl River")

#note when using cloglog (which you are supposed to do when the change of an event is "extremely" low or high) yields pretty much the same p value as the logit. my most extreme case is 7 out of 98, which maybe is not even really that extreme. so i am going to stick with logit
b1 <- glm(NtoTPandTtoP ~ Salinity15cmppt, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BP, na.action=na.exclude)#18 1's out of 82
#summary(b1)
drop1(b1,test="Chisq",.~.)
b1 <- glm(NtoTPandTtoP ~ Salinity15cmppt, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BS, na.action=na.exclude)#27 1's out of 81
#summary(b1)
drop1(b1,test="Chisq",.~.)
b1 <- glm(NtoTPandTtoP ~ Salinity15cmppt, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BB, na.action=na.exclude)#11 1's out of 98
#summary(b1)
drop1(b1,test="Chisq",.~.)
b1 <- glm(NtoTPandTtoP ~ Salinity15cmppt, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_PR, na.action=na.exclude)#7 1's out of 98
#summary(b1)
drop1(b1,test="Chisq",.~.)



##### Salinity and probability of going to more native #####

salnat<-ggplot(data = PP_PN_PT_TT_TN, aes(x = Salinity15cmppt, y = PtoNTandTtoN,color=Site,linetype=Site)) +
  theme_classic()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line(),legend.position="none")+
  theme(plot.margin = unit(c(17,5,5,5), "pt"))+#
  ylab("") +# toward more native
  #ylab("Probability of transition") +# toward more native
  xlab("Salinity (ppt)")+
  geom_point(size=.8)+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F,size=.5)+
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "solid"))

b1 <- glm(PtoNTandTtoN ~ Site*Salinity15cmppt, family = binomial(link="logit"), data = PP_PN_PT_TT_TN, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)

PP_PN_PT_TT_TN_BP<-PP_PN_PT_TT_TN%>%filter(Site=="Barataria")
PP_PN_PT_TT_TN_BS<-PP_PN_PT_TT_TN%>%filter(Site=="Bayou Sauvage")
PP_PN_PT_TT_TN_BB<-PP_PN_PT_TT_TN%>%filter(Site=="Big Branch")
PP_PN_PT_TT_TN_PR<-PP_PN_PT_TT_TN%>%filter(Site=="Pearl River")

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BP, na.action=na.exclude)#23 1s out of 62
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BP$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BP$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BS, na.action=na.exclude)#16 1s out of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BS$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BS$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BB, na.action=na.exclude)#18 1s out of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BB$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BB$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_PR, na.action=na.exclude)#22 1s out of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_PR$PtoNTandTtoN);length(PP_PN_PT_TT_TN_PR$PtoNTandTtoN)



#Water depth

#Looking at variation in water depth by years to see if I should delete years 2017-2019. Overall, there is about a 10-13 cm range in plots at a site in any given year. so it is a little sketchy to attribute a site mean (of say 10) to all the plots, when in reality it would range from 4-16cm. But at some sites like big branch there were the water depths in 2017-19 way higher than the ranges in other years (in which case it would be fine to desingate all plots as 50 when the other years range under 25). Actually because most of the water depth variation is between years rather than within years, it migth be important to ues all the data we can. The full models are more significant with using all data, but the site level regressions are basically the same using 2017-19 vs not. It is also good to use all data b/c otherwise some of the site level regressions only have like 1 transition
temp<-tr2%>%
  filter(Site=="Big Branch")
ggplot(temp,aes(x=WaterDepthcm)) +
  geom_histogram() +
  facet_wrap(~Year)


##### Water depth - Using all years, going more phrag #####

#Figs/Stats for all years
watphrag<-ggplot(data = NN_NT_NP_TT_TP, aes(x = WaterDepthcm, y = NtoTPandTtoP,color=Site,linetype=Site)) +
  theme_classic()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line(),legend.position="none")+
  theme(plot.margin = unit(c(17,5,5,5), "pt"))+#
  ylab("Probability of transition") +# toward more Phragmites
  xlab("Water depth (cm)")+
  geom_point(size=0.8)+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F, size=.5)+
  scale_linetype_manual(values = c("dashed", "dashed", "solid", "dashed"))

b1 <- glm(NtoTPandTtoP ~ Site*WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)

#I don't trust the BS data b/c only 4 of 81 plots had waterdepth>0
data.frame(NN_NT_NP_TT_TP_BS%>%filter(WaterDepthcm>0))
length(NN_NT_NP_TT_TP_BS$WaterDepthcm)

#Stats for all years
b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BP, na.action=na.exclude)#14 1s out of 82
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP_BP$NtoTPandTtoP);length(NN_NT_NP_TT_TP_BP$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BS, na.action=na.exclude)#27 1s to 81
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP_BS$NtoTPandTtoP);length(NN_NT_NP_TT_TP_BS$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_BB, na.action=na.exclude)#11 1s out of 98
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP_BB$NtoTPandTtoP);length(NN_NT_NP_TT_TP_BB$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP_PR, na.action=na.exclude)#7 1s out of 98
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP_PR$NtoTPandTtoP);length(NN_NT_NP_TT_TP_PR$NtoTPandTtoP)


##### Water depth - Using all years, going more native #####

watnat<-ggplot(data = PP_PN_PT_TT_TN, aes(x = WaterDepthcm, y = PtoNTandTtoN,color=Site,linetype=Site)) +
  theme_classic()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line(),legend.position="none")+
  theme(plot.margin = unit(c(17,5,5,5), "pt"))+#
  ylab("") +# toward more native
  #ylab("Probability of a transition") +# toward more native
  xlab("Water depth (cm)")+
  geom_point(size=0.8)+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F,size=0.5)+
  scale_linetype_manual(values = c("solid", "dashed", "blank", "dashed"))

b1 <- glm(PtoNTandTtoN ~ Site*WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)

#without BS
PP_PN_PT_TT_TNnoBS<-PP_PN_PT_TT_TN%>%
  filter(Site!="Bayou Sauvage")

b1 <- glm(PtoNTandTtoN ~ Site*WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TNnoBS, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)

#I don't trust the BS data b/c only 1 of 50 plots had waterdepth>0
data.frame(PP_PN_PT_TT_TN_BS%>%filter(WaterDepthcm>0))
length(PP_PN_PT_TT_TN_BS$WaterDepthcm)

#Stats on all years
b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BP, na.action=na.exclude)#23 1s of 62
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BP$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BP$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BS, na.action=na.exclude)# 16 1s of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BS$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BS$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BB, na.action=na.exclude)#18 1s of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_BB$PtoNTandTtoN);length(PP_PN_PT_TT_TN_BB$PtoNTandTtoN)

b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_PR, na.action=na.exclude)#22 of 50
drop1(b1,test="Chisq",.~.)
sum(PP_PN_PT_TT_TN_PR$PtoNTandTtoN);length(PP_PN_PT_TT_TN_PR$PtoNTandTtoN)


##### Four panel transition plot #####

salwatlegend<-
  ggplot(data = PP_PN_PT_TT_TN, aes(x = WaterDepthcm, y = PtoNTandTtoN,fill=Site,color=Site)) +#color=Site
  theme_classic()+
  theme(line=element_line(linewidth =.3),axis.text=element_text(size=9),axis.title = element_text(size = 10),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(size=10,hjust=0,vjust = 1, margin=margin(l=0)),axis.line=element_line())+
  theme(legend.text=element_text(size=8))+
  theme(legend.spacing.x = unit(.30, 'cm'))+
  #guides(fill = guide_legend(byrow = TRUE)) +
  #theme(legend.key.spacing.y=unit(.1,'cm'))+
  geom_point(size=0.8)+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F,size=0.5)+
  theme(legend.spacing.y = unit(-1.5, 'mm')) + 
  guides(fill = guide_legend(byrow = TRUE))
#to use the legend.spacing.y you need to you use "fill=Site" not color=Site in the iniitial aes, i have no idea why this matters


#need to mess with plot margins to get this nicer
pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Manuscripts/Temporal/Figs/FourPanelTransitionPlot.pdf",width=6,height=4)#width=12.4,height=3
legend <- cowplot::get_legend(salwatlegend)
plot_grid(salphrag,salnat,get_legend(salphrag),watphrag,watnat,legend,nrow = 2,ncol=3,labels=c("a) Toward Phragmites dominance","b) Toward native dominance","","c) Toward Phragmites dominance","d) Toward native dominance"),label_size=10,hjust=-.17,vjust=1.2,scale=1,label_fontface = "plain",rel_widths=c(1,1,.5))
dev.off()




#Trying a few sites (not bayou sauvage) with both water depth and salinity, things are similar except not sig at bayou sauvage
b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt + WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BP, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt + WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BS, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt + WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_PR, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)

b1 <- glm(PtoNTandTtoN ~ Salinity15cmppt + WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN_BB, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)






##### Using 2020-2023 only, since we don't have good plot level water depth 2017-2019 #####
NN_NT_NP_TT_TP2<-NN_NT_NP_TT_TP%>%
  filter(Year>2019)

#Toward more phrag

ggplot(data = NN_NT_NP_TT_TP2, aes(x = WaterDepthcm, y = NtoTPandTtoP,color=Site,linetype=Site)) +
  ylab("Probability of a transition toward more Phragmites") +
  xlab("Water depth (cm)")+
  geom_point()+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F)+
  scale_linetype_manual(values = c("dashed", "solid", "dashed", "dashed"))

b1 <- glm(NtoTPandTtoP ~ Site*WaterDepthcm, family = binomial(link="logit"), data = NN_NT_NP_TT_TP2, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)


#I don't trust the BS data b/c only 4 of 55 plots had waterdepth>0
data.frame(NN_NT_NP_TT_TP2_BS%>%filter(WaterDepthcm>0))
length(NN_NT_NP_TT_TP2_BS$WaterDepthcm)

#Stats for 2020-2023
NN_NT_NP_TT_TP2_BP<-NN_NT_NP_TT_TP2%>%filter(Site=="Barataria")
NN_NT_NP_TT_TP2_BS<-NN_NT_NP_TT_TP2%>%filter(Site=="Bayou Sauvage")
NN_NT_NP_TT_TP2_BB<-NN_NT_NP_TT_TP2%>%filter(Site=="Big Branch")
NN_NT_NP_TT_TP2_PR<-NN_NT_NP_TT_TP2%>%filter(Site=="Pearl River")

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="cloglog"), data = NN_NT_NP_TT_TP2_BP, na.action=na.exclude)# 8 1s of 54
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP2_BP$NtoTPandTtoP);length(NN_NT_NP_TT_TP2_BP$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="cloglog"), data = NN_NT_NP_TT_TP2_BS, na.action=na.exclude)#22 1s of 55
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP2_BS$NtoTPandTtoP);length(NN_NT_NP_TT_TP2_BS$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="cloglog"), data = NN_NT_NP_TT_TP2_BB, na.action=na.exclude)# 9 1s of 71
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP2_BB$NtoTPandTtoP);length(NN_NT_NP_TT_TP2_BB$NtoTPandTtoP)

b1 <- glm(NtoTPandTtoP ~ WaterDepthcm, family = binomial(link="cloglog"), data = NN_NT_NP_TT_TP2_PR, na.action=na.exclude)# 1 1s of 70
drop1(b1,test="Chisq",.~.)
sum(NN_NT_NP_TT_TP2_PR$NtoTPandTtoP);length(NN_NT_NP_TT_TP2_PR$NtoTPandTtoP)




#Towards less phrag

#2020-2023 only
PP_PN_PT_TT_TN2<-PP_PN_PT_TT_TN%>%
  filter(Year>2019)

ggplot(data = PP_PN_PT_TT_TN2, aes(x = WaterDepthcm, y = PtoNTandTtoN,color=Site,linetype=Site)) +
  ylab("Probability of a transition toward more native") +
  xlab("Water depth (cm)")+
  geom_point()+
  geom_smooth(method='glm',method.args=list(family='binomial'),se=F)+
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed"))

b1 <- glm(PtoNTandTtoN ~ Site*WaterDepthcm, family = binomial(link="logit"), data = PP_PN_PT_TT_TN2, na.action=na.exclude)
drop1(b1,test="Chisq",.~.)


#I don't trust the BS data b/c only 1 plots had waterdepth>0
data.frame(PP_PN_PT_TT_TN2_BS%>%filter(WaterDepthcm>0))
data.frame(PP_PN_PT_TT_TN_BP%>%filter(WaterDepthcm>15))

#Stats on 2020-2023
PP_PN_PT_TT_TN2_BP<-PP_PN_PT_TT_TN2%>%filter(Site=="Barataria")
PP_PN_PT_TT_TN2_BS<-PP_PN_PT_TT_TN2%>%filter(Site=="Bayou Sauvage")
PP_PN_PT_TT_TN2_BB<-PP_PN_PT_TT_TN2%>%filter(Site=="Big Branch")
PP_PN_PT_TT_TN2_PR<-PP_PN_PT_TT_TN2%>%filter(Site=="Pearl River")

b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="cloglog"), data = PP_PN_PT_TT_TN2_BP, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)
b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="cloglog"), data = PP_PN_PT_TT_TN2_BS, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)
b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="cloglog"), data = PP_PN_PT_TT_TN2_BB, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)
b1 <- glm(PtoNTandTtoN ~ WaterDepthcm, family = binomial(link="cloglog"), data = PP_PN_PT_TT_TN2_PR, na.action=na.exclude)#
drop1(b1,test="Chisq",.~.)








##### Contingency tables #####

## For each site, 2X2 contingency table of 
## Blah this doesn't work b/c the T stasis is in there twice, and you'd add them together to get the second column sum which doesn't make sense
#     to pair    stasis
# N,T
# T,P

tr2BP<-tr2%>%filter(Site=="Barataria")
data.frame(tr2BP)

tbl<-table(tr2BP$State1,tr2BP$State2);tbl
table(tr2BP$State1,tr2BP$NtoTPandTtoP)
chisq.test(tbl)

#Note about contingency tables: using a chi sq test assumes that you have a lot of data. the chi 2 test in general relies on an approximation. if you have n<5, you could use a continuity correction which is an approximation to the fisher's exact test. But really you should use a fishers exact test in the first place. fisher's exact test is computationally intensive but any computer can now do it.
#https://stats.stackexchange.com/questions/362517/when-to-switch-off-the-continuity-correction-in-chisq-test-function
#however on further exploration, the fisher exact tests also makes some assumptions, for example that that row and column totals are fixed. also you can't do it on a 2 x 1 table.
#https://www.graphpad.com/guides/prism/latest/statistics/stat_chi-square_or_fishers_test.htm
#so for now I will keep it as is.
#alternatively I could use "simulate.p.value" but I think that this makes the same assumptions of the fisher exact test (in terms of row and column marginals)
#alternatively I could use a g.test but despite what Claudia's paper says, it sill suffers when samples size <5

###### Persistence of Phrag within a site ######
#          Stasis   to other
# one site
#

tr2BP<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1=="P")
#data.frame(tr2BP)

tbl<-table(tr2BP$Site,tr2BP$PtoNT);tbl
chisq.test(tbl, correct = FALSE)
chisq.test(tbl,correct=F,simulate.p.value = T)
g.test(tbl) #library(AMR)

##BS is persistent***
tr2BS<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1=="P")
#data.frame(tr2BS)

tbl<-table(tr2BS$Site,tr2BS$PtoNT);tbl
chisq.test(tbl, correct = FALSE)
assocstats(tbl)

tr2BB<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1=="P")
#data.frame(tr2BB)

tbl<-table(tr2BB$Site,tr2BB$PtoNT);tbl
chisq.test(tbl, correct = FALSE)

tr2PR<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1=="P")
#data.frame(tr2PR)

tbl<-table(tr2PR$Site,tr2PR$PtoNT);tbl
chisq.test(tbl, correct = FALSE)
#prop.test(tbl,correct=F) I'm not exactly sure why we wouldn't use this, the results are similar but not exactly that of the chisq test. but from the help file on chisq.test, it is valid to do a 1x2 chi squared test so I will stick with that.

###### Persistence of Transition within a site ######
#note 0 is changing, 1 is staying the same
tr2BP<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1=="T")
#data.frame(tr2BP)

tbl<-table(tr2BP$Site,tr2BP$TT);tbl
chisq.test(tbl, correct = FALSE)

tr2BS<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1=="T")
#data.frame(tr2BS)

tbl<-table(tr2BS$Site,tr2BS$TT);tbl
chisq.test(tbl, correct = FALSE)

#BB T is nearly significant p=0.05551
tr2BB<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1=="T")
#data.frame(tr2BB)

tbl<-table(tr2BB$Site,tr2BB$TT);tbl
chisq.test(tbl, correct = FALSE)

tr2PR<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1=="T")
#data.frame(tr2PR)

tbl<-table(tr2PR$Site,tr2PR$TT);tbl
chisq.test(tbl, correct = FALSE)

###### Persistence of Native within a site ######
#Barataria native is persistent**
tr2BP<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1=="N")
#data.frame(tr2BP)

tbl<-table(tr2BP$Site,tr2BP$NtoTP);tbl
chisq.test(tbl, correct = FALSE)

#BS native is persistent**
tr2BS<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1=="N")
#data.frame(tr2BS)

tbl<-table(tr2BS$Site,tr2BS$NtoTP);tbl
chisq.test(tbl, correct = FALSE)

#BB native is persistent **
tr2BB<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1=="N")
#data.frame(tr2BB)

tbl<-table(tr2BB$Site,tr2BB$NtoTP);tbl
chisq.test(tbl, correct = FALSE)

#PR native is persistent**
tr2PR<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1=="N")
#data.frame(tr2PR)

tbl<-table(tr2PR$Site,tr2PR$NtoTP);tbl
chisq.test(tbl, correct = T) #note the correction does not work for 1 x 2 tables
chisq.test(tbl, correct = F)
g.test(tbl)



###### Pairwise testing persistence of phrag ######

#           stasis    to other 
# BP Phrag
# BS Phrag

#Use the correct = TRUE option, if expected counts in any cell in the contingency table are less than 5
#BP BS
tr2BPBS<-tr2%>%
  filter(Site=="Barataria"|Site=="Bayou Sauvage")%>%
  filter(State1=="P")
#data.frame(tr2BPBS)

#tbl<-table(tr2BPBS$Site,tr2BPBS$PP);tbl
tbl<-table(tr2BPBS$Site,tr2BPBS$PtoNT);tbl
chisq.test(tbl, correct = FALSE)

#how is this different from logistic regression. it is really close but a tiny bit different. i think is is basically the same thing
m1<-glm(PP~Site, family=binomial,data=tr2BPBS)
drop1(m1,test="Chisq")

#BP BB
tr2BPBB<-tr2%>%
  filter(Site=="Barataria"|Site=="Big Branch")%>%
  filter(State1=="P")
#data.frame(tr2BPBB)

tbl<-table(tr2BPBB$Site,tr2BPBB$PtoNT);tbl
chisq.test(tbl, correct = FALSE)

#BP PR
tr2BPPR<-tr2%>%
  filter(Site=="Barataria"|Site=="Pearl River")%>%
  filter(State1=="P")
#data.frame(tr2BPPR)

tbl<-table(tr2BPPR$Site,tr2BPPR$PtoNT);tbl
chisq.test(tbl, correct = FALSE)

#BS BB
tr2BSBB<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Big Branch")%>%
  filter(State1=="P")
#data.frame(tr2BSBB)

tbl<-table(tr2BSBB$Site,tr2BSBB$PtoNT);tbl
chisq.test(tbl, correct = FALSE)

#BS PR *****different
tr2BSPR<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Pearl River")%>%
  filter(State1=="P")
#data.frame(tr2BSPR)

tbl<-table(tr2BSPR$Site,tr2BSPR$PtoNT);tbl
chisq.test(tbl, correct = FALSE)
m1<-glm(PP~Site, family=binomial,data=tr2BSPR)
drop1(m1,test="Chisq")

#BB PR
tr2BBPR<-tr2%>%
  filter(Site=="Big Branch"|Site=="Pearl River")%>%
  filter(State1=="P")
#data.frame(tr2BBPR)

tbl<-table(tr2BBPR$Site,tr2BBPR$PtoNT);tbl
chisq.test(tbl, correct = FALSE)


###### Pairwise testing persistence of transition ######

#           stasis    to other 
# BP Trans
# BS Trans

#Use the correct = TRUE option, if expected counts in any cell in the contingency table are less than 5
#BP BS
tr2BPBS<-tr2%>%
  filter(Site=="Barataria"|Site=="Bayou Sauvage")%>%
  filter(State1=="T")
#data.frame(tr2BPBS)

tbl<-table(tr2BPBS$Site,tr2BPBS$TT);tbl
chisq.test(tbl, correct = FALSE)

#BP BB
tr2BPBB<-tr2%>%
  filter(Site=="Barataria"|Site=="Big Branch")%>%
  filter(State1=="T")
#data.frame(tr2BPBB)

tbl<-table(tr2BPBB$Site,tr2BPBB$TT);tbl
chisq.test(tbl, correct = FALSE)

#BP PR
tr2BPPR<-tr2%>%
  filter(Site=="Barataria"|Site=="Pearl River")%>%
  filter(State1=="T")
#data.frame(tr2BPPR)

tbl<-table(tr2BPPR$Site,tr2BPPR$TT);tbl
chisq.test(tbl, correct = FALSE)

#BS BB ***** significantly different, BS is more movey, BB is more stable
tr2BSBB<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Big Branch")%>%
  filter(State1=="T")
#data.frame(tr2BSBB)
View(data.frame(tr2BSBB%>%arrange(Site,years)))

tbl<-table(tr2BSBB$Site,tr2BSBB$TT);tbl
chisq.test(tbl, correct = FALSE)

#BS PR *****nearly sig, BS is more movey, PR is more stable
tr2BSPR<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Pearl River")%>%
  filter(State1=="T")
#data.frame(tr2BSPR)

tbl<-table(tr2BSPR$Site,tr2BSPR$TT);tbl
chisq.test(tbl, correct = FALSE)

#BB PR
tr2BBPR<-tr2%>%
  filter(Site=="Big Branch"|Site=="Pearl River")%>%
  filter(State1=="T")
#data.frame(tr2BBPR)

tbl<-table(tr2BBPR$Site,tr2BBPR$TT);tbl
chisq.test(tbl, correct = FALSE)


###### Pairwise testing persistence of native ######

#           stasis    to other 
# BP native
# BS native

#Use the correct = TRUE option, if expected counts in any cell in the contingency table are less than 5
#BP BS ****nearly sig, Barataria is more stable
tr2BPBS<-tr2%>%
  filter(Site=="Barataria"|Site=="Bayou Sauvage")%>%
  filter(State1=="N")
#data.frame(tr2BPBS)

tbl<-table(tr2BPBS$Site,tr2BPBS$NtoTP);tbl
chisq.test(tbl, correct = FALSE)

#BP BB
tr2BPBB<-tr2%>%
  filter(Site=="Barataria"|Site=="Big Branch")%>%
  filter(State1=="N")
#data.frame(tr2BPBB)

tbl<-table(tr2BPBB$Site,tr2BPBB$NtoTP);tbl
chisq.test(tbl, correct = FALSE)
fisher.test(tbl)

#BP PR
tr2BPPR<-tr2%>%
  filter(Site=="Barataria"|Site=="Pearl River")%>%
  filter(State1=="N")
#data.frame(tr2BPPR)

tbl<-table(tr2BPPR$Site,tr2BPPR$NtoTP);tbl
chisq.test(tbl, correct = T)
fisher.test(tbl)

#BS BB
tr2BSBB<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Big Branch")%>%
  filter(State1=="N")
#data.frame(tr2BSBB)

tbl<-table(tr2BSBB$Site,tr2BSBB$NtoTP);tbl
chisq.test(tbl, correct = FALSE)

#BS PR ***** sig, BS is more movey, PR is more stable
tr2BSPR<-tr2%>%
  filter(Site=="Bayou Sauvage"|Site=="Pearl River")%>%
  filter(State1=="N")
#data.frame(tr2BSPR)

tbl<-table(tr2BSPR$Site,tr2BSPR$NtoTP);tbl
chisq.test(tbl, correct = T)

#BB PR ***nearly sig, big branch is more movey, pr more stable
tr2BBPR<-tr2%>%
  filter(Site=="Big Branch"|Site=="Pearl River")%>%
  filter(State1=="N")
#data.frame(tr2BBPR)

tbl<-table(tr2BBPR$Site,tr2BBPR$NtoTP);tbl
chisq.test(tbl, correct = T)



###### Pairwise testing directionality ######

#           to pair    stasis    
# BP phrag
# BP trans

#Use the correct = TRUE option, if expected counts in any cell in the contingency table are less than 5
#overall, using the simulation or fisher exact test gives qualitatively similar results. the only thing that changes occasionally is that the simulation and fisher tests are nearly significant when the correct=T restuls are like p=0.15

#BP, T&P
tr2BP_TP<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1!="N",State2!="N")

#1 is stasis
tbl<-table(tr2BP_TP$State1,tr2BP_TP$stasis);tbl
chisq.test(tbl, correct = FALSE)

#BP, N&P
tr2BP_NP<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1!="T",State2!="T")

tbl<-table(tr2BP_NP$State1,tr2BP_NP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#BP, N&T
#sig, directionally from T to N
tr2BP_NT<-tr2%>%
  filter(Site=="Barataria")%>%
  filter(State1!="P",State2!="P")

tbl<-table(tr2BP_NT$State1,tr2BP_NT$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)


#BS, T&P
#significant, directionally from T to P
tr2BS_TP<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1!="N",State2!="N")

tbl<-table(tr2BS_TP$State1,tr2BS_TP$stasis);tbl
chisq.test(tbl, correct = FALSE)

#BS, N&P
tr2BS_NP<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1!="T",State2!="T")

tbl<-table(tr2BS_NP$State1,tr2BS_NP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#BS, N&T
tr2BS_NT<-tr2%>%
  filter(Site=="Bayou Sauvage")%>%
  filter(State1!="P",State2!="P")

tbl<-table(tr2BS_NT$State1,tr2BS_NT$stasis);tbl
chisq.test(tbl, correct = F)


#BB, T&P
#nearly significant, directionally from P to T
tr2BB_TP<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1!="N",State2!="N")

tbl<-table(tr2BB_TP$State1,tr2BB_TP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#BB, N&P
#significant, directionally from P to N
tr2BB_NP<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1!="T",State2!="T")

tbl<-table(tr2BB_NP$State1,tr2BB_NP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#BB, N&T
#sig, directionally from T to N
tr2BB_NT<-tr2%>%
  filter(Site=="Big Branch")%>%
  filter(State1!="P",State2!="P")

tbl<-table(tr2BB_NT$State1,tr2BB_NT$stasis);tbl
chisq.test(tbl, correct = F)


#PR, T&P
#significant, directionally from P to T
tr2PR_TP<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1!="N",State2!="N")

tbl<-table(tr2PR_TP$State1,tr2PR_TP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#PR, N&P
tr2PR_NP<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1!="T",State2!="T")

tbl<-table(tr2PR_NP$State1,tr2PR_NP$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)

#PR, N&T
#sigificant, directionally from T to N
tr2PR_NT<-tr2%>%
  filter(Site=="Pearl River")%>%
  filter(State1!="P",State2!="P")

tbl<-table(tr2PR_NT$State1,tr2PR_NT$stasis);tbl
chisq.test(tbl, correct = T)
chisq.test(tbl, correct = F, simulate.p.value = T)
fisher.test(tbl)






##### Transition probabilities and CRMS data ####
biomass4f #has CRMS data averaged by site


##################note these might be wrong b/c I had some NAs that were not removed ####### REDO ALL THESE FIGURES ###############
#### 5/6/24 unfortunately I have forgotten what this issue was about. i am not using the crms data anymore and my new tr2. I changed the tr2 to have 499 rows instead of 504 so maybe that fixed things?

head(data.frame(tr2))

tr3<-tr2%>%
  select(NN:Year,NtoTP,NtoTPandTtoP)%>%
  group_by(Site,Year)%>%
  summarize(across(where(is.numeric), list(mean = mean, se = std.error), na.rm = TRUE))%>%
  full_join(biomass4f)
data.frame(tr3)


ggplot(data=tr3, aes(x=CRMSsalinity,y = NtoTPandTtoP_mean,color=Site))+#,color=Transect
  labs(x="Growing Season Salinity (ppt)",y="Probability of transitioning toward more Phragmites") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_errorbar(aes(ymin=NtoTPandTtoP_mean-NtoTPandTtoP_se,ymax=NtoTPandTtoP_mean+NtoTPandTtoP_se))+
  geom_smooth(method="lm",se=F)#+
#facet_wrap(vars(Site),strip.position = "top")#,scales="free"

ggplot(data=tr3, aes(x=CRMSsalinity,y = NtoTPandTtoP_mean,color=Site))+#,color=Transect
  labs(x="Growing Season Salinity (ppt)",y="Probability of transitioning toward more Phragmites") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_errorbar(aes(ymin=NtoTPandTtoP_mean-NtoTPandTtoP_se,ymax=NtoTPandTtoP_mean+NtoTPandTtoP_se))+
  geom_smooth(method="lm",se=F)#+
#facet_wrap(vars(Site),strip.position = "top")#,scales="free"





##### Change in relative phrag biomass #####

biomass<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Corrected_Biomass2023NAs.csv") #

head(biomass)
dim(biomass)

biomass2<-biomass%>%
  arrange(Year,Plot) %>%#should be arranged but just in case
  select(Year, Site, Transect, Plot, pH, Salinity15cmppt,WaterDepthcm,Lat,Long,NatRichness,Litter,Phragmites.australis.Biomass.g.,TOTAL.BIOMASS,Percentage.Phrag,Percentage.Native, Native.Biomass ) #added Richness and Litter

head(biomass2)

#Make a dataframe with year 2 environmental info, and year 1 and year 2 phrag/native info
#rows 1-84, 85-168, 169-252,253-336,337-420,421-504,505-588
biomass3<-cbind(biomass2[85:588,1:9],biomass2[1:504,10:16],biomass2[85:588,10:16])
colnames(biomass3)[10:23]<-c("NatRichness1","Litter1","PhragBiomass1","TotalBiomass1","PercentPhrag1","PercentNative1","NativeBiomass1","NatRichness2","Litter2","PhragBiomass2","TotalBiomass2","PercentPhrag2","PercentNative2","NativeBiomass2")
head(biomass3)

biomass4<-biomass3%>%
  mutate(ChangePhrag=(PercentPhrag2-PercentPhrag1)*100)
head(biomass4)

#for playing with averages within our dataset (not crms)
biomass4a<-biomass2%>%
  filter(is.na(Phragmites.australis.Biomass.g.)==F)%>%
  group_by(Plot)%>%
  summarize(sum=sum(Phragmites.australis.Biomass.g.))
ind<-biomass4a$Plot[which(biomass4a$sum>0)]

biomass4b<-biomass4%>%
  filter(Plot%in%ind)
biomass4c<-biomass4b%>%
  filter(Year%in%c(2020,2021,2022,2023))


#Figures by plot

#biomass4 plots everything, biomass4b is just plots that at least had some phrag at some point in time, biomass4c is plots that had at least some phrag and only plots where water depth was measured at the plot level

#Salinity
pdf("xxxx.pdf",width=6.5,height=4.5)
ggplot(data=biomass4b, aes(x=Salinity15cmppt,y = ChangePhrag,color=Site))+#,color=Transect
  labs(x="Salinity (ppt)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"
dev.off()

#Water depth
ggplot(data=biomass4b, aes(x=WaterDepthcm,y = ChangePhrag,color=Site))+#,color=Transect
  labs(x="Water depth (cm)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"

#pH
ggplot(data=biomass4b, aes(x=pH,y = ChangePhrag))+#,color=Transect
  labs(x="pH",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"

#Litter mass, not sure what units litter is in, probably just g in the 20x20 square. this is kind of confusing b/c litter is generated by phrag, so the causality is tricky
ggplot(data=biomass4b, aes(x=Litter2,y = ChangePhrag,color=Site))+#,color=Transect
  labs(x="Litter (g)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"

#Native richness
ggplot(data=biomass4b, aes(x=NatRichness2,y = ChangePhrag))+#,color=Transect
  labs(x="Native Richness",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_smooth(method="lm",se=F)+
  facet_wrap(vars(Site),strip.position = "top")#,scales="free"

#Figures by year with standard errors

#Barataria CRMS0188-H01
#Pearl River	CRMS4110-H01
#Turtle cove	CRMS0030-H01
#Fontainblebleau	CRMS2854-H01
#Big Branch	CRMS0006-H01
#Bayou Sauvage	CRMS4107-H01
#LUMCON1	CRMS0311-H01
#LUMCON2	CRMS0311-H01

#"growing year = Mar 1 to Oct 31" according to CRMS

##### CRMS salinity #####
sals<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/HYDROGRAPHIC_MONTHLY.csv")
sals<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/HYDROGRAPHIC_MONTHLYMay2024c.csv")

head(sals)
#note that the latest measurement is sometimes in 9/23. So I should download updated data in a few months to get october 2023. note after the fact, there are V plots and P plots (station back), V is the veg plots that they survey once in jul/aug/sept, P is the soil porewater plots that they survey at least 5 times per year. I will just use the P plots so numbers aren't biased toward the summer
sals2<-sals%>%
  separate(CPRA.Station.ID,c("StationFront","ID"),"-",remove=F)%>%
  separate(ID,c("StationType",NA),1,remove=F)%>%
  filter(StationFront%in%c("CRMS0188","CRMS4110","CRMS0006","CRMS4107"))%>%
  filter(Measurement.Depth..ft.==.328)%>%
  filter(StationType=="P")%>%
  dplyr::select(Station=CPRA.Station.ID,StationFront,ID,StationType,Date=Date..mm.dd.yyyy.,Salinityppt=Soil.Porewater.Salinity..ppt.)%>%
  separate(Date,c("Month","Day","Year"),"/",remove=F)%>%
  filter(Year%in%c("17","18","19","20","21","22","23"),StationType=="P")%>%
  filter(Month%in%c("3","4","5","6","7","8","9","10"))%>%  
  mutate(StationFront=case_match(StationFront,"CRMS0006"~"Big Branch","CRMS0188"~"Barataria","CRMS4110"~"Pearl River","CRMS4107"~"Bayou Sauvage",.ptype = factor(levels = c("Barataria","Bayou Sauvage","Pearl River","Big Branch"))))%>%
  group_by(StationFront, Year)%>% 
  summarise(AnnualSalinityppt=mean(Salinityppt, na.rm=T))%>%
  mutate(Year=as.numeric(Year)+2000)%>%
  rename(Site=StationFront)%>%
  rename(CRMSsalinity=AnnualSalinityppt)%>%
  group_by(Site)%>%
  summarise(CRMSgrowingseasonsalinity=mean(CRMSsalinity, na.rm=T))
  

head(sals2)
data.frame(sals2)
#sals2$Site<-c("Big Branch","Barataria","Bayou Sauvage","Pearl River")
#sals2[order(sals2$AnnualSalinityppt),]

#without the plots where phrag never was, used for below figures
biomass4d<-biomass4b%>%
  select(-Plot)%>%
  group_by(Site,Year)%>%
  summarize(across(where(is.numeric), list(mean = mean, se = std.error), na.rm = TRUE))%>%
  left_join(sals2)
data.frame(biomass4d)

#with the plots where phrag never was, used for above figures for transitions
biomass4f<-biomass4%>%
  select(-Plot)%>%
  group_by(Site,Year)%>%
  summarize(across(where(is.numeric), list(mean = mean, se = std.error), na.rm = TRUE))%>%
  left_join(sals2)
data.frame(biomass4f)

ggplot(data=biomass4d, aes(x=CRMSsalinity,y = ChangePhrag_mean,color=Site))+#,color=Transect
  labs(x="Growing Season Salinity (ppt)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)),axis.text.x = element_text(angle = 35, vjust=1, hjust=1))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_errorbar(aes(ymin=ChangePhrag_mean-ChangePhrag_se,ymax=ChangePhrag_mean+ChangePhrag_se))+
  geom_smooth(method="lm",se=F)#+
  #facet_wrap(vars(Site),strip.position = "top")#,scales="free"


  
##### CRMS water depth #####\
#go here and get all stations from individual CRMS sites: https://cims.coastal.louisiana.gov/DataDownload/DataDownload.aspx?type=hydro_hourly
#note that the last date for some sites is in 9/23 so i will need to go back and get the october data when it is ready and redo everything
watBP<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/BP_HYDROGRAPHIC_HOURLY.csv") #last date 10/18/23
watBS<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/BS_HYDROGRAPHIC_HOURLY.csv") #last date 10/3/23
watBB<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/BB_HYDROGRAPHIC_HOURLY.csv") #last datte 9/19/23
watPR<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/PR_HYDROGRAPHIC_HOURLY.csv") #last date 9/27/23
  
wat<-rbind(watBP,watBS,watBB,watPR)%>%
  separate(Station.ID,c("StationFront","ID"),"-",remove=F)%>%
  separate(ID,c("StationType",NA),1,remove=F)%>%
  dplyr::select(Station=Station.ID,StationFront,ID,StationType,Date=Date..mm.dd.yyyy.,Time=Time..hh.mm.ss.,waterdepthmarsh=Adjusted.Water.Elevation.to.Marsh..ft.,marshelevationdatum=Adjusted.Marsh.Mat.Elevation.to.Datum..ft.,waterdepthdatum=Adjusted.Water.Elevation.to.Datum..ft.)%>%
  separate(Date,c("Month","Day","Year"),"/",remove=F)%>%
  filter(Year%in%c("17","18","19","20","21","22","23"))%>%
  filter(StationType=="H")%>%
  #filter(StationType=="M")%>%
  #filter(Station!="CRMS0188-H01")%>%
  filter(Month%in%c("3","4","5","6","7","8","9","10"))%>%  
  mutate(StationFront=case_match(StationFront,"CRMS0006"~"Big Branch","CRMS0188"~"Barataria","CRMS4110"~"Pearl River","CRMS4107"~"Bayou Sauvage",.ptype = factor(levels = c("Barataria","Bayou Sauvage","Pearl River","Big Branch"))))%>%
  group_by(StationFront, Year)%>%
  summarize(across(where(is.numeric), list(mean = mean), na.rm = TRUE))%>%
  mutate(Year=as.numeric(Year)+2000)%>%
  rename(Site=StationFront)%>%
  mutate(waterdepthmarsh_mean=waterdepthmarsh_mean/0.0328084,marshelevationdatum_mean=marshelevationdatum_mean/0.0328084,waterdepthdatum_mean=waterdepthdatum_mean/0.0328084)
 

head(wat)
data.frame(wat)

#I think I should use the -M sites for Barataria, because it is a flotant marsh. However unfortunately there are only M sites in 2017,2018, and 2023. So I will try using the regular H data which I think does not account for it being a flotant marsh. but wait the regular H data does not calculate adjusted to marsh elevation. i'm not sure why not. i could probalby calculate it but i'd have to pull marsh elevation data from teh M plots which we don't have.
#I should probably use water depth adjusted to marsh however still this might not be a good estimate of our exact site b/c it depends on how high the CRMS marsh sits
unique(wat$Station.ID)


biomass4e<-biomass4b%>%
  select(-Plot)%>%
  group_by(Site,Year)%>%
  summarize(across(where(is.numeric), list(mean = mean, se = std.error), na.rm = TRUE))%>%
  left_join(wat)
data.frame(biomass4e)

ggplot(data=biomass4e, aes(x=waterdepthdatum_mean,y = ChangePhrag_mean,color=Site))+#,color=Transect
  labs(x="Growing Season Water depth (cm)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_errorbar(aes(ymin=ChangePhrag_mean-ChangePhrag_se,ymax=ChangePhrag_mean+ChangePhrag_se))+
  geom_smooth(method="lm",se=F)#+
  #facet_wrap(vars(Site),strip.position = "top",scales="free")#

ggplot(data=biomass4e, aes(x=waterdepthmarsh_mean,y = ChangePhrag_mean,color=Site))+#,color=Transect
  labs(x="Growing Season Water depth (cm)",y="Change in Phrag Relative Abundance") +
  theme_classic()+
  theme(line=element_line(linewidth =.3),text=element_text(size=12),strip.background = element_rect(colour="white", fill="white"),strip.text.x = element_text(hjust = 0, margin=margin(l=0)))+#,legend.position = "none"
  geom_point(size=1.8)+
  geom_errorbar(aes(ymin=ChangePhrag_mean-ChangePhrag_se,ymax=ChangePhrag_mean+ChangePhrag_se))+
  geom_smooth(method="lm",se=F)#+
#facet_wrap(vars(Site),strip.position = "top",scales="free")#







biomass5<-biomass4%>%
  filter(Site=="Big Branch")

m1<-lme(ChangePhrag~Salinity15cmppt,random=~1|Plot,data=biomass5,na.action=na.omit)
anova(m1,type="marginal")

#All sites
#+Yearfac + Yearfac*Site 
m1 <- lme(
  ChangePhrag ~ Site + Salinity15cmppt+ Site*Salinity15cmppt,
  #correlation = corAR1(form =~ Year|Plot),
  random = ~1|Plot, data = biomass4,na.action=na.omit)
summary(m1)
anova(m1,type="marginal")

hist(resid(m1,type="normalized"))
plot(biomass4$Salinity15cmppt[is.na(biomass4$Salinity15cmppt)==F&is.na(biomass4$ChangePhrag)==F],resid(m1,type="normalized"))

m1 <- lme(
  ChangePhrag ~ Salinity15cmppt,
  random = ~Salinity15cmppt|Site, data = biomass4,na.action=na.omit)
summary(m1)
anova(m1,type="marginal")








##### Old code for other bins ####
##### Using Bin1 #####

rivdat2<-rivdat%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin1)%>%
  mutate(Bin1=recode_factor(Bin1,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin1)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)


as.data.frame(rivdat2)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdat2$N1,
    N2 = rivdat2$N2,
    Value = rivdat2$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -44L), class = "data.frame")#

# edgesp <- structure(
#   list(
#     N1 = rep(c("Native","Transition","Phragmites","N2","T2","P2","N3","T3","P3","N4","T4","P4","N5","T5","P5"),each=3),
#     N2 = c(rep(c("N2","T2","P2"),3),rep(c("N3","T3","P3"),3),rep(c("N4","T4","P4"),3),rep(c("N5","T5","P5"),3),rep(c("N6","T6","P6"),3)),
#     Value = c(48,2,1,1, 1,13,2,35,2,2,43,5,17,3,2,28, 2,10,2,3, 2,1,21,21, 2,1,2,37, 4,13,2,2, 3,11,1,1, 4,3,16,2, 12, 41, 5,31,19)
#   ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -45L), class = "data.frame")#

#edgesp <- edgesp[edgesp$Value > 0, ]
RP <- makeRiver(nodesp, edgesp)
plot(RP, plot_area=0.9)




##### Using Bin3  #####

rivdat2<-rivdat%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin3)%>%
  mutate(Bin3=recode_factor(Bin3,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin3)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdat2)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdat2$N1,
    N2 = rivdat2$N2,
    Value = rivdat2$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -40L), class = "data.frame")#

RP <- makeRiver(nodesp, edgesp)
plot(RP, plot_area=0.9)


##### Using Bin5  #####

rivdat2<-rivdat%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin5)%>%
  mutate(Bin5=recode_factor(Bin5,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin5)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdat2)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdat2$N1,
    N2 = rivdat2$N2,
    Value = rivdat2$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -39L), class = "data.frame")#

RP <- makeRiver(nodesp, edgesp)
plot(RP, plot_area=0.9)


##### Separating by site - Barataria #####
# Using old Bin6 
rivdatBP<-rivdat%>%
  filter(Site=="Barataria")%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBP)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBP$N1,
    N2 = rivdatBP$N2,
    Value = rivdatBP$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -33L), class = "data.frame")#

RPBP <- makeRiver(nodesp, edgesp)
plot(RPBP, plot_area=0.9)


##### Separating by site - Big Branch #####
# Using Bin6 
rivdatBB<-rivdat%>%
  filter(Site=="Big Branch")%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBB)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBB$N1,
    N2 = rivdatBB$N2,
    Value = rivdatBB$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -29L), class = "data.frame")#

RPBB <- makeRiver(nodesp, edgesp)
plot(RPBB, plot_area=0.9)



##### Separating by site - Bayou Sauvage #####
# Using Bin6 
rivdatBS<-rivdat%>%
  filter(Site=="Bayou Sauvage")%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatBS)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatBS$N1,
    N2 = rivdatBS$N2,
    Value = rivdatBS$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -32L), class = "data.frame")#

RPBS <- makeRiver(nodesp, edgesp)
plot(RPBS, plot_area=0.9)


##### Separating by site - Pearl River #####
# Using Bin6 
rivdatPR<-rivdat%>%
  filter(Site=="Pearl River")%>%
  arrange(Plot,Year)%>%
  select(Year,Plot,Bin6)%>%
  mutate(Bin6=recode_factor(Bin6,"Low"="N","Mid"="T","High"="P"))%>%
  mutate(Year=recode_factor(Year,"2017"="y1","2018"="y2","2019"="y3","2020"="y4","2021"="y5","2022"="y6"))%>%
  pivot_wider(names_from=Year,values_from=Bin6)%>%
  mutate(y1=recode_factor(y1,"N"="N1","T"="T1","P"="P1"))%>%
  mutate(y2=recode_factor(y2,"N"="N2","T"="T2","P"="P2"))%>%
  mutate(y3=recode_factor(y3,"N"="N3","T"="T3","P"="P3"))%>%
  mutate(y4=recode_factor(y4,"N"="N4","T"="T4","P"="P4"))%>%
  mutate(y5=recode_factor(y5,"N"="N5","T"="T5","P"="P5"))%>%
  mutate(y6=recode_factor(y6,"N"="N6","T"="T6","P"="P6"))%>%
  unite("y1_y2",y1,y2,sep="_",remove=F)%>%
  unite("y2_y3",y2,y3,sep="_",remove=F)%>%
  unite("y3_y4",y3,y4,sep="_",remove=F)%>%
  unite("y4_y5",y4,y5,sep="_",remove=F)%>%
  unite("y5_y6",y5,y6,sep="_",remove=F)%>%
  filter(is.na(y2)==F)%>%
  select(Plot,y1_y2,y2_y3,y3_y4,y4_y5,y5_y6)%>%
  pivot_longer(y1_y2:y5_y6,names_to="years",values_to = "change")%>%
  mutate(ones=1)%>%
  group_by(years,change)%>%
  summarise(sum=sum(ones))%>%
  mutate(change=factor(change,levels=c("N1_N2", "N1_T2", "N1_P2", "T1_N2", "T1_T2", "T1_P2", "P1_N2", "P1_T2", "P1_P2",
                                       "N2_N3", "N2_T3", "N2_P3", "T2_N3", "T2_T3", "T2_P3", "P2_N3", "P2_T3", "P2_P3",
                                       "N3_N4", "N3_T4", "N3_P4", "T3_N4", "T3_T4", "T3_P4", "P3_N4", "P3_T4", "P3_P4",
                                       "N4_N5", "N4_T5", "N4_P5", "T4_N5", "T4_T5", "T4_P5", "P4_N5", "P4_T5", "P4_P5",
                                       "N5_N6", "N5_T6", "N5_P6", "T5_N6", "T5_T6", "T5_P6", "P5_N6", "P5_T6", "P5_P6")))%>%
  arrange(change)%>%
  separate(change, c("N1","N2"), remove=F)

as.data.frame(rivdatPR)


nodesp <- structure(
  list(
    ID = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L), .Label = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6"), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,4L,4L,4L,5L,5L,5L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue","red","darkgreen","blue", "red","darkgreen","blue","red","darkgreen","blue")), 
  .Names = c("ID", "x", "col"),
  row.names = c("N1", "T1", "P1", "N2", "T2", "P2","N3","T3", "P3", "N4","T4","P4","N5","T5","P5","N6","T6","P6" ), class = "data.frame")

edgesp <- structure(
  list(
    N1 = rivdatPR$N1,
    N2 = rivdatPR$N2,
    Value = rivdatPR$sum
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -26L), class = "data.frame")#

RPPR <- makeRiver(nodesp, edgesp)
plot(RPPR, plot_area=0.9)




#### example from package ####

x <- riverplot.example()
plot( x )
x <- riverplot.example(no=2)
riverplot(x, lty=1, plot_area=1, disentangle=TRUE, 
          gravity="c", default_style=list(nodestyle="invisible"))


##### from Claudia #####
library(riverplot)

#!version 0.3 best, 0.5 shows lines for zero flow, too
#all years

nodes <- structure(
  list(
    ID = structure(c(1L, 3L, 5L, 2L, 4L, 6L, 7L, 8L, 9L, 10L, 11L,12L,13L,14L,15L, 16L,17L,18L,19L, 20L,21L,22L,23L),
                   .Label = c("Medusahead", "M", "Annuals", "A", "Perennials", "P","Forbs","M2", "A2", "P2","Forbs2","M3", "A3", "P3","Forbs3", "M4", "A4", "P4","Forbs4", "M5", "A5", "P5","Forbs5" ), class = "factor"),
    x = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L,4L,4L,4L, 5L,5L,5L,5L, 6L, 6L, 6L, 6L),
    col = c("red","darkgreen","blue","red","darkgreen","blue","orange","red","darkgreen","blue","orange","red","darkgreen","blue","orange", "red","darkgreen","blue","orange","red","darkgreen","blue","orange")), 
  .Names = c("ID", "x", "col"),
  row.names = c("Medushead", "Annuals", "Perennials", "M", "A", "P","Forbs","M2", "A2", "P2","Forbs2","M3", "A3", "P3","Forbs3", "M4", "A4", "P4","Forbs4", "M5", "A5", "P5","Forbs5"), class = "data.frame")

edges <- structure(
  list(
    N1 = c("Medusahead", "Medusahead", "Medusahead", "Medusahead","Annuals", "Annuals", "Annuals","Annuals", "Perennials", "Perennials", "Perennials","Perennials", "M","M","M","M", "A","A","A","A", "P","P","P","P","Forbs","Forbs","Forbs","Forbs","M2","M2","M2","M2", "A2","A2","A2","A2", "P2","P2","P2","P2","Forbs2","Forbs2","Forbs2","Forbs2","M3","M3","M3","M3", "A3","A3","A3","A3", "P3","P3","P3","P3","Forbs3","Forbs3","Forbs3","Forbs3", "M4","M4","M4","M4", "A4","A4","A4","A4", "P4","P4","P4","P4","Forbs4","Forbs4","Forbs4","Forbs4"),
    N2 = c("M", "A", "P", "Forbs","M", "A", "P", "Forbs","M", "A", "P","Forbs", "M2", "A2", "P2","Forbs2","M2", "A2", "P2","Forbs2","M2", "A2", "P2","Forbs2","M2", "A2", "P2","Forbs2","M3", "A3", "P3","Forbs3","M3", "A3", "P3","Forbs3","M3", "A3", "P3","Forbs3","M3", "A3", "P3","Forbs3","M4", "A4", "P4","Forbs4","M4", "A4", "P4","Forbs4","M4", "A4", "P4","Forbs4","M4", "A4", "P4","Forbs4", "M5", "A5", "P5","Forbs5","M5", "A5", "P5","Forbs5","M5", "A5", "P5","Forbs5","M5", "A5", "P5","Forbs5"),
    Value = c(48,0,0,0, 0,13,0,35, 0,0,43,5,17,3,0,28, 0,10,0,3, 0,1,21,21, 0,1,2,37, 4,13,0,0, 3,11,1,0, 4,3,16,0, 12, 41, 5,31,19,0,1,3, 27,31,2,8, 5,1,15,1, 5,2,2,22,20,13,3,20, 4,21,0,9, 5,2,9,4, 3,1,0,30)
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -76L), class = "data.frame")

edges <- edges[edges$Value > 0, ]
RP <- makeRiver(nodes, edges)
plot(RP, plot_area=0.9)







