#Ribbon plots

#based on: https://search.r-project.org/CRAN/refmans/riverplot/html/riverplot-package.html

#install.packages("riverplot")
library(riverplot)
library(tidyverse)

#rivdat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Phragbiomass2023.csv") #old with only bins 1-4
rivdat<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/LAmarsh/Survey/Stats/Temporal/Phragnative1.csv")

head(rivdat)
View(rivdat)

#Bin1: Value-based intervals with the cutoffs being 0 to 0.33, 0.33 to 0.66, and 0.66 to 1
#Bin2: Value-based intervals with the cutoffs being 0 to 0.25, 0.25 to 0.5, and 0.5 to 1
#Bin3: Percentile-based intervals based on the total dataset's 33rd and 66th percentile values acting as cutoffs
#Bin4: Percentile-based intervals sub-setted by Site using 33rd and 66th percentile values acting as cutoffs for site-specific groupings
#Bin5: Percentile-based intervals based on the total dataset's 33rd and 66th percentile values acting as cutoffs from the base year 2017
#Bin6: Percentile-based intervals sub-setted by Site using 33rd and 66th percentile values acting as cutoffs for site-specific groupings from the base year 2017

#plot 112 and 113 are missing in year2

#finding percentiles
#upshot is I'm not sure how Coleman got the percentiles....oh maybe he did it in terms of relative biomass not just Phrag biomass
quantile(rivdat$Phragmites.australis.Biomass.g., c(.33, .66)) 
#odd the 33rd percentile is 0, how did that go down? i think this is what happened and it is awkward
cbind(rivdat$Phragmites.australis.Biomass.g.,rivdat$Bin3)
length(which(rivdat$Bin3=="Low"))
length(which(rivdat$Bin3=="Mid"))
length(which(rivdat$Bin3=="High"))


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


##### Using Bin6 #####
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


##### Using Bin6 #####
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
  ), .Names = c("N1", "N2", "Value"), row.names = c(NA, -25L), class = "data.frame")#

RPBB <- makeRiver(nodesp, edgesp)
plot(RPBB, plot_area=0.9)




##### Separating by site - Bayou Sauvage #####


##### Using Bin6 #####
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


##### Using Bin6 #####
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
