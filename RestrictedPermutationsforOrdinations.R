##### Restricted permutation tests for repeated measures ordinations #####

# The "normal" models are sketchy because we have repeated measures data and they aren't independent. This is taken from Gavin Simpson's powerpoint and gihub page: https://github.com/gavinsimpson/advanced-vegan-webinar-july-2020

# Going through the tutorial in https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#this is downloaded as "Applied Multivariate Statistics in R" book


##### All sites, all transects, 2017 and 2022 #####

#data from EmilyFarrer.R
envTY
speTYc
ind<-which(!envTY$Plot%in%c(111))
speTYd<-speTYc[ind,]
envTYb<-envTY[ind,]

#calculate dissimilarity directly
temp<-vegdist(speTYd,method="bray",binary=F)
tempsqr<-sqrt(temp)
library(ecotraj)
is.metric(tempsqr)

# references for issues of negative eiganvalues:
#https://www.google.com/books/edition/Multidimensional_Scaling_Second_Edition/SKZzmEZqvqkC?hl=en&gbpv=1&bsq=cailliez
#Lehmenn 2019 and Hopkins 2016 papers downloaded to Emily's computer 


###### Testing the effect of transect and site ######
#note that adonis2 and capscale give slightly different results, so just pick one or the other, using add="lingoes" or "cailliez" to deal with negative eigenvalues changes the results a bit too. if you do lingoes or cailliez capscale and adonis2 give the same result
#I like capscale better than adonis2 b/c capscale only uses positive eigenvalues and just ignores negative ones. whereas adonis2 addes the negative ones into the positive variances which is weird.
#to test the effect of transect and site you freely permute plots as chunks and reassign them different transect and site IDs

h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envTYb$Plot, type = "free"),
         nperm=999)
dbrdaTYb <- capscale(speTYd ~ Site+Transect+SiteTransect+Plot, distance="bray",add="lingoes",data = envTYb)#
anova(dbrdaTYb, permutations = h,by="terms") #model="reduced" permutes the residuals of the model after Condition() is applied 
#adonis2(speTYd~Transect+Site+SiteTransect+Plot, method="bray",permutations = h,by="terms",data=envTYb)#add="lingoes",
adonis2(speTYd~Site+Transect+SiteTransect+Plot, method="bray",permutations = h,by="terms",data=envTYb)#add="lingoes", #since transect and site are almost completely independent, the F values don't change much depending on their order, so I will just do one for now

#how do ordinations code the interactive effect?
#the following are the same:
#adonis2(speTYd~SiteTransect, method="bray",by="terms",data=envTYb)#add="lingoes",
#adonis2(speTYd~Site*Transect, method="bray",by="terms",data=envTYb)#add="lingoes",

#these work. you can use Transect+Site+SiteTransect as interchangeable for Transect+Site+Transect:Site
#adonis2(speTYd~Transect+Site+Transect:Site, method="bray",permutations = h,by="terms",data=envTYb)
#adonis2(speTYd~Transect+Site+SiteTransect+Plot, method="bray",by="terms",data=envTYb)

#see page 13 in https://cran.r-hub.io/web/packages/permute/vignettes/permutations.pdf
#also see https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#the anova function calculates the F value= variance/Df of the factor divided by the residual variance/df
#but this is not correct b/c we want to use not the residual variance but rather the unexplained variation among plots as the denominator, so do the following
#anova raw bray
(18.9544/3)/(13.5525/71)=33.09998#Site
(5.6159/2)/(13.5525/71)=14.71053#Transect
(3.4130/6)/(13.5525/71)=2.980053#SiteTransect
#anova lingoes
(23.497/3)/(123.426/71)=4.505499#Site
(8.609/2)/(123.426/71)=2.476135#Transect
(12.100/6)/(123.426/71)=1.160074#SiteTransect
#adonis2: 
(18.641/3)/(8.499/71)=51.9085#Site
(5.372/2)/(8.499/71)=22.43864#Transect
(2.388/6)/(8.499/71)=3.324862#SiteTransect

#adonis: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envTYb),shuffleSet(n = nrow(envTYb), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 6)
colnames(results) <- c("Site","Transect","SiteTransect","Plot","Residual","Total")
for (i in 1:nrow(perms)) {
  temp.data <- envTYb[perms[i, ], ]
  temp <- adonis2(speTYd ~ Site+Transect+SiteTransect + Plot,
                  data = temp.data,
                  method = "bray",
                  permutations = 0)
  results[i, ] <- t(temp$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Site=(Site/3)/(Plot/71),F.Transect = (Transect/2)/(Plot/71),F.SiteTransect=(SiteTransect/6)/(Plot/71))

#make sure there are no duplicates of the actual data. this should equal 1. the first row of the perm data is the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: 
with(results, sum(F.Site >= F.Site[1]) / length(F.Site))
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))
with(results, sum(F.SiteTransect >= F.SiteTransect[1]) / length(F.SiteTransect))

#capscale: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envTYb),shuffleSet(n = nrow(envTYb), control = h, nset = 9999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 5)
colnames(results) <- c("Site","Transect","SiteTransect","Plot","Residual")
for (i in 1:nrow(perms)) {
  temp.data <- envTYb[perms[i, ], ]
  temp<-capscale(speTYd ~ Site+Transect +SiteTransect+ Plot, data = temp.data, distance = "bray",add="lingoes")
  temp2<-anova(temp,permutations = 0,by="terms")
  results[i, ] <- t(temp2$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Site=(Site/3)/(Plot/71),F.Transect = (Transect/2)/(Plot/71),F.SiteTransect=(SiteTransect/6)/(Plot/71))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: 
with(results, sum(F.Site >= F.Site[1]) / length(F.Site))
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))
with(results, sum(F.SiteTransect >= F.SiteTransect[1]) / length(F.SiteTransect))


###### Effect of year ######
#these how() statements do the same thing
# h2 <- how(within = Within(type = "free"),
#               plots = Plots(type = "none"),
#               blocks = envBP$Plot,
#               nperm = 999,
#               observed = TRUE)
h2 <- how(within = Within(type = "free"),
          plots = Plots(strata = envTYb$Plot, type = "none"),
          nperm = 9999)
check(envTYb, control = h2)
#ignore the p value in the Plot row, it is a duplicate of the P value in the Yearfac row, this is incorrect says Gavin Simpson. The the effect of Year is F=7.98, P=0.001
adonis2(speTYd ~ Plot + Yearfac, data = envTYb,method = "bray",permutations = h2)
dbrdaTYb <- capscale(speTYd ~ Plot+Yearfac, distance="bray",add="lingoes", data = envTYb)
anova(dbrdaTYb, permutations = h2,by="terms")



###### The interaction term ######
#using Gavin Simpson's pdf(s), he only tests the effect of year and interactions together. that is not what I want. 
#it is the same results to condition the model on plot or just put plot first and do sequential anova
#the interaction term is tested against the "usual" residual degrees of freedom b/c it is manipulated at the split plot level (not the whole plot level)
# dbrdaBPb <- capscale(speBPb ~ Yearfac + Yearfac:Transect + Condition(Plot), distance="bray", data = envBP)
#from first pdf
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBP$Plot, type = "free"),
#          nperm=999)
#from second pdf
# h <- how(within = Within(type = "free"),
#        plots = Plots(strata = envBP$Plot, type = "none"),
#        nperm=999)
# anova(dbrdaBPb, permutations = h, model = "reduced")

#testing out permutations
# envBPx<-envBP%>%unite("Plot.Year",c(Plot,Yearfac),remove=F)%>%arrange(Plot)
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBPx$Plot, type = "free"))
# h <- how(within = Within(type = "free"),
#          plots = Plots(strata = envBPx$Plot, type = "none"))
# envBPx[shuffle(nrow(envBPx), control = h), c("Plot.Year","Plot", "Yearfac", "Transect")]

#alternate method from this website to get only interactive effect
#conditions on plat and year and tests for the interaction only, uses the plots strata=plot, type=free formulation. (link and permalink below)
#https://github.com/naupaka/esa_vegan/blob/master/03-constrained-ordination/constrained-ordination.md
#https://github.com/naupaka/esa_vegan/blob/8276570b1ab1c4027bfaf1a263658582d0d81099/03-constrained-ordination/constrained-ordination.md
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envTYb$Plot, type = "free"),
         nperm=9999)
dbrdaTYb <- capscale(speTYd ~ Yearfac:Site + Yearfac:Transect + Yearfac:SiteTransect + Condition(Plot+Yearfac), distance="bray", data = envTYb,add="lingoes")
anova(dbrdaTYb, permutations = h,model="reduced",by="terms")

#Notes on corrections for negative eigenvalues: 
#adding lingoes changed the F stats but did not change the significance at all. not sure if this is general (the permutations would be similar with our without lingoes even though the variance explained is very small with lingoes) or if it is due to the super significant data we have
#using lingoes the constant that is added is huge 1.6 in the below model, using cailliez it is 1.5. the Legendre and Anderson paper say that 0.8 is large. These are twice that much. However the Legendre and Anderson paper say unequivocally to use lingoes (rather than Cailliez or sqrt Bray-curtis) as it more or less preserves distances and perfoms best in permutation tests (the other ways are less conservative and would give you more significance that they should)
dbrdaTYb <- capscale(speTYd ~ Yearfac*Site*Transect, distance="bray", data = envTYb,add="lingoes")
# 166 plots, 45 species; 4:1
#https://vegandevs.github.io/vegan/reference/varpart.html says that the lingoes and cailliez corrections essentialy just add unexplained variance to the models, so that is weird when the whole ordination permutations are dealing with residual variance, but since it's permutation based it doesnt really matter.


#upshot - the two ways of doing the how() format are confusing. They were both used in gavin simpson's powerpoints for determination of the full year+year:mowing+year:fertilizer+year:removal interactions for testing the significance of the full model. and they yield the same result for this (though it might be b/c it is so significant). however two things are complicating things: 1) using year as linear vs factor makes a difference in whether the second h below can be used with a model conditioning on year and plotid and 2) the two h's below give different results when you do the significance by terms. The first h is the one that Naupaka and Gavin used in their 2014 github page where they conditioned on year and plotID and tested sig by model with only the interactive effects there. So I should probably use this. the only thing I can think of is that the first h preserves the temporal correlations among years so it may be somewhat more conservative than when you randomize years freely. OR it could be the error that I mentioned above for how it duplicates p values incorrectly. However, it does make sense that they both are ways of testing the interaction - one is holding year constant and mixing up treatment, the other is holding treatments constant but mixing up years. 





##### Barataria all years #####
# Going through the tutorial in https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#this is downloaded as "Applied Multivariate Statistics in R" book

#check of permutations, this screws with everything, need to reload envBP
# envBP<-envBP%>%unite("Plot.Year",c(Plot,Yearfac),remove=F)%>%arrange(Plot)
# (h <- how(within = Within(type = "none"), plots = Plots(strata = envBP$Plot, type = "free"),nperm=999))
# envBP[shuffle(nrow(envBP), control = h), c("Plot.Year","Plot", "Yearfac", "Transect")]
# check(envBP,h)

###### Testing the effect of transect ######
#note that adonis2 and capscale give slightly different results, so just pick one or the other, using add="lingoes" or "cailliez" to deal with negative eigenvalues changes the results a bit too. if you do lingoes or cailliez capscale and adonis2 give the same result
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBP$Plot, type = "free"),
         nperm=999)
dbrdaBPb <- capscale(speBPc ~ Transect+Plot, distance="bray",data = envBP)#add="lingoes",
anova(dbrdaBPb, permutations = h,by="terms") #model="reduced" permutes the residuals of the model after Condition() is applied 
adonis2(speBPc~Transect+Plot, method="bray",permutations = h,by="terms",data=envBP)#add="lingoes",
#see page 13 in https://cran.r-hub.io/web/packages/permute/vignettes/permutations.pdf
#also see https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#the anova function calculates the F value= variance/Df of the factor divided by the residual variance/df
(2.5158/2)/(8.3709/105)=15.78 #anova
#but this is not correct b/c we want to use not the residual variance but rather the unexplained variation among plots as the denominator, so do the following
(2.5158/2)/(4.5640/18)=4.961 #anova
(2.4069/2)/(3.7220/18)=5.820#adonis2

#adonis: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBP),shuffleSet(n = nrow(envBP), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 4)
colnames(results) <- c("Transect", "Plot", "Residual","Total")
for (i in 1:nrow(perms)) {
  temp.data <- envBP[perms[i, ], ]
  temp <- adonis2(speBPc ~ Transect + Plot,
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

#calculate p value: F=5.82, P=0.001
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))

#capscale: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBP),shuffleSet(n = nrow(envBP), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 3)
colnames(results) <- c("Transect", "Plot", "Residual")
for (i in 1:nrow(perms)) {
  temp.data <- envBP[perms[i, ], ]
  temp<-capscale(speBPc ~ Transect + Plot, data = temp.data, distance = "bray")
  temp2<-anova(temp,permutations = 0,by="terms")
  results[i, ] <- t(temp2$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Transect = (Transect/2)/(Plot/18))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: F=4.96, P=0.001
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))

###### Effect of year ######
#these how() statements do the same thing
# h2 <- how(within = Within(type = "free"),
#               plots = Plots(type = "none"),
#               blocks = envBP$Plot,
#               nperm = 999,
#               observed = TRUE)
h2 <- how(within = Within(type = "free"),
          plots = Plots(strata = envBP$Plot, type = "none"),
          nperm = 999)
check(envBP, control = h2)
#ignore the p value in the Plot row, it is a duplicate of the P value in the Yearfac row, this is incorrect says Gavin Simpson. The the effect of Year is F=7.98, P=0.001
adonis2(speBPc ~ Plot + Yearfac, data = envBP,method = "bray",permutations = h2)
dbrdaBPb <- capscale(speBPc ~ Plot+Yearfac, distance="bray", data = envBP)
anova(dbrdaBPb, permutations = h2,by="terms")



###### The interaction term ######
#using Gavin Simpson's pdf(s), he only tests the effect of year and interactions together. that is not what I want. 
#it is the same results to condition the model on plot or just put plot first and do sequential anova
#the interaction term is tested against the "usual" residual degrees of freedom b/c it is manipulated at the split plot level (not the whole plot level)
# dbrdaBPb <- capscale(speBPb ~ Yearfac + Yearfac:Transect + Condition(Plot), distance="bray", data = envBP)
#from first pdf
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBP$Plot, type = "free"),
#          nperm=999)
#from second pdf
# h <- how(within = Within(type = "free"),
#        plots = Plots(strata = envBP$Plot, type = "none"),
#        nperm=999)
# anova(dbrdaBPb, permutations = h, model = "reduced")

#testing out permutations
# envBPx<-envBP%>%unite("Plot.Year",c(Plot,Yearfac),remove=F)%>%arrange(Plot)
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBPx$Plot, type = "free"))
# h <- how(within = Within(type = "free"),
#          plots = Plots(strata = envBPx$Plot, type = "none"))
# envBPx[shuffle(nrow(envBPx), control = h), c("Plot.Year","Plot", "Yearfac", "Transect")]

#alternate method from this website to get only interactive effect
#conditions on plat and year and tests for the interaction only, uses the plots strata=plot, type=free formulation. (link and permalink below)
#https://github.com/naupaka/esa_vegan/blob/master/03-constrained-ordination/constrained-ordination.md
#https://github.com/naupaka/esa_vegan/blob/8276570b1ab1c4027bfaf1a263658582d0d81099/03-constrained-ordination/constrained-ordination.md
dbrdaBPb <- capscale(speBPc ~ Yearfac:Transect + Condition (Plot+ Yearfac), distance="bray", data = envBP)
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBP$Plot, type = "free"),
         nperm=999)
anova(dbrdaBPb, permutations = h,model="reduced",by="terms")

#upshot - the two ways of doing the how() format are confusing. They were both used in gavin simpson's powerpoints for determination of the full year+year:mowing+year:fertilizer+year:removal interactions for testing the significance of the full model. and they yield the same result for this (though it might be b/c it is so significant). however two things are complicating things: 1) using year as linear vs factor makes a difference in whether the second h below can be used with a model conditioning on year and plotid and 2) the two h's below give different results when you do the significance by terms. The first h is the one that Naupaka and Gavin used in their 2014 github page where they conditioned on year and plotID and tested sig by model with only the interactive effects there. So I should probably use this. the only thing I can think of is that the first h preserves the temporal correlations among years so it may be somewhat more conservative than when you randomize years freely. OR it could be the error that I mentioned above for how it duplicates p values incorrectly. However, it does make sense that they both are ways of testing the interaction - one is holding year constant and mixing up treatment, the other is holding treatments constant but mixing up years. 





##### Bayou Sauvage all years #####

#Trying just 2017 and 2022
datBS<-dat2%>%
  filter(Site=="Bayou Sauvage"&Year%in%c(2017,2022))%>% #
  filter(!is.na(Phragmites.australis))%>%
  filter(NatAbun+Phragmites.australis>0)
speBS<-datBS%>%
  select(Phragmites.australis:Vigna.luteola)
#Take out Species that didn't exist in Bayou Sauvage
speBSb<-speBS[colSums(speBS>0) > 0]
envBS<-datBS%>%
  select(Year:DeadPhragStems,Yearfac)
speBSc<-decostand(speBSb,method="log",logbase=10)

ind<-which(envBS$Plot%in%c(107,108,109,110,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127)) #these are all plots that are present in 2017 and 2022. this seems to work for all the analyses below
#ind<-which(envBS$Plot%in%c(107,108,109,110,112,113,114,115,116,117,119,120,121,122,123,124,126,127)) #these are all plots that are present in 2017 and 2022 and making it balanced, so deleting a plot in T and N. I deleted the plots in the same line as 111 (118 and 152)
speBSd<-speBSc[ind,]
envBSb<-envBS[ind,]


###### Testing the effect of transect ######
#note that adonis2 and capscale give slightly different results, so just pick one or the other, using add="lingoes" or "cailliez" to deal with negative eigenvalues changes the results a bit too. if you do lingoes or cailliez capscale and adonis2 give the same result
#to test the effect of transect you freely permute plots as chunks and reassign them different transect IDs
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBSb$Plot, type = "free"),
         nperm=999)
dbrdaBSb <- capscale(speBSd ~ Transect+Plot, distance="bray",data = envBSb)#add="lingoes",
anova(dbrdaBSb, permutations = h,by="terms") #model="reduced" permutes the residuals of the model after Condition() is applied 
adonis2(speBSd~Transect+Plot, method="bray",permutations = h,by="terms",data=envBSb)#add="lingoes",
#see page 13 in https://cran.r-hub.io/web/packages/permute/vignettes/permutations.pdf
#also see https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/restricting-permutations/#Analysis%20of%20a%20RCB%20Design
#the anova function calculates the F value= variance/Df of the factor divided by the residual variance/df
(3.3806/2)/(5.2418/20)=6.449311 #anova
#but this is not correct b/c we want to use not the residual variance but rather the unexplained variation among plots as the denominator, so do the following
(3.3806/2)/(2.8614/17)=10.04232 #anova
(3.2306/2)/(2.2229/17)=12.35328#adonis2

#adonis: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBSb),shuffleSet(n = nrow(envBSb), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 4)
colnames(results) <- c("Transect", "Plot", "Residual","Total")
for (i in 1:nrow(perms)) {
  temp.data <- envBSb[perms[i, ], ]
  temp <- adonis2(speBSd ~ Transect + Plot,
                  data = temp.data,
                  method = "bray",
                  permutations = 0)
  results[i, ] <- t(temp$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Transect = (Transect/2)/(Plot/17))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3&perms[,4]==4)

#calculate p value: P=0.001
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))

#capscale: still need to do permutation test by hand to get real significance
perms <- rbind(1:nrow(envBSb),shuffleSet(n = nrow(envBSb), control = h, nset = 999))
head(perms)
results <- matrix(nrow = nrow(perms), ncol = 3)
colnames(results) <- c("Transect", "Plot", "Residual")
for (i in 1:nrow(perms)) {
  temp.data <- envBSb[perms[i, ], ]
  temp<-capscale(speBSd ~ Transect + Plot, data = temp.data, distance = "bray")
  temp2<-anova(temp,permutations = 0,by="terms")
  results[i, ] <- t(temp2$SumOfSqs)
}
results <- results %>%
  data.frame() %>%
  mutate(F.Transect = (Transect/2)/(Plot/17))

#make sure there are no duplicates of the actual data
which(perms[,1]==1&perms[,2]==2&perms[,3]==3)

#calculate p value: P=0.001
with(results, sum(F.Transect >= F.Transect[1]) / length(F.Transect))


###### Effect of year ######

#to test the effect of year you freely permute years within each plot. but in the anova you need to take out the effect of plot so that that variance is not included in the residual (to calculate F)

#these how() statements do the same thing
# h2 <- how(within = Within(type = "free"),
#               plots = Plots(type = "none"),
#               blocks = envBP$Plot,
#               nperm = 999,
#               observed = TRUE)
h2 <- how(within = Within(type = "free"),
          plots = Plots(strata = envBSb$Plot, type = "none"),
          nperm = 999)
check(envBSb, control = h2)
#ignore the p value in the Plot row, it is a duplicate of the P value in the Yearfac row, this is incorrect says Gavin Simpson. The the effect of Year is F=, P=0.001
adonis2(speBSd ~ Plot + Yearfac, data = envBSb,method = "bray",permutations = h2)
dbrdaBSb <- capscale(speBSd ~ Plot+Yearfac, distance="bray", data = envBSb)
anova(dbrdaBSb, permutations = h2,by="terms")
plot(dbrdaBSb)


###### The interaction term ######
#using Gavin Simpson's pdf(s), he only tests the effect of year and interactions together. that is not what I want. 
#it is the same results to condition the model on plot or just put plot first and do sequential anova
#the interaction term is tested against the "usual" residual degrees of freedom b/c it is manipulated at the split plot level (not the whole plot level)
# dbrdaBPb <- capscale(speBPb ~ Yearfac + Yearfac:Transect + Condition(Plot), distance="bray", data = envBP)
#from first pdf
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBP$Plot, type = "free"),
#          nperm=999)
#from second pdf
# h <- how(within = Within(type = "free"),
#        plots = Plots(strata = envBP$Plot, type = "none"),
#        nperm=999)
# anova(dbrdaBPb, permutations = h, model = "reduced")

#testing out permutations
# envBPx<-envBP%>%unite("Plot.Year",c(Plot,Yearfac),remove=F)%>%arrange(Plot)
# h <- how(within = Within(type = "none"), 
#          plots = Plots(strata = envBPx$Plot, type = "free"))
# h <- how(within = Within(type = "free"),
#          plots = Plots(strata = envBPx$Plot, type = "none"))
# envBPx[shuffle(nrow(envBPx), control = h), c("Plot.Year","Plot", "Yearfac", "Transect")]

#alternate method from this website to get only interactive effect
#conditions on plat and year and tests for the interaction only, uses the plots strata=plot, type=free formulation. (link and permalink below)
#https://github.com/naupaka/esa_vegan/blob/master/03-constrained-ordination/constrained-ordination.md
#https://github.com/naupaka/esa_vegan/blob/8276570b1ab1c4027bfaf1a263658582d0d81099/03-constrained-ordination/constrained-ordination.md

#To test the interaction term you permute plots as chunks and keep year constant. so the test hold years constant and mixes up transect ID.

dbrdaBSb <- capscale(speBSd ~ Yearfac:Transect + Condition (Plot+ Yearfac), distance="bray", data = envBSb)
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBSb$Plot, type = "free"),
         nperm=999)
anova(dbrdaBSb, permutations = h,model="reduced",by="terms")  #model="reduced" permutes the residuals of the model after Condition() is applied 

#upshot - the two ways of doing the how() format are confusing. They were both used in gavin simpson's powerpoints for determination of the full year+year:mowing+year:fertilizer+year:removal interactions for testing the significance of the full model. and they yield the same result for this (though it might be b/c it is so significant). however two things are complicating things: 1) using year as linear vs factor makes a difference in whether the second h below can be used with a model conditioning on year and plotid and 2) the two h's below give different results when you do the significance by terms. The first h is the one that Naupaka and Gavin used in their 2014 github page where they conditioned on year and plotID and tested sig by model with only the interactive effects there. So I should probably use this. the only thing I can think of is that the first h preserves the temporal correlations among years so it may be somewhat more conservative than when you randomize years freely. OR it could be the error that I mentioned above for how it duplicates p values incorrectly. However, it does make sense that they both are ways of testing the interaction - one is holding year constant and mixing up treatment, the other is holding treatments constant but mixing up years. 


