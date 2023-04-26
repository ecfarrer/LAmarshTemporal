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

#Effect of year
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
#ignore the p value in the Plot row, it is a duplicate of the P value in the Yearfac row, this is incorrect says Gavin Simpson. The the effect of Year is F=5.05, P=0.001
adonis2(speBPb ~ Plot + Yearfac, data = envBP,method = "bray",permutations = h2)
dbrdaBPb <- capscale(speBPb ~ Plot+Yearfac, distance="bray", data = envBP)
anova(dbrdaBPb, permutations = h2,by="terms")



#The interaction term: 
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
dbrdaBPb <- capscale(speBPb ~ Yearfac:Transect + Condition (Plot+ Yearfac), distance="bray", data = envBP)
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = envBP$Plot, type = "free"),
         nperm=999)
anova(dbrdaBPb, permutations = h,model="reduced",by="terms")

#upshot - the two ways of doing the how() format are confusing. They were both used in gavin simpson's powerpoints for determination of the full year+year:mowing+year:fertilizer+year:removal interactions for testing the significance of the full model. and they yield the same result for this (though it might be b/c it is so significant). however two things are complicating things: 1) using year as linear vs factor makes a difference in whether the second h below can be used with a model conditioning on year and plotid and 2) the two h's below give different results when you do the significance by terms. The first h is the one that Naupaka and Gavin used in their 2014 github page where they conditioned on year and plotID and tested sig by model with only the interactive effects there. So I should probably use this. the only thing I can think of is that the first h preserves the temporal correlations among years so it may be somewhat more conservative than when you randomize years freely. OR it could be the error that I mentioned above for how it duplicates p values incorrectly. However, it does make sense that they both are ways of testing the interaction - one is holding year constant and mixing up treatment, the other is holding treatments constant but mixing up years. 

#practice data from Gavin Simpson's files
ohspp<-read.csv("/Users/farrer/Downloads/GavinSimpsonRestrictedPermutations/ohraz-spp.csv",row.names = 1)
ohspp<-ohspp[,-1]
ohenv<-read.csv("/Users/farrer/Downloads/GavinSimpsonRestrictedPermutations/ohraz-env.csv",row.names = 1)
ohenv$yearfac<-factor(ohenv$year)
dbrdatest <- capscale(ohspp ~ year+year:removal+year:mowing+year:fertilizer+ Condition (plotid), distance="bray", data = ohenv)#,add="lingoes"
h <- how(within = Within(type = "none"), 
         plots = Plots(strata = ohenv$plotid, type = "free"))
h <- how(within = Within(type = "free"),
         plots = Plots(strata = ohenv$plotid, type = "none"))
anova(dbrdatest, permutations = h,model="reduced",by="terms")
anova(dbrdatest, permutations = h,model="reduced")
check(ohenv, control = h)

