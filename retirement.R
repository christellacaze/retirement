########################################################################
# using the RLD from RAJAR Q2 2014, can we establish the drivers for radio hours
# and more particularly explore the variable "retired" (controlled for age)
install.packages("openxlsx")
library(openxlsx)
codebook<-read.xlsx("rajar q2 2014 RLD.xlsx", sheet=4)
mydf <- read.xlsx("rajar q2 2014 RLD.xlsx", sheet = 3, startRow = 1, colNames = TRUE)

mydf<-mydf[-grep("remove", names(mydf))] # to remove the redundant columns with name="remove"
codebook<-codebook[-grep("remove", codebook$name),]

str(mydf)
names(mydf)

# create the station reach binaries
mydf$allradioR<-rep(NA,nrow(mydf))
mydf$allradioR[mydf$allradioH!=0]<-1
mydf$allradioR[mydf$allradioH==0]<-0
sum(mydf$allradioR==1)/nrow(mydf)

mydf$allbbcR<-rep(NA,nrow(mydf))
mydf$allbbcR[mydf$allbbcH!=0]<-1
mydf$allbbcR[mydf$allbbcH==0]<-0
sum(mydf$allbbcR==1)/nrow(mydf)

mydf$allCRR<-rep(NA,nrow(mydf))
mydf$allCRR[mydf$allCRH!=0]<-1
mydf$allCRR[mydf$allCRH==0]<-0
sum(mydf$allCRR==1)/nrow(mydf)

mydf$radio1R<-rep(NA,nrow(mydf))
mydf$radio1R[mydf$radio1H!=0]<-1
mydf$radio1R[mydf$radio1H==0]<-0
sum(mydf$radio1R==1)/nrow(mydf)

mydf$radio2R<-rep(NA,nrow(mydf))
mydf$radio2R[mydf$radio2H!=0]<-1
mydf$radio2R[mydf$radio2H==0]<-0
sum(mydf$radio2R==1)/nrow(mydf)

mydf$radio3R<-rep(NA,nrow(mydf))
mydf$radio3R[mydf$radio3H!=0]<-1
mydf$radio3R[mydf$radio3H==0]<-0
sum(mydf$radio3R==1)/nrow(mydf)

mydf$radio4R<-rep(NA,nrow(mydf))
mydf$radio4R[mydf$radio4H!=0]<-1
mydf$radio4R[mydf$radio4H==0]<-0
sum(mydf$radio4R==1)/nrow(mydf)

mydf$radio5R<-rep(NA,nrow(mydf))
mydf$radio5R[mydf$radio5H!=0]<-1
mydf$radio5R[mydf$radio5H==0]<-0
sum(mydf$radio5R==1)/nrow(mydf)


# create the "retired" binary
mydf$retired<-rep(NA,nrow(mydf))
mydf$retired[mydf$work==6]<-1
mydf$retired[mydf$work!=6]<-0
sum(mydf$retired)
nrow(mydf[mydf$work==6,])

summary(factor(mydf$ethnicNS))
names(mydf)
mydf<-mydf[-32]

#############################
# DATA EXPLORATION
#############################

# distribution of weights
hist(mydf$weight, col="red", main="histogram of survey weights",
     breaks=seq(0,ceiling(max(mydf$weight)),by=0.5))

# all radio hours
hist(mydf$allradioH, breaks=seq(0,160,by=1), main="histogram for all radio hours")
hist(mydf$allradioH[mydf$allradioH!=0], breaks=seq(0,160,by=1), 
     main="histogram for non-null radio hours")

# reach, HpH, HpL for all adults
nrow(mydf[mydf$allradioH!=0,])/nrow(mydf) #91.1% unw reach
sum(mydf$allradioH)/nrow(mydf) # 20 HpH
sum(mydf$allradioH)/nrow(mydf[mydf$allradioH!=0,]) # 21.9 HpL

# reach, HpH, HpL for all retired:
nrow(mydf[mydf$allradioH!=0 & mydf$retired==1,])/nrow(mydf[mydf$retired==1,]) #89.27 unw reach
sum(mydf$allradioH[mydf$retired==1])/nrow(mydf[mydf$retired==1,]) # 22.1 HpH
sum(mydf$allradioH[mydf$retired==1])/nrow(mydf[mydf$allradioH!=0 & mydf$retired==1,]) # 24.7 HpL

# histogram for individual age:
summary(mydf$age) # min is 6, that's odd...
summary(factor(mydf$agex7)) # category one is empty - that's 10-14
  # so it looks like our indiv age should start at 15
15-6 # so we should add 9 to indiv age
mydf$age<-mydf$age+9
hist(mydf$age, breaks=seq(0,100,by=1))
plot(factor(mydf$agex7))
table(factor(mydf$agex7), factor(mydf$retired))
    # so we have 3 retired in 15-24s (very funny...) and 4 in 35-44s
plot(factor(mydf$retired)~factor(mydf$agex7), main="proportion of retired by age group",
     xlab="age group", ylab="retired")

# scatterplot radio hours by age
plot(mydf$allradioH~mydf$age, pch=20, cex=0.6, main="all radio hours by respondent's age")
colramp = colorRampPalette(c('white', 'blue', 'green', 'yellow', 'red'))
smoothScatter(mydf$age, mydf$allradioH, colramp=colramp)

plot((mydf$allradioH)^(1/4)~mydf$age, pch=20, cex=0.6,
     main="qdrt of all radio hours by respondent's age")
smoothScatter(mydf$age, mydf$allradioH^(1/4), colramp=colramp)
with(mydf[mydf$allradioH!=0,],
     smoothScatter(age, allradioH^(1/4), colramp=colramp))

summary(lm(allradioH~age, data=mydf)) 
  # age only explains 2% of the variablility in radio hours
summary(lm(allradioH~age, data=mydf[mydf$allradioH!=0,]))
summary(lm(allradioH^(1/4)~age, data=mydf)) 
summary(lm(allradioH^(1/4)~age, data=mydf[mydf$allradioH!=0,])) 
summary(lm(allradioH~age^4, data=mydf[mydf$allradioH!=0,])) 


# histograms and densities for all radio hours

plot(density(mydf$allradioH[mydf$allradioH!=0]), col="red", lwd=2,
     main="density of non-null radio hours")

plot(density(mydf$allradioH[mydf$allradioH!=0 & mydf$retired==0]), col="blue", lwd=2,
     main="density for non-null radio hours by retired")
lines(density(mydf$allradioH[mydf$allradioH!=0 & mydf$retired==1]), col="red", lwd=2)
legend("topright", c("retired", "not retired"), lwd=c(2,2), col=c("red", "blue"))
  # retired vs others look very different 
  # but that's in fact because of age...

with(mydf[mydf$allradioH!=0 & mydf$retired==1,],
     plot(density(allradioH^(1/4)), col="red", lwd=2,
          main="density for qdrt of non-null radio hours by retired"))
with(mydf[mydf$allradioH!=0 & mydf$retired==0,],
     lines(density(allradioH^(1/4)), col="blue", lwd=2))
legend("topright", c("retired", "not retired"), lwd=c(2,2), col=c("red", "blue"))
  # same as before but taking the qdrt of non-null hours

round(summary(factor(mydf$work))/nrow(mydf),3)*100
with(mydf,round(prop.table(table(factor(work), factor(agex7)),2),3)*100)
  # about 30% of 55-64 are retired

plot(density(mydf$allradioH[mydf$allradioH!=0 & mydf$agex7==6 & mydf$retired==0]), 
     col="blue", lwd=2, main="density for non-null radio hours for 55-64s by retired")
lines(density(mydf$allradioH[mydf$allradioH!=0 & mydf$agex7==6 & mydf$retired==1]), 
      col="red", lwd=2)
legend("topright", c("retired", "not retired"), lwd=c(2,2), col=c("red", "blue"))
  # now by looking specifically at the age group 55-64s
  # where we have approx 30% retired, 
  # we see there isn't much difference between retired and others

with(mydf[mydf$allradioH!=0 & mydf$retired==1 & mydf$agex7==6,],
     plot(density(allradioH^(1/4)), col="red", lwd=2,
          main="density for qdrt of non-null radio hours for 55-64s by retired"))
with(mydf[mydf$allradioH!=0 & mydf$retired==0 & mydf$agex7==6,],
     lines(density(allradioH^(1/4)), col="blue", lwd=2))
legend("topright", c("retired", "not retired"), lwd=c(2,2), col=c("red", "blue"))
  # same as above but with qdrt, now the difference is a little more apparent

with(mydf[mydf$allradioH!=0 & mydf$work==6 & mydf$agex7==6,],
     plot(density(allradioH^(1/4)), col="red", lwd=2,
          main="density for qdrt of non-null radio hours for 55-64s by retired vs FT empl."))
with(mydf[mydf$allradioH!=0 & mydf$work==1 & mydf$agex7==6,],
     lines(density(allradioH^(1/4)), col="blue", lwd=2))
legend("topright", c("retired", "full time empl."), lwd=c(2,2), col=c("red", "blue"))
  # same as above but this time showing retired vs full-time employment
  # still no massive difference....

# boxplot radio hours by retired

plot((mydf$allradioH)^(1/4)~factor(mydf$retired),
     main="qdrt of all radio hours by retired")
  # again the slight difference is due to the compounding factor of age

with(mydf[mydf$allradioH!=0,],plot(allradioH^(1/4)~factor(retired), 
                                   main="qdrt of non-null radio hours by retired"))
  # same as above but removing hours=0

with(mydf[mydf$allradioH!=0 & mydf$agex7==6,],
     plot(allradioH^(1/4)~factor(retired),
         main="qdrt of non-null radio hours for 55-64s by retired"))
      # no notable difference between retired / not retired if we only look at 55-64s


# does retired impact on radio reach?
chisq.test(with(mydf, table(factor(allradioR),factor(retired)))) # yes for all adults
chisq.test(with(mydf[mydf$agex7==6,], 
                table(factor(allradioR),factor(retired))))# NO for 55-64s 


# linear regression on retired
summary(lm(mydf$allradioH~factor(mydf$retired)))
  # retired is highly signif but the model's Rsquared is tiny
summary(lm(allradioH~factor(retired), 
           data=mydf[mydf$allradioH!=0,]))
  # it slightly improves if we look at non-null radio hours
summary(lm(allradioH~factor(retired), 
           data=mydf[mydf$allradioH!=0 & mydf$agex7==6,]))
  # but if we restrict ourselves to 55-64s it's no longer signif.
summary(lm(allradioH~factor(retired)+factor(agex7), data=mydf))
  # now we look at age group and retired, which is now signif
  # ... but the benchmark is 15-24 where we have 3 "retired"
  # To change the benchmark to 55-64 we can either set the
  # contrasts in the lm or create a new factor with re-ordered levels
nlevels(factor(mydf$agex7))
summary(lm(allradioH~factor(retired)+
          C(factor(agex7),contr.treatment(6,base=5)), data=mydf))
agebase5564<-factor(mydf$agex7, levels=c(6,2:5,7))
  # which is the same as doing this:
levels(agebase5564)<-c("55-64","15-24","25-34","35-44","45-54","65plus")
summary(agebase5564)
summary(lm(mydf$allradioH~factor(mydf$retired)+agebase5564))

summary(lm(mydf$allradioH~factor(mydf$retired)+agebase5564-1))

summary(lm(allradioH~factor(retired)+factor(agex7), 
           data=mydf[mydf$allradioH!=0,]))

summary(lm(allradioH^(1/4)~factor(retired)+factor(agex7),
           data=mydf[mydf$allradioH!=0,]))

with(mydf[mydf$allradioH!=0,],
     summary(lm(allradioH~factor(work)+factor(agex7))))
with(mydf[mydf$allradioH!=0,],
     summary(lm(allradioH^(1/4)~factor(work)+factor(agex7))))



# now looking for retired vs full time employment
with(mydf[mydf$work==1 | mydf$work==6,],
     summary(lm(allradioH~factor(work)))) 
    # working status is signif if we look at all adults
with(mydf[mydf$work==1 | mydf$work==6,],
     summary(lm(allradioH~factor(work)+factor(agex7))))
with(mydf[mydf$work==1 | mydf$work==6,],
     summary(lm(allradioH~factor(retired):factor(agex7))))

with(mydf[mydf$agex7==6 & (mydf$work==1 | mydf$work==6),],
     summary(lm(allradioH~factor(work)))) 
    # working status not significant if we limit ourselves to 55-64

with(mydf[mydf$agex7==6 & (mydf$work==1 | mydf$work==6),],
     summary(lm(allradioH^(1/4)~factor(work)))) 
    # working status significant if we limit ourselves to 55-64 but look at qdrt(hours)

with(mydf[mydf$allradioH!=0 & mydf$agex7==6 & (mydf$work==1 | mydf$work==6),],
     summary(lm(allradioH^(1/4)~factor(work)))) 
    # working status NOT significant if we limit ourselves to 55-64,
    # look at qdrt(hours) but exclude hours=0



###############################################
# look into the relaimpo package
# http://www.jstatsoft.org/v17/i01/paper

install.packages("relaimpo")
library(relaimpo)
temp.lm<-lm(allradioH~age+factor(work), data=mydf)
summary(temp.lm)

metrics<-calc.relimp(temp.lm, type=c("lmg", "first", "last"))



######################################
# looking for drivers for radio hours
summary(mydf)
str(mydf)
names(mydf)
binaries<-c(3:9,13:20,27:31,42:46,55:63)
names(mydf)[binaries]
names(mydf)[-binaries]
str(mydf[binaries])
summary(mydf[binaries])
mydfcat<-mydf
for (i in binaries){
  mydfcat[[i]]<-factor(mydfcat[[i]])
  levels(mydfcat[[i]])<-c("NO","YES")
  }

summary(factor(mydfcat$hhsize))

summary(factor(mydfcat$radiosets))
mydfcat$radiosets<-mydfcat$radiosets-1

summary(factor(mydfcat$hhtenure))
mydfcat$hhtenure[mydfcat$hhtenure==6]<-NA
mydfcat$hhtenure<-factor(mydfcat$hhtenure)
levels(mydfcat$hhtenure)<-c("owned outright","mortgage", "rented from council", "rented privately",
                            "rent-free")
summary(mydfcat$hhtenure)

summary(factor(mydfcat$selfempl))
mydfcat$selfempl[mydfcat$selfempl==2]<-0 # to get a zero/one variables
mydfcat$selfempl[mydfcat$selfempl==3]<-0 # turns the not-stated into NO
mydfcat$selfempl<-factor(mydfcat$selfempl)
levels(mydfcat$selfempl)<-c("NO", "YES")

summary(factor(mydfcat$gender))
mydfcat$gender[mydfcat$gender==2]<-0
mydfcat$gender<-factor(mydfcat$gender)
names(mydfcat)[22]<-"male"

summary(factor(mydfcat$agex7))
mydfcat$agex7<-factor(mydfcat$agex7)
levels(mydfcat$agex7)<-c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")

summary(factor(mydfcat$agex3))
mydfcat$agex3<-factor(mydfcat$agex3)
levels(mydfcat$agex3)<-c("15-34", "35-54","55+")

summary(factor(mydfcat$work))
mydfcat$work[mydfcat$work==7]<-NA
mydfcat$work<-factor(mydfcat$work)
levels(mydfcat$work)<-c("full-time", "part-time", "education", "looking for work", 
                        "not looking for work", "retired")

summary(factor(mydfcat$cie))
mydfcat$cie[mydfcat$cie==2]<-0
mydfcat$cie[mydfcat$cie==3]<-0
mydfcat$cie<-factor(mydfcat$cie)
levels(mydfcat$cie)<-c("NO", "YES")

summary(factor(mydfcat$shopper))
mydfcat$shopper[mydfcat$shopper==2]<-0
mydfcat$shopper<-factor(mydfcat$shopper)
levels(mydfcat$shopper)<-c("NO", "YES")

summary(factor(mydfcat$tvndays))
mydfcat$tvndays[mydfcat$tvndays==10]<-NA
mydfcat$tvndays[mydf$tvndays==1]<-7
mydfcat$tvndays[mydf$tvndays==2]<-6
mydfcat$tvndays[mydf$tvndays==3]<-5
mydfcat$tvndays[mydf$tvndays==4]<-4
mydfcat$tvndays[mydf$tvndays==5]<-3
mydfcat$tvndays[mydf$tvndays==6]<-2
mydfcat$tvndays[mydf$tvndays==7]<-1
mydfcat$tvndays[mydf$tvndays==8]<-0.5
mydfcat$tvndays[mydf$tvndays==9]<-0
summary(factor(mydfcat$tvndays))

summary(factor(mydfcat$tvhwkday))
mydfcat$tvhwkday[mydf$tvhwkday==10]<-NA
mydfcat$tvhwkday[mydf$tvhwkday==1]<-0
mydfcat$tvhwkday[mydf$tvhwkday==2]<-0.5
mydfcat$tvhwkday[mydf$tvhwkday==3]<-1.5
mydfcat$tvhwkday[mydf$tvhwkday==4]<-3.5
mydfcat$tvhwkday[mydf$tvhwkday==5]<-5.5
mydfcat$tvhwkday[mydf$tvhwkday==6]<-7.5
mydfcat$tvhwkday[mydf$tvhwkday==7]<-9.5
mydfcat$tvhwkday[mydf$tvhwkday==8]<-11.5
mydfcat$tvhwkday[mydf$tvhwkday==9]<-13.5
summary(factor(mydfcat$tvhwkday))

summary(factor(mydfcat$tvhweday))
mydfcat$tvhweday[mydf$tvhweday==10]<-NA
mydfcat$tvhweday[mydf$tvhweday==1]<-0
mydfcat$tvhweday[mydf$tvhweday==2]<-0.5
mydfcat$tvhweday[mydf$tvhweday==3]<-1.5
mydfcat$tvhweday[mydf$tvhweday==4]<-3.5
mydfcat$tvhweday[mydf$tvhweday==5]<-5.5
mydfcat$tvhweday[mydf$tvhweday==6]<-7.5
mydfcat$tvhweday[mydf$tvhweday==7]<-9.5
mydfcat$tvhweday[mydf$tvhweday==8]<-11.5
mydfcat$tvhweday[mydf$tvhweday==9]<-13.5
summary(factor(mydfcat$tvhweday))

table(mydfcat$tvhweday,mydfcat$tvhwkday)
length(complete.cases(mydfcat$tvhweday))
sum(is.na(mydfcat$tvhwkday)==TRUE | is.na(mydfcat$tvhweday)==TRUE)

mydfcat$tvtime<-5*mydfcat$tvhwkday+2*mydfcat$tvhweday
summary(mydfcat$tvtime)

summary(factor(mydfcat$tvweight))
mydfcat$tvweight[mydf$tvweight==7]<-NA
mydfcat$tvweight<-factor(mydfcat$tvweight)
levels(mydfcat$tvweight)<-c("heavy", "medium heavy", "medium",
                            "medium light", "light", "non-viewers")
mydfcat$tvweight<-factor(mydfcat$tvweight, levels(mydfcat$tvweight)[c(6,5,4,3,2,1)])
mydfcat$tvweight<-factor(mydfcat$tvweight, ordered=TRUE)

summary(factor(mydfcat$marital))
mydfcat$marital[mydf$marital==3]<-NA
mydfcat$marital<-factor(mydfcat$marital)
levels(mydfcat$marital)<-c("with partner","other")

summary(factor(mydfcat$intfreq))
mydfcat$intfreq<-factor(mydfcat$intfreq)
levels(mydfcat$intfreq)<-c("everyday", "most days", "once a week",
                           "once a month", "less often", "never")
mydfcat$intfreq<-factor(mydfcat$intfreq, levels(mydfcat$intfreq)[c(6,5,4,3,2,1)], ordered=TRUE)

summary(factor(mydfcat$dabown))
mydfcat$dabown[mydf$dabown==3]<-0
mydfcat$dabown[mydf$dabown==2]<-0
mydfcat$dabown<-factor(mydfcat$dabown)
levels(mydfcat$dabown)=c("NO","YES")

summary(factor(mydfcat$disability))
mydfcat$disability[mydf$disability==3]<-NA
mydfcat$disability[mydf$disability==2]<-0
mydfcat$disability<-factor(mydfcat$disability)
levels(mydfcat$disability)=c("NO","YES")

summary(mydfcat)

####################################################
# trees on all radio hours---------------------------
#####################################################

# REGRESSION TREE:
install.packages("rpart.plot")
library(rpart.plot)
heat.tree <- function(tree, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- tree$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(tree, branch.col=cols, box.col=cols, ...)
}


hours.rpart<-rpart(allradioH~., data=mydfcat[c(2:47,64)], 
                 method="anova", cp=0.05, 
                 control=rpart.control(minsplit=50, minbucket=20))
summary(hours.rpart)
heat.tree(hours.rpart, type=2, extra=101, fallen.leaves=TRUE,
          varlen=0, faclen=0, tweak=1, cex=0.8) 
#this pulls out radiosets, then age.

# same on the qdrt for all radio hours
hours.rpart<-rpart((allradioH)^(1/4)~., data=mydfcat[c(2:47,64)], 
                   method="anova", cp=0.05, 
                   control=rpart.control(minsplit=50, minbucket=20))
summary(hours.rpart)
heat.tree(hours.rpart, type=2, extra=101, fallen.leaves=TRUE,
          varlen=0, faclen=0, tweak=1, cex=0.8) 
#this now pulls out radiosets, then int/pod/mob listeners and DABown

# same but excluding hours=0
hours.rpart<-with(mydfcat[mydfcat$allradioH!=0,],
          rpart((allradioH)^(1/4)~., data=mydfcat[c(2:47,64)], 
                   method="anova", cp=0.05, 
                   control=rpart.control(minsplit=50, minbucket=20)))
summary(hours.rpart)
heat.tree(hours.rpart, type=2, extra=101, fallen.leaves=TRUE,
          varlen=0, faclen=0, tweak=1, cex=0.8) 
#this now pulls out radiosets, then int/pod/mob listeners and DABown



# produce boxplots for all radio by categorical variables
# with significance tests
boxplot(mydfcat$allradioH,mydfcat$dabown)
boxplot((mydfcat$allradioH)^(1/4)~mydfcat[[41]], 
        names=paste0(levels(mydfcat[[41]]), "\n(N=", table(mydfcat[41]),")"),
        col="lightgreen", cex.axis=0.7,
        main=paste0("qdrt all radio hours by ",names(mydfcat[41])))

test<-oneway.test(mydfcat$allradioH~mydfcat[[40]])
oneway.test(mydfcat$allradioH~mydfcat$dabown)
head(mydfcat[[41]])
summary(mydfcat[[41]])
names(mydfcat)
str(test)
test$p.value
testresult<-ifelse(test$p.value<0.05,"SIGNIF", "NOT SIGNIF")



factors<-sapply(mydfcat, is.factor)
factorsindex<-grep(TRUE,factors)


pdf("qdrt radio hours boxplots.pdf", width=10)
for (i in factorsindex[-38]){ # variable 38 is allradioR which creates a problem
  test<-oneway.test((mydfcat$allradioH)^(1/4)~mydfcat[[i]])
  testresult<-ifelse(test$p.value<0.05,"SIGNIF", "NOT SIGNIF")
  boxplot((mydfcat$allradioH)^(1/4)~mydfcat[[i]], 
          cex.axis=0.8,
          col=ifelse(test$p.value<0.05,"orangered","skyblue"),
          names=paste0(levels(mydfcat[[i]]), "\n(N=", table(mydfcat[i]),")"),
          main=paste0("qdrt all radio hours by ",names(mydfcat[i]),
                      " - ",testresult))
}
dev.off()


pdf("qdrt non null radio hours boxplots.pdf", width=10)
for (i in factorsindex[-38]){ # variable 38 is allradioR which creates a problem
  test<-oneway.test((mydfcat$allradioH[mydfcat$allradioH!=0])^(1/4)~mydfcat[[i]][mydfcat$allradioH!=0])
  testresult<-ifelse(test$p.value<0.05,"SIGNIF", "NOT SIGNIF")
  boxplot((mydfcat$allradioH[mydfcat$allradioH!=0])^(1/4)~mydfcat[[i]][mydfcat$allradioH!=0], 
               cex.axis=0.8,
               col=ifelse(test$p.value<0.05,"orangered","skyblue"),
               names=paste0(levels(mydfcat[[i]]), "\n(N=", table(mydfcat[i]),")"),
               main=paste0("qdrt non-null radio hours by ",names(mydfcat[i]),
                           " - ",testresult))
}
dev.off()





#####################################################
# regression on all radio hours----------------------
#####################################################

summary(lm(allradioH~., data=mydfcat))
