require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(psy)
require(MASS)
require(descr)

# clear the environment
rm(list=ls())

########################################################################
###explore##D:\wageningen2\Chapter 6- risk perception\beakal\rabies perception
Dataraw<- read.csv(file.choose(), header = TRUE)
#("Rabiesprotectivebrdatafbinaryplusorbinarywithall2.cvs", header=T)

##########
head(Dataraw)

table(Dataraw$Prevlog)
table(Dataraw$prevord)
table(Dataraw$protection_behaviour_average)


##########
table(Dataraw$Prevlog)
table(Dataraw$prevord)
table(Dataraw$protection_behaviour_average)

############################################
##*Descriptive analysis  
##############################################
require(MASS)
require(descr)
##summarize

table(Dataraw$age_catagory)#, Dataraw$prevord) 
crosstab(Dataraw, row.vars = "age_catagory","genderOFRESPONDANT", col.vars = "prevord", type = "c")
chisq.test(table(Dataraw$age_catagory, Dataraw$prevord))

table(Dataraw$genderOFRESPONDANT)#, Dataraw$prevord) 
crosstab(Dataraw, row.vars = "genderOFRESPONDANT", col.vars = "prevord", type = "c")
chisq.test(table(Dataraw$genderOFRESPONDANT, Dataraw$prevord))


table(Dataraw$Levelofeduofrespondant)#, Dataraw$prevord) 
chisq.test(table(Dataraw$Levelofeduofrespondant, Dataraw$prevord))


table(Dataraw$incomeofthehousehold)#, Dataraw$prevord) 
chisq.test(table(Dataraw$incomeofthehousehold, Dataraw$prevord))

table(Dataraw$mmdvmfamilymembercloserelative)#, Dataraw$prevord)
chisq.test(table(Dataraw$mmdvmfamilymembercloserelative, Dataraw$prevord))

table(Dataraw$bittenbyrabieddog)#, Dataraw$prevord)
chisq.test(table(Dataraw$bittenbyrabieddog, Dataraw$prevord))

table(Dataraw$yourdogvaccinatedthisyear, Dataraw$prevord) 
chisq.test(table(Dataraw$yourdogvaccinatedthisyear, Dataraw$prevord))

##frequencies
attach(Dataraw)
prop.table(table(age_catagory)) 

table(Dataraw$genderOFRESPONDANT)#, Dataraw$prevord) 
table(Dataraw$Levelofeduofrespondant)#, Dataraw$prevord) 
table(Dataraw$incomeofthehousehold)#, Dataraw$prevord) 
table(Dataraw$mmdvmfamilymembercloserelative)#, Dataraw$prevord) 
table(Dataraw$bittenbyrabieddog)#, Dataraw$prevord)
table(Dataraw$yourdogvaccinatedthisyear)#, Dataraw$prevord) 

##cronbach(X)
protectionbr<-subset(Dataraw,select=c(Protectivebehaviour_1, Protectivebehaviour_2,Protectivebehaviour_3, Protectivebehaviour_4))
cronbach(protectionbr)

knowledgetest<-subset(Dataraw,select=c(Knowledge_cause,Knowledge_consequence,Knowledge_identification))
cronbach(knowledgetest)

benefittest<-subset(Dataraw, select=c(Perceived_Benefits_1,Perceived_Benefits_2,Perceived_Benefits_3))
cronbach(benefittest)

barriertest<-subset(Dataraw,select=c(Perceived_Barriers_1_farfromvaccente,Perceived_Barriers_2_cost,
                                     Perceived_Barriers_3_dogtransport,Perceived_Barriers_4_difficultyofhandlingdog,
                                     Perceived_Barriers_6_trustonvac))
cronbach(barriertest)

threattest<-subset(Dataraw,select=c(Perceived_threat_Susceptibility1,Perceived_threat_severity1,
                                    Perceived_threat_Susceptibility2,Perceived_threat_severity2))
cronbach(threattest)


cuetoactiontest<-subset(Dataraw, select=c(Readiness_to_action1,Readiness_to_action2,readiness_to_action3,readiness_to_action4) )
cronbach(cuetoactiontest)

selfefficiecytest<-subset(Dataraw, select=c(Self_efficacy_1,Self_efficacy_2,Self_efficacy_3))
cronbach(selfefficiecytest)

library(plyr)
##mean and sd
min(Dataraw$prevord)


cor(knowledgetest)
cor(benefittest)
cor(barriertest)
cor(threattest)
cor(cuetoactiontest)
cor(selfefficiecytest)
cor(protectionbr)

###alternate correlation tests
forcorelation<-subset(Dataraw,select=c(prevord,Knowledge_sum,Perceived_Benefits_average,
                                       Perceived_Barriers_1_farfromvaccente,Perceived_Barriers_2_cost,
                                       Perceived_Barriers_3_dogtransport,Perceived_Barriers_4_difficultyofhandlingdog,
                                       Perceived_Barriers_6_trustonvac, 
                                       Percieved_threat_ave,readiness_to_actionaverage,self_efficacyaverage))

options(digits=2)
cor(forcorelation)
write.csv(cor(forcorelation), file = "cor.csv")

##################################################################
##ordinal regresion
#################################################################
m <- polr(as.factor(prevord) ~ Knowledge_sum+Perceived_Barriers_1_farfromvaccente + 
            Perceived_Barriers_3_dogtransport + Perceived_Barriers_4_difficultyofhandlingdog
            , data = Dataraw, Hess=TRUE)
library(car)
vif(m)

summary(m)

## store table
(ctable <- coef(summary(m)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m)) # default method gives profiled CIs


## odds ratios
exp(coef(m))

## OR and CI
exp(cbind(OR = coef(m), ci))

confint.default(m)
exp(coef(m), digit=3)
exp(cbind(OR = coef(m), ci))

#deviance
deviance(m)
logLik(m)
AIC(m)

#McFadden, Cox & Snell and Nagelkerke pseudo R2

m0 <- polr(as.factor(prevord) ~ 1, method="logistic", data=Dataraw)
LLf   <- logLik(m)
LL0   <- logLik(m0)

#McFadden pseudo-R2
as.vector(1 - (LLf / LL0))

#Cox & Snell
as.vector(1 - exp((2/249) * (LL0 - LLf)))

##Coefficient tests and overall model test
#Individual coefficient tests
sumOrd   <- summary(m)
(coefOrd <- coef(sumOrd))

library(lmtest)
##Model comparisons - likelihood-ratio tests
m0 <- polr(as.factor(prevord) ~ 1, method="logistic", data=Dataraw)
lrtest(m, m0)

############################################################################
#Classification and regresion tree - CART
#######################################################################
##let us remove dependant variable and use only significant variables in the ordinal regression
library(rpart)
library(rpart.plot)

cleanfinals1<-subset(Dataraw,select=c(prevord,Knowledge_sum,Perceived_Barriers_1_farfromvaccente,
                                      Perceived_Barriers_3_dogtransport))
colnames(cleanfinals1) <- c("Intention","Knowledge","Distance","transporting_dogs"                            )
fit <- rpart(as.factor(Intention)~Knowledge+Distance+transporting_dogs
          , method="class",data=cleanfinals1)


#exclude a column with  variance
par(mfrow=c(1,1))

rpart.plot(fit)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# create attractive postcript plot of tree 
post(fit, file = "D:/wageningen2/Chapter 6- risk perception/beakal/rabies perception/CARTtest.ps", 
     title = "Classification Tree for preventive behaviour ")


# plot tree

library(tree)
tr = tree(fit, data=cleanfinals1)
summary(tr)
plot(tr); text(tr)

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for preventive behaviour")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "D:/wageningen2/Chapter 6- risk perception/beakal/rabies perception/CARTtest2.ps", 
     title = "Classification Tree for preventive behaviour")


