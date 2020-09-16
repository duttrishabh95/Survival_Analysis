setwd("D:/Logikview/MCA_Final_Project")

library(survminer)
library(rpart)
library(dplyr)
library(rattle)
library(survival)

pmain<-read.csv("machine.csv")

###-------Data Exploration-------###

##Missing Value Check
summary(pmain) #No missing values

##Dependent Variable Summary
summary(pmain$broken)

##Finding % of records where machines is in broken state
pmain %>% filter(broken==1) %>% summarise(n()) #no. of machines broken is 39.47% of total (35522 out of 90000)

##Broken percentage grouped by team & provider
broken<-pmain %>% filter(broken==1) %>% group_by(team,provider) %>% 
  summarise(Broken=n(),BPerc=Broken/sum(pmain$broken)*100) %>% data.frame()

class(broken)

##Total Machines grouped by team & provider
t<-pmain %>% group_by(team,provider) %>% summarise(Total=n()) %>% data.frame()

#Adding column "Total" from table 't' to table 'broken'
broken<- cbind(broken,t[,c("Total")])

colnames(broken)<-c("team","provider","Broken","BPerc","Total")

#finding percentage of broken machines out of total machines grouped by team & provider
broken$PercofTotal<-broken[,"Broken"]/broken[,"Total"]*100

#####-----Survival Analytics-----#####

##Kaplan-Mieier non-parametric analysis

#Creating survival/surv object

kmsurvival<-survfit(formula=Surv(lifetime,broken)~1, data=pmain)
summary(kmsurvival)$table #Summary shows survival probability

#Plotting Surv object

ggsurvplot(kmsurvival)

#summary & plot shows that machines have broken on or after lifetime= 60, that means Pressure at 3 points
#and other variables don't have any effect on machine breakdown till lifetime=60

#Confirming the output from Survival Probability graph
pmain %>% filter(broken==1) %>% summarise(mindu=min(lifetime))

##Survival Prob analysis grouped with team
kmsurvival1<-survfit(formula=Surv(lifetime,broken)~pmain$team, data=pmain)
summary(kmsurvival1)$table #medain lifetime of machines maintained across is pretty much same (around 80 days)
kmsurvival1

ggsurvplot(kmsurvival1)

##Log-rank test for comparing Survival Curves across Teams
##H0(Null-hypothesis)=survival prob is same across teams

surv_diff<-survdiff(Surv(lifetime,broken)~team,data=pmain)
surv_diff
#Since p=0 therefore we reject Null Hypothesis that survival probability is same across teams.

##Survival Probability analysis grouped with providers
kmsurvival2<-survfit(formula=Surv(lifetime,broken)~pmain$provider, data=pmain)
summary(kmsurvival2)$table
kmsurvival2

#median lifetime of machines manufactured by provider 3 has median lifetime =65 months whereas 
#breakdown starts from 60 months only

#median lifetime of machines manufactured by provider 1 has median lifetime =92 months whereas 
#breakdown starts from 60 months only and last breakdown happened at 93 months

ggsurvplot(kmsurvival2)

#Log-Rank test for comparing Survival-curves
#H0(Null-hypothesis)=survival prob is same across providers
surv_diff1<-survdiff(Surv(lifetime,broken)~provider,data=pmain)
surv_diff1
#Since p=0 therefore we reject Null Hypothesis that survival probability is same across providers.

##Survival Probability grouped with Team & Provider

kmsurvival3<-survfit(formula=Surv(lifetime,broken)~pmain$provider+pmain$team, data=pmain)
summary(kmsurvival3)$table
kmsurvival3

ggsurvplot(kmsurvival3)

#####-----Survival Regression Model-----#####

###Predicting Expected remaining lifetime of a machine

##Creating Survival Object
s<-Surv(pmain$lifetime,pmain$broken)

#survreg command is used to create Parametric regression model for survival objects
survregmodel<-survreg(s~pressureInd_1+pressureInd_2+pressureInd_3+team+provider,dist="gaussian",data=pmain)
summary(survregmodel)

pmain$TeamB<-ifelse(pmain$team=="TeamB",1,0)
pmain$TeamC<-ifelse(pmain$team=="TeamC",1,0)
pmain$Provider2<-ifelse(pmain$provider=="Provider2",1,0)
pmain$Provider3<-ifelse(pmain$provider=="Provider3",1,0)
pmain$Provider4<-ifelse(pmain$provider=="Provider4",1,0)

survregmodel_1<-survreg(s~pressureInd_1+pressureInd_2+pressureInd_3+TeamB+TeamC+Provider2+Provider3+Provider4,dist="gaussian",data=pmain)
summary(survregmodel_1)

##Predicting Median Expected Lifetime of machines from survival regression model

#Predicitng using Quantile Regression Model
#p=0.5 and type=quantile gives expected median lifetime

expect<- predict(survregmodel_1,data=pmain,type="quantile",p=0.5)
forecast<-data.frame(expect)

#predciting remaining lifetime of machines

forecast$lifetime<-pmain$lifetime
forecast$broken<-pmain$broken
forecast$remainingLT<-forecast$expect-pmain$lifetime

pmain$remainingLT<-fore
cast$remainingLT

#Machines with remaining lifetime less than 20 months should be arranged at top of dataset so that preventive
#maintenance can be scheduled for them wihtout breakdown.

pmain %>% filter(broken==0 & remainingLT>=0 & remainingLT<=20) %>%
  arrange(remainingLT)

#Team wise no of machines with Remaining Lifetime < 20
pmain %>% filter(broken==0 & remainingLT>=0 & remainingLT<=20) %>%
  group_by(team) %>% summarise(Total=n()) #TeamB has large amount of machines with remainingLt<=20 months

#Provider wise no of machines with Remaining Lifetime < 20
pmain %>% filter(broken==0 & remainingLT>=0 & remainingLT<=20) %>%
  group_by(provider) %>% summarise(Total=n()) #provider3 has large amount of machines with remainingLt<=20 months

#Provider & Team wise no of machines with Remaining Lifetime < 20
pmain %>% filter(broken==0 & remainingLT>=0 & remainingLT<=20) %>%
  group_by(provider,team) %>% summarise(Total=n())

write.csv(pmain,file="Pexitics_Expected_RemainingLifetime.csv")

#####-----SURVIVAL TREE-----#####

datapex2<-pmain

##Creating pressure windows for variables Pressure_Ind1,2,3
##Creating deciles for Pressure1 levels by dividing in 20 quants

#creating deciles for Pressure2 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_1,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[2]

datapex2$Pressure1_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<datapex2$pressureInd_1 & datapex2$pressureInd_1<=oj2[i+1])
  datapex2$Pressure1_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
}

index<-which(datapex2$Pressure1_dumm=='0')
datapex2$Pressure1_dumm[index]<-paste("less than",oj2[1])

unique(datapex2$Pressure1_dumm)

#creating deciles for Pressure2 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_2,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[2]

datapex2$Pressure2_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<datapex2$pressureInd_2 & datapex2$pressureInd_2<=oj2[i+1])
  datapex2$Pressure2_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
  
}

index<-which(datapex2$Pressure2_dumm=='0')
datapex2$Pressure2_dumm[index]<-paste("less than",oj2[1])

unique(datapex2$Pressure2_dumm)

#creating deciles for Pressure3 levels by dividing in 20 quants

oj<-quantile(datapex2$pressureInd_3,p=(1:5)/5)
oj<-round(oj)
oj2<-as.list(oj)
oj2[2]

datapex2$Pressure3_dumm<-'0'

for(i in 1:length(oj2)-1)
{
  index<-which(oj2[i]<datapex2$pressureInd_3 & datapex2$pressureInd_3<=oj2[i+1])
  datapex2$Pressure3_dumm[index]<- paste(oj2[i],"to",oj2[i+1])
  
}

index<-which(datapex2$Pressure3_dumm=='0')
datapex2$Pressure3_dumm[index]<-paste("less than",oj2[1])

unique(datapex2$Pressure3_dumm)

##Creating Survival Tree using rpart with method="exp" (exponential)

#omitting pressure column and only considering windows of pressure to create Tree

stree<-rpart(Surv(lifetime,broken)~provider+team+Pressure1_dumm+Pressure2_dumm+Pressure3_dumm,
             data=datapex2[,-c(1,4,5,6)],method='exp',control=rpart.control(cp=0.001,maxdepth=4))

class(stree)

stree

##Extracting rules from survival tree

rpart.plot::rpart.rules(stree,nn=TRUE)
class(rpart.plot::rpart.rules(stree))

asRules(stree)

##Plotting Survival Tree
rpart.plot::rpart.plot(stree)
      
      #OR

par(mar=rep(0.1,4))
plot(stree,uniform=TRUE,branch=1, compress=TRUE)
text(stree, n=TRUE, cex=0.5)

#Adding terminal to their respective data points in data sets

datapex2$TerminalNode<-stree$where

unique(datapex2$TerminalNode)

##Creating Survival Plot using Kaplan-Mieier Non Parametric Test between Survival Object 
##and Terminal Node Variable

km<-survfit(formula=Surv(lifetime,broken)~datapex2$TerminalNode, data=datapex2[,-c(1,4,5,6)])
km
summary(km)

ggsurvplot(km)