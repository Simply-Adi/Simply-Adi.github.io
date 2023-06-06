# plyr and dplyr conflicts, so don't load them together
library(ggplot2)
library(dplyr)
library(reshape2)
ner<-read.csv("elec.csv")
str(ner)
ner$state<-trimws(ner$state)
ner$state<-as.factor(ner$state)
levels(ner$state)
str(ner)
ner<- mutate_each(ner,funs = list(as.numeric),Er:Ps_percent)
ner<- mutate_each(ner,funs = list(as.factor),Year:Month)
yr<-levels(ner$Year)
levels(ner$Year)
#each year must have 84 entries for full dataset
index<-which(ner$Year==yr[6])
index
length(ner[index,]$Month)
ner[index,]$Year
str(ner)
#Yearly kWh for total NER
mlist=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
levels(ner$Month)<-mlist
ner$Ps_ab<-ner$Pr-ner$Pm
ner$Ps_percent<-100*(ner$Ps_ab/ner$Pr)
ner$Es_ab<-ner$Er-ner$Em
ner$Es_percent<-100*(ner$Es_ab/ner$Er)
#-----------------------------------------------------------
ag<-ner%>%group_by(Year,Month)%>%summarise(ner_MkWh_req=sum(Er),ner_MkWh_met=sum(Em))%>%as.data.frame()
ner_MkWh_pdef<-((ag$ner_MkWh_req-ag$ner_MkWh_met)*100)/ag$ner_MkWh_req #if positive deficit,else negative
ag<-data.frame(ag,ner_MkWh_pdef)
#grouping by year
num_yr=ag$Year
plot_basic<-ggplot(ag,aes(x=Month,y=ner_MkWh_req,color=Year,group=Year))+geom_line(size=1)+theme_bw()
plot_basic
ggplot(ag,aes(x=as.numeric(Year)-1,y=ner_MkWh_req))+geom_point(aes(color=Month),size=2)+theme_bw()+stat_smooth(method="loess")+ylab("Spread of MkWh Consumption")+xlab("Order of year(2014=0)")
#when does the peak kWh occur
lev<-levels(ag$Year)
max<-NULL
min<-NULL
index1<-NULL
index2<-NULL
mini<-NULL
max=vector("double",length(lev))
dmax=vector("double",length(lev))
min=vector("double",length(lev))
dmin=vector("double",length(lev))
index1=vector("integer",nrow(ag))
index2=vector("integer",length(lev))
length(max)
for(i in 1:length(lev))
{ #maxmin of kwh
index1<-which(ag$Year==lev[i])
max[i]<-max(ag[index1,]$ner_MkWh_req)
min[i]<-min(ag[index1,]$ner_MkWh_req)
dmax[i]<-max(ag[index1,]$ner_MkWh_pdef)
dmin[i]<-min(ag[index1,]$ner_MkWh_pdef)
index2[i]<-which.max(ag[index1,]$ner_MkWh_req)
}
max_MkWh<-data.frame(lev,max,index2)
str(max_MkWh)
maxi<-vector("integer",length(lev))
mini=vector("integer",length(lev))
dmaxi<-vector("integer",length(lev))
dmini=vector("integer",length(lev))
for(i in 1:length(lev))
{
  maxi[i]<-which(ag$ner_MkWh_req==max[i])
  mini[i]<-which(ag$ner_MkWh_req==min[i])
  dmaxi[i]<-which(ag$ner_MkWh_pdef==dmax[i])
  dmini[i]<-which(ag$ner_MkWh_pdef==dmin[i])
}
maxi
mini
#NER kWh & deficit plots
plot_basic+geom_point(data=ag[maxi,],aes(x=Month,y=ner_MkWh_req),shape=23,fill="blue", color="darkred", size=3)+geom_point(data=ag[mini,],aes(x=Month,y=ner_MkWh_req),shape=21,fill="red", color="darkred", size=3)+ylab("Electricity Requirement(MkWh)")
ggplot(ag)+geom_line(aes(x=Month,y=ner_MkWh_pdef,color=Year,group=Year),size=1)+geom_point(data=ag[dmaxi,],aes(x=Month,y=ner_MkWh_pdef),shape=23,fill="blue", color="darkred", size=3)+geom_point(data=ag[dmini,],aes(x=Month,y=ner_MkWh_pdef),shape=21,fill="red", color="darkred", size=3)+ylab("Electricity Deficit(MkWh percent)")+theme_bw()
write.csv(ag,"ag.csv")
#state wise analysis
#effect of power deficit
ggplot(ner,aes(x=Ps_percent,y=Em))+geom_point(aes(color=state))+geom_smooth(data=subset(ner,state=="Assam"),method="loess")+geom_smooth(data=subset(ner,state!="Assam"),method="loess")+theme_bw()+ylab("Met MkWh Demand")+xlab("Peak Power Deficit(%)")
ggplot(ner,aes(x=Pr,y=Er))+geom_point(aes(color=state))+geom_smooth(data=subset(ner,state=="Assam"),method="loess")+geom_smooth(data=subset(ner,state!="Assam"),method="loess")+theme_bw()+ylab("Monthly MkWh Requirement(MkWh)")+xlab("Monthly Peak Power Requirement(MW)")
ggplot(data=subset(ner,state!="Assam"),aes(x=as.numeric(Year)-1,y=Ps_percent))+geom_point(aes(color=Month),size=2)+theme_bw()+stat_smooth(method="loess")+ylab("Spread of Power Deficit")+xlab("Order of Year(2014=0)")
ggplot(ner,aes(x=Year,y=Ps_percent))+geom_boxplot(aes(color=state))+theme_bw()+ylab("Spread of Peak Power Deficit(%)")
nas<-subset(ner,state!="Assam")
#control for state
#Em acts like supply, Er is like demand
fit1<-lm(nas$Em~nas$Ps_percent+nas$Er+nas$state)
fit2<-lm(ner$Em~ner$Ps_percent+ner$Er)
fit3<-lm(ner$Em~ner$Ps_percent+ner$Er+ner$state)
summary(fit1)
contrasts(ner$state)
levels(ner$state)
#checking linearity of relationship
plot(fit2,which=1,col=c("blue"))
#normality of errors
plot(fit2,which=2,col=c("blue"))
#equal variance of residuals
plot(fit2,which=3,col=c("blue"))
plot(fit2,which=5,col=c("blue"))
#state wise yearly kWh
#prepare SGDP data-----------------------------
sgdp<-read.csv("SGDP.csv")
sgdp$States.UTs<-trimws(sgdp$States.UTs)
sgdp$States.UTs<-as.factor(sgdp$States.UTs)
sgdp$States.UTs<-as.factor(sgdp$Year.UTs)
tidy_sgdp<-melt(sgdp,id="States.UTs",variable.name ="Year",value.name ="SGDP(constant)",na.rm=FALSE)
str(sgdp)
sum(is.na(tidy_sgdp$`SGDP(constant)`)) #22 missing values
str(tidy_sgdp)
tidy_sgdp$Year<-gsub(".([0-9]+)$","",tidy_sgdp$Year)
tidy_sgdp$Year<-gsub("X","",tidy_sgdp$Year)
ne_sgdp<-subset(tidy_sgdp,tidy_sgdp$States.UTs%in%levels(ner$state))
ne_sgdp<-ne_sgdp%>%rename(state=States.UTs)

is.na(ne_sgdp)
library(tidyverse)
forc<-ne_sgdp%>%spread(key=state,value=`SGDP(constant)`)
write.csv(forc,"forc.csv")
index<-which(is.na(ne_sgdp$`SGDP(constant)`))
index
ne_sgdp[index,]
forc_val=c(17871.12575,262048.0135,19733.65981,18098.42519,38867.3314)
ne_sgdp[index,]$`SGDP(constant)`=forc_val
ggplot(ne_sgdp,aes(x=Year))+geom_line(aes(y=`SGDP(constant)`,color=state,group=state),size=1)+theme_bw()
#-----------------------------------------------------------------
yearly_ner<-ner%>%group_by(Year,state)%>%summarise(state_MkWh_met=sum(Em),state_MkWh_req=sum(Er),Peak_Power_Deficit=max(Ps_percent))%>%as.data.frame()
str(yearly_ner)
library(knitr)
library(broom)
m1<-lm(state_MkWh_met~state_MkWh_req+Peak_Power_Deficit+state,yearly_ner)
write.csv(tidy(m1),"cf.csv")
write.csv(glance(m1),"rsq.csv")
#---------------------------------------------------------------------
ne_sgdp_fil<-subset(ne_sgdp,Year%in%levels(yearly_ner$Year))
mg_data=merge(yearly_ner,ne_sgdp,by=c("state","Year"))
gr<-read.csv("est_gr.csv")
gr
gr<-gr%>%melt(id=c("state","gprev"),variable.name="Year",value.name ="ProjectedPop")
gr$Year<-gsub("X","",gr$Year)
gr$Year<-as.factor(gr$Year)
state_dat<-merge(mg_data,gr,by=c("Year","state"))
str(state_dat)
state_dat$ProjectedPop<-as.numeric(state_dat$ProjectedPop)
pc_gdp=state_dat$`SGDP(constant)`/state_dat$ProjectedPop
state_dat<-cbind(state_dat,pc_gdp)
state_dat$pc_gdp=state_dat$pc_gdp*10^(7)
colnames(mg_data)
ggplot(state_dat)+geom_point(aes(x=`SGDP(constant)`,y=state_MkWh_req,color=state))
#-----------------------------------------------------------
#library(GGally)
#scatmat(state_dat,columns=c(4,8),color="state")
#panel regression
str(state_dat)
#fixed is insignificant,gdp is also insignificant
fe_mod<-lm(state_MkWh_met~state_MkWh_req+Peak_Power_Deficit+pc_gdp,state_dat)
summary(fe_mod)
car::vif(fe_mod)
#HDI
shdi<-read.csv("state_HDI.csv")
ne_shdi<-subset(shdi,Region%in%levels(state_dat$state))
ne_shdi<-melt(ne_shdi,id="Region",variable.name="Year",value=value)
ne_shdi$Year<-gsub("X","",ne_shdi$Year)
ne_shdi$Year<-as.factor(ne_shdi$Year)
ne_shdi$value<-as.numeric(ne_shdi$value)
ne_shdi=ne_shdi%>%rename(HDI=value)
ne_shdi=ne_shdi%>%rename(state=Region)
ggplot(ne_shdi)+geom_line(aes(x=Year,y=HDI,color=state,group=state))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hdforc<-spread(ne_shdi,key="state",value="HDI")
write.csv(hdforc,"hdforc.csv")
hdforcval<-c(0.666311179,0.619389947,0.73146735,0.663047219,0.713112729,0.684386336,0.666139397)
hdforcval=round(hdforcval,digits=3)
hdi<-read.csv("fhd.csv")
hdi<-hdi[,-1]
hdi=melt(hdi,id="Year",variable.name ="state",value=value)
hdi$state=gsub("\\."," ",hdi$state)
library(stringr)
hdi$state=str_squish(hdi$state)
hdi$state=as.factor(hdi$state)
hdi$Year=as.factor(hdi$Year)
hdi=hdi%>%rename(HDI=value)
state_dat=merge(state_dat,hdi,by=c("state","Year"))
class(state_dat$HDI)
fe_mod2<-lm(state_MkWh_met~state_MkWh_req+Peak_Power_Deficit+Year,state_dat)
summary(fe_mod2)
fe_mod3<-lm(state_MkWh_req~HDI+state,state_dat)
summary(fe_mod3)
fe_mod3_cf<-tidy(fe_mod3)
ggplot(state_dat)+geom_point(aes(x=pc_gdp,y=HDI,color=state))+theme_bw()+xlab("Per Capita GDP(Constant 2011-12 INR)")
ggplot(state_dat)+geom_point(aes(x=pc_gdp,y=state_MkWh_req,color=state))+theme_bw()
summary(fe_mod4<-lm(state_MkWh_req~pc_gdp+state,state_dat))
