#import month return rates, delete the stock whose trading days is below 1 day, delete the N/A value
Drtn<-read.table("monthly return.txt",skip=1,sep="\t",colClass=c("character","character","numeric","numeric","numeric"))
names(Drtn)<-c("stk","month","mv","trday","rtn")
Drtn$stk<-substr(Drtn$stk,2,7)
Drtn<-Drtn[!is.na(Drtn$rtn),]
Drtn<-Drtn[Drtn$trday>=2,]
Drtn<-Drtn[,-4]

#import data of IPO, merging it with Drtn, delete the data of the IPO month, delete the data before Jan 2005
Dipo<-read.table("ipo.txt",skip=1,sep="\t",colClass=c("character","character"))
names(Dipo)<-c("stk","imonth")
Dipo$stk<-substr(Dipo$stk,2,7)
Drtn<-merge(Drtn,Dipo,all.x=T)
Drtn<-Drtn[is.na(Drtn$imonth),]####delete the data of the IPO month
###length(unique(Drtn$stk))####the stk value changes from 2887 to 1255 
Drtn<-Drtn[,-5]
Drtn$mon<-substr(Drtn$month,5,6)
Drtn$year<-substr(Drtn$month,1,4)
Drtn<-Drtn[Drtn$year>=2005,]
head(Drtn)

#import the market return, use the method of market value-weignted, delete the data before Jan 2005
Dmkt<-read.table("market return.txt",skip=1,sep="\t",colClass=c("integer","character","numeric","numeric"))
names(Dmkt)<-c("mkt","month","lmrtn","mrtn")
Dmkt$year<-substr(Dmkt$month,1,4)
Dmkt<-Dmkt[Dmkt$mkt=="5",]####5 is the market return of A-share market
Dmkt<-Dmkt[Dmkt$year>=2005,]
Dmkt<-Dmkt[,-1]
head(Dmkt)

#import the net asset data, select the parent statement, select the companies with the net assets > 100 million 
Dasset<-read.table("asset.txt",skip=1,sep="\t",colClass=c("character","character","character","numeric"))
names(Dasset)<-c("stk","month","type","equity")
Dasset<-Dasset[Dasset$type=="A",]
Dasset$stk<-substr(Dasset$stk,2,7)
Dasset<-Dasset[,-3]
Dasset$year<-substr(Dasset$month,1,4)
equity<-aggregate(Dasset$equity,list(Dasset$stk,Dasset$year),max)
names(equity)<-c("stk","year","equity")

Drtn<-merge(Drtn,equity,all.x=T)
Drtn<-Drtn[Drtn$equity>1,]
###length(unique(Drtn$stk))#the skt number is 1251 now

Drtn<-Drtn[order(Drtn[,1],Drtn[,3]),]
Dar<-merge(Drtn,Dmkt,all.x=T)
head(Dar)

#####calculate ar
Dar<-Dar[Dar$mon>="07"&Dar$mon<="12",]
Dar$ar<-Dar$rtn-Dar$lmrtn
Dar<-Dar[!is.na(Dar$stk),]

####read the data of quarterly reports, semi-annual reports and annual reports. p.s: the value of eps is after adjustment
Dnb<-read.table("semi-annual.txt",skip=1,sep="\t",colClass=c("character","integer","character","character","numeric"))
names(Dnb)<-c("stk","type","acmon","month","eps")
Dnb$stk<-substr(Dnb$stk,2,7)
Dnb<-Dnb[!is.na(Dnb$eps),]

###select the semi-annual reports
Dbnb<-Dnb[Dnb$type=="2",]
Dbnb$year<-substr(Dbnb$month,1,4)
Dbnb<-Dbnb[Dbnb$year!="2000",]
Dbnb$mon<-substr(Dbnb$month,5,6)

##table(Dbnb$mon)#the report month of semi-annual report includes Feb, July, Aug, Sept, Oct, Dec, while the July and Aug are the majority, delete other months

###delete the stocks whose semi-annual reports published on Feb, Oct, Dec
Dbnb<-Dbnb[Dbnb$mon=="07"|Dbnb$mon=="08",]

####delete the stocks whose semi-annual reports are not complete during the period 2001-2016
bnb<-0
for(k in 1:(length(unique(Dbnb$stk)))){
	stk<-sort(unique(Dbnb$stk))[k]
	if(nrow(Dbnb[Dbnb$stk==stk,])==16){
		sa<-Dbnb[Dbnb$stk==stk,]
		ss<-rbind(bnb,sa)
		bnb<-ss
	}
}
bnb<-bnb[!bnb$stk=="0",]
Dbnb<-bnb
###length(unique(Dbnb$stk)) ##left 522 stocks

####calculate Sue
#####1st algorithm: sue1=ue/sd1,ue represents the first-order differential of the past 5 periods, sd1 represents the sd of ue in past 4 periods
#####2nd algorithm £¬sue2=ue/sd2,sd2 represents the sd of eps in past 4 periods
Dsue<-0
for(i in 1:(length(unique(Dbnb$year))-4)){
year1<-sort(unique(Dbnb$year))[i]
year2<-sort(unique(Dbnb$year))[i+1]
year3<-sort(unique(Dbnb$year))[i+2]
year4<-sort(unique(Dbnb$year))[i+3]
year5<-sort(unique(Dbnb$year))[i+4]

for(k in 1:(length(unique(Dbnb$stk)))){
stk<-sort(unique(Dbnb$stk))[k]
if(length(Dbnb[Dbnb$year>=year1&Dbnb$year<=year5&Dbnb$stk==stk,])<5){
break
}
data<-Dbnb[Dbnb$year>=year1&Dbnb$year<=year5&Dbnb$stk==stk,c("stk","eps","year")]
Due<-data[data$year>=year2&data$year<=year5,]
Due$ue<-diff(data$eps,1)
ss<-aggregate(Due$ue,list(Due$stk),sd) ####calculatesd1
names(ss)<-c("stk","sd1")
xx<-aggregate(Due$eps,list(Due$stk),sd) ####calculate sd2
names(xx)<-c("stk","sd2")
ss<-merge(ss,xx,all.x=T)
ss$year<-year5
ss$ue<-Due[Due$year==year5,]$ue
ss$sue1<-ss$ue/ss$sd1
ss$sue2<-ss$ue/ss$sd2
sa<-rbind(Dsue,ss)
Dsue<-sa
}
}
Dsue<-Dsue[!Dsue$stk=="0",]
##a<-Dsue
Dsue<-merge(Dsue,Dbnb[,c("stk","month","eps","year","mon")])
names(Dsue)<-c("stk","year","sd1","sd2","ue","sue1","sue2","pmonth","eps","mon")
Dsue<-Dsue[,-10]#delete mon
Dsue<-Dsue[,-3]#delete sd1
Dsue<-Dsue[,-3]#delete sd2


####merge the table of ar and sue
z<-merge(Dar,Dsue,all=T)
z<-z[!is.na(z$sue1),] 
z<-z[!is.na(z$sue2),] 
###length(unique(z$stk)) ###the skt number remains unchange, 522

####select 5-month data after the reporting month, e.g: if the report published on July, use the data from July to Nov
z$pmon<-substr(z$pmonth,5,6)
z1<-z[z$pmon=="08"&z$mon>="08"&z$mon<="12",]
z1$no<-as.integer(z1$mon)-7
z2<-z[z$pmon=="07"&z$mon>="07"&z$mon<="11",]
z2$no<-as.integer(z2$mon)-6
z<-rbind(z1,z2)
z<-z[!is.na(z$stk),] #519Ö»

####delete the stocks whose data of ar are not complete(Each stock should have 60 records) the skt is 216 now
pead<-0
for(k in 1:(length(unique(z$stk)))){
	stk<-sort(unique(z$stk))[k]
	if(nrow(z[z$stk==stk,])==60){
	sa<-z[z$stk==stk,]
	ss<-rbind(pead,sa)
	pead<-ss
	}
}
pead<-pead[!pead$stk=="0",]
pead<-pead[,-16]

####calculate CAR
ss<-0
xa<-0
for(k in 1:(length(unique(pead$year)))){
	year<-sort(unique(pead$year))[k]
	for(i in 1:(length(unique(pead$mon)))){
		number<-sort(unique(pead$no))[i]
		data<-pead[pead$year==year&pead$no==number,c("stk","ar","year","mon")]
		xx<-rbind(xa,data)
		xa<-xx
		xa<-xa[!xa$stk=="0",]
		car<-aggregate(xa$ar,list(xa$year,xa$stk),sum)
		names(car)<-c("year","stk","car")
		car$no<-number
		sa<-rbind(ss,car)
		ss<-sa
	}
	xa<-0
}
ss<-ss[!ss$stk=="0",]

####merge the table of pead and ss
Dcar<-merge(pead,ss,all.x=T)

#####group according to sue
####group according to sue1,sue2, ue respectively, comparing the phenomenon of PEAD in different calculations 
####group according to sue1
Dcar$dsue1<-as.integer(cut(Dcar$sue1,breaks=c(min(Dcar$sue1),0,max(Dcar$sue1))))
b<-cut(Dcar$sue1,breaks=c(min(Dcar$sue1),0,max(Dcar$sue1)))
attr(b,'levels')
pead1<-aggregate(Dcar$car,list(Dcar$no,Dcar$dsue1),mean)
names(pead1)<-c("mon","dsue1","avcar")
#####car t-test test whether the PEAD phenomenon is exist
t.test(Dcar[Dcar$dsue1==1&Dcar$no==2,]$car,Dcar[Dcar$dsue1==1&Dcar$no==5,]$car,paired=T)
t.test(Dcar[Dcar$dsue1==2&Dcar$no==2,]$car,Dcar[Dcar$dsue1==2&Dcar$no==4,]$car,paired=T)

pead1<-reshape(pead1,idvar="mon",timevar="dsue1",direction="wide")
pead1
write.csv(pead1,"pead1.csv")

####group according to sue2
Dcar$dsue2<-as.integer(cut(Dcar$sue2,breaks=c(min(Dcar$sue2),0,max(Dcar$sue2))))
pead2<-aggregate(Dcar$car,list(Dcar$no,Dcar$dsue2),mean)
b<-cut(Dcar$sue2,breaks=c(min(Dcar$sue2),0,max(Dcar$sue2)))
attr(b,'levels')
names(pead2)<-c("mon","dsue2","avcar")
pead2<-reshape(pead2,idvar="mon",timevar="dsue2",direction="wide")
pead2
write.csv(pead2,"pead2.csv")
####group according to ue
Dcar$due1<-as.integer(cut(Dcar$ue,breaks=c(min(Dcar$ue),0,max(Dcar$ue))))
b<-cut(Dcar$ue,breaks=c(min(Dcar$ue),0,max(Dcar$ue)))
attr(b,'levels')
pead3<-aggregate(Dcar$car,list(Dcar$no,Dcar$due1),mean)
names(pead3)<-c("mon","due1","avcar")
pead3<-reshape(pead3,idvar="mon",timevar="due1",direction="wide")
pead3
write.csv(pead3,"pead3.csv")

######choose the method of sue1(best), diving the interval into 10 groups, comparing the difference of PEAD phenomenon among the 10 groups
Dcar$dsue3<-as.integer(cut(Dcar$sue1,breaks=c(min(Dcar$sue1),
 quantile(Dcar$sue1,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)))))
b<-cut(Dcar$sue1,breaks=c(min(Dcar$sue1),
 quantile(Dcar$sue1,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))))
attr(b,'levels')
pead4<-aggregate(Dcar$car,list(Dcar$no,Dcar$dsue3),mean)
names(pead4)<-c("mon","dsue3","avcar")
pead4<-reshape(pead4,idvar="mon",timevar="dsue3",direction="wide")
pead4
write.csv(pead4,"pead4.csv")

#########comparing the phenomenon of PEAD among different size of companies
Dcar$dmv<-as.integer(cut(Dcar$mv,breaks=c(min(Dcar$mv)-1,
quantile(Dcar$mv,probs=c(0.25,0.65,1.0)))))

scar<-Dcar[Dcar$dmv=="1",c("month","no","stk","mv","rtn","car","sue1","ar")]
mcar<-Dcar[Dcar$dmv=="2",c("month","no","stk","mv","rtn","car","sue1","ar")]
bcar<-Dcar[Dcar$dmv=="3",c("month","no","stk","mv","rtn","car","sue1","ar")]

#####devide the small-size companies into two groups
scar$dsue1<-as.integer(cut(scar$sue1,breaks=c(min(scar$sue1),0,max(scar$sue1))))
scar<-scar[!is.na(scar$dsue1),]
spead1<-aggregate(scar$car,list(scar$no,scar$dsue1),mean)
names(spead1)<-c("mon","dsue","avcar")

####t-test
t.test(spead1[spead1$dsue==1,]$avcar,spead1[spead1$dsue==2,]$avcar)

spead1<-reshape(spead1,idvar="mon",timevar="dsue",direction="wide")
spead1
write.csv(spead1,"spead1.csv")

scar1<-scar[scar$dsue1==1,]
scar2<-scar[scar$dsue1==2,]



#####devide the small-size company into 10 groups
scar$dsue2<-as.integer(cut(scar$sue1,breaks=c(min(Dcar$sue1),
 quantile(scar$sue1,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)))))
spead2<-aggregate(scar$car,list(scar$no,scar$dsue2),mean)
names(spead2)<-c("mon","dsue","avcar")
spead2<-reshape(spead2,idvar="mon",timevar="dsue",direction="wide")
spead2

#####devide the medium-size companies into 2 groups
mcar$dsue1<-as.integer(cut(mcar$sue1,breaks=c(min(mcar$sue1),0,max(mcar$sue1))))
mcar<-mcar[!is.na(mcar$dsue1),]
mpead1<-aggregate(mcar$car,list(mcar$no,mcar$dsue1),mean)
names(mpead1)<-c("mon","dsue","avcar")

####t-test
t.test(mpead1[mpead1$dsue==1,]$avcar,mpead1[mpead1$dsue==2,]$avcar)

mpead1<-reshape(mpead1,idvar="mon",timevar="dsue",direction="wide")
mpead1
write.csv(mpead1,"mpead1.csv")
mcar1<-mcar[mcar$dsue1==1,]
mcar2<-mcar[mcar$dsue1==2,]

#####devide the medium-size companies into 10 groups
mcar$dsue2<-as.integer(cut(mcar$sue1,breaks=c(min(mcar$sue1),
 quantile(mcar$sue1,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)))))
mpead2<-aggregate(mcar$car,list(mcar$no,mcar$dsue2),mean)
names(mpead2)<-c("mon","dsue","avcar")
mpead2<-reshape(mpead2,idvar="mon",timevar="dsue",direction="wide")
mpead2

#####devide the large-size companies into 2 groups
bcar$dsue1<-as.integer(cut(bcar$sue1,breaks=c(min(bcar$sue1),0,max(bcar$sue1))))
bcar<-bcar[!is.na(bcar$dsue1),]
bpead1<-aggregate(bcar$car,list(bcar$no,bcar$dsue1),mean)
names(bpead1)<-c("mon","dsue","avcar")

####t-test
t.test(bpead1[bpead1$dsue==1,]$avcar,bpead1[bpead1$dsue==2,]$avcar)

bpead1<-reshape(bpead1,idvar="mon",timevar="dsue",direction="wide")
bpead1
write.csv(bpead1,"bpead1.csv")
bcar1<-bcar[bcar$dsue1==1,]
bcar2<-bcar[bcar$dsue1==2,]



#####devide the large-size companies into 10 groups
bcar$dsue2<-as.integer(cut(bcar$sue1,breaks=c(min(bcar$sue1),
 quantile(bcar$sue1,probs=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)))))
bpead2<-aggregate(bcar$car,list(bcar$no,bcar$dsue2),mean)
names(bpead2)<-c("mon","dsue","avcar")
bpead2<-reshape(bpead2,idvar="mon",timevar="dsue",direction="wide")
bpead2

#####t-test of the car in two calculations 
t.test(pead2[pead2$dsue1==1,]$avcar,pead2[pead2$dsue1==2,]$avcar)
t.test(pead1[pead1$dsue1==1,]$avcar,pead1[pead1$dsue1==2,]$avcar)


######import the 4-factor model
Df<-read.table("f4.txt",skip=1,sep="\t",colClass=c("character","numeric","numeric","numeric","numeric"))
names(Df)<-c("month","rmrf","smb","hml","mom")
Df$year<-substr(Df$month,1,4)
Df<-Df[Df$year>="2005",]
Df0<-merge(Dcar[,c("sue1","car","month","dsue1","rtn","lmrtn")],Df,all.x=T)
Df1<-merge(scar,Df,all.x=T)
Df2<-merge(mcar,Df,all.x=T)
Df3<-merge(bcar,Df,all.x=T)

####import risk-free rate
rf<-read.table("rf.txt",skip=1,sep="\t",colClass=c("character","numeric"))
names(rf)<-c("month","rf")
rf$year<-substr(rf$month,1,4)
rf<-rf[rf$year>="2005",]
rf$rf<-rf$rf/100
Df0<-merge(Df0,rf,all.x=T)
Df1<-merge(Df1,rf,all.x=T)
Df2<-merge(Df2,rf,all.x=T)
Df3<-merge(Df3,rf,all.x=T)

#####the regression based on 4-factor models
p3<-lm(car~rmrf+smb+hml+mom+sue1,Df0)
summary(p3) ##significant level:***
p4<-lm(car~sue1,Df0)
summary(p4) ##***

#####test whether the PEAD phenomenon could be explained by the risk pricing thoery
l1<-lm(car~rmrf+smb+hml+mom,Df0[Df0$dsue1==1,])
summary(l1) 
l2<-lm(car~rmrf+smb+hml+mom,Df0[Df0$dsue1==2,])
summary(l2) 

#######the result of the regression
dat1<-data.frame(alpha=NA,at=NA,rmrf=NA,rmrft=NA,smb=NA,smbt=NA,
hml=NA,hmlt=NA,mom=NA,momt=NA,sue=NA,suet=NA,r2=NA)

dat1$alpha<-summary(p3)$coef[1]
dat1$rmrf<-summary(p3)$coef[2]
dat1$smb<-summary(p3)$coef[3]
dat1$hml<-summary(p3)$coef[4]
dat1$mom<-summary(p3)$coef[5]
dat1$sue<-summary(p3)$coef[6]
dat1$at<-summary(p3)$coef[13]
dat1$rmrft<-summary(p3)$coef[14]
dat1$smbt<-summary(p3)$coef[15]
dat1$hmlt<-summary(p3)$coef[16]
dat1$momt<-summary(p3)$coef[17]
dat1$suet<-summary(p3)$coef[18]
dat1$r2<-summary(p3)$adj.r.squared
write.csv(dat1,"dat1.csv")

###########import the result of the test of risk pricing theory
dat2<-data.frame(alpha=NA,at=NA,rmrf=NA,rmrft=NA,smb=NA,smbt=NA,
hml=NA,hmlt=NA,r2=NA)

dat2$alpha<-summary(l1)$coef[1]
dat2$rmrf<-summary(l1)$coef[2]
dat2$smb<-summary(l1)$coef[3]
dat2$hml<-summary(l1)$coef[4]

dat2$at<-summary(l1)$coef[9]
dat2$rmrft<-summary(l1)$coef[10]
dat2$smbt<-summary(l1)$coef[11]
dat2$hmlt<-summary(l1)$coef[12]
dat2$r2<-summary(l1)$adj.r.squared
write.csv(dat2,"dat2.csv")

dat3<-data.frame(alpha=NA,at=NA,rmrf=NA,rmrft=NA,smb=NA,smbt=NA,
hml=NA,hmlt=NA,r2=NA)

dat3$alpha<-summary(l2)$coef[1]
dat3$rmrf<-summary(l2)$coef[2]
dat3$smb<-summary(l2)$coef[3]
dat3$hml<-summary(l2)$coef[4]

dat3$at<-summary(l2)$coef[9]
dat3$rmrft<-summary(l2)$coef[10]
dat3$smbt<-summary(l2)$coef[11]
dat3$hmlt<-summary(l2)$coef[12]
dat3$r2<-summary(l2)$adj.r.squared
write.csv(dat3,"dat3.csv")

########the statistics summary based on Carhart's four-factor models
a1<-c(mean(Df0$car),median(Df0$car),sd(Df0$car),min(Df0$car),max(Df0$car))
a2<-c(mean(Df0$rmrf),median(Df0$rmrf),sd(Df0$rmrf),min(Df0$rmrf),max(Df0$rmrf))
a3<-c(mean(Df0$sue1),median(Df0$sue1),sd(Df0$sue1),min(Df0$sue1),max(Df0$sue1))
a4<-c(mean(Df0$smb),median(Df0$smb),sd(Df0$smb),min(Df0$smb),max(Df0$smb))
a5<-c(mean(Df0$hml),median(Df0$hml),sd(Df0$hml),min(Df0$hml),max(Df0$hml))
a6<-c(mean(Df0$mom),median(Df0$mom),sd(Df0$mom),min(Df0$mom),max(Df0$mom))
a7<-c(mean(Df0$rtn),median(Df0$rtn),sd(Df0$rtn),min(Df0$rtn),max(Df0$rtn))
a8<-c(mean(Df0$lmrtn),median(Df0$lmrtn),sd(Df0$lmrtn),min(Df0$lmrtn),max(Df0$lmrtn))
sa2<-rbind(a1,a2,a3,a4,a5,a6,a7,a8)
write.csv(sa2,"sa2.csv")


####the statistics summary of car with differnt size of companies
s1<-c(mean(scar$rtn),median(scar$rtn),sd(scar$rtn),min(scar$rtn),max(scar$rtn))
s2<-c(mean(scar$mv),median(scar$mv),sd(scar$mv),min(scar$mv),max(scar$mv))
s3<-c(mean(scar1$car),median(scar1$car),sd(scar1$car),min(scar1$car),max(scar1$car))
s4<-c(mean(scar1$sue1),median(scar1$sue1),sd(scar1$sue1),min(scar1$sue1),max(scar1$sue1))
s5<-c(mean(scar2$car),median(scar2$car),sd(scar2$car),min(scar2$car),max(scar2$car))
s6<-c(mean(scar2$sue1),median(scar2$sue1),sd(scar2$sue1),min(scar2$sue1),max(scar2$sue1))

m1<-c(mean(mcar$rtn),median(mcar$rtn),sd(mcar$rtn),min(mcar$rtn),max(mcar$rtn))
m2<-c(mean(mcar$mv),median(mcar$mv),sd(mcar$mv),min(mcar$mv),max(mcar$mv))
m3<-c(mean(mcar1$car),median(mcar1$car),sd(mcar1$car),min(mcar1$car),max(mcar1$car))
m4<-c(mean(mcar1$sue1),median(mcar1$sue1),sd(mcar1$sue1),min(mcar1$sue1),max(mcar1$sue1))
m5<-c(mean(mcar2$car),median(mcar2$car),sd(mcar2$car),min(mcar2$car),max(mcar2$car))
m6<-c(mean(mcar2$sue1),median(mcar2$sue1),sd(mcar2$sue1),min(mcar2$sue1),max(mcar2$sue1))

b1<-c(mean(bcar$rtn),median(bcar$rtn),sd(bcar$rtn),min(bcar$rtn),max(bcar$rtn))
b2<-c(mean(bcar$mv),median(bcar$mv),sd(bcar$mv),min(bcar$mv),max(bcar$mv))
b3<-c(mean(bcar1$car),median(bcar1$car),sd(bcar1$car),min(bcar1$car),max(bcar1$car))
b4<-c(mean(bcar1$sue1),median(bcar1$sue1),sd(bcar1$sue1),min(bcar1$sue1),max(bcar1$sue1))
b5<-c(mean(bcar2$car),median(bcar2$car),sd(bcar2$car),min(bcar2$car),max(bcar2$car))
b6<-c(mean(bcar2$sue1),median(bcar2$sue1),sd(bcar2$sue1),min(bcar2$sue1),max(bcar2$sue1))

sa1<-rbind(s1,s2,s3,s4,s5,s6,m1,m2,m3,m4,m5,m6,b1,b2,b3,b4,b5,b6)
write.csv(sa1,"sa1.csv")

####### correlation test among rmrf,sue,smb,hml
a<-cbind(Df$rmrf,Df$smb,Df$hml,Df$mom)
cor<-cor(a)
cor
write.csv(cor,"cor.csv")

save(Dipo,Drtn,Dmkt,Dasset,equity,Dar,Dbnb,Dnb,Dsue,z,pead,ss,rf,Dcar,
scar,mcar,bcar,Df,Df0,Df1,Df2,Df3,pead1,pead2,pead3,pead4,spead1,spead2,
mpead1,mpead2,bpead1,bpead2,p1,p2,p3,scar1,scar2,mcar1,mcar2,bcar1,bcar2,file="pead.RData")
##############


