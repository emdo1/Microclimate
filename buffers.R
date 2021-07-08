rm(list = ls())
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
#read in farm data
fs<-list.files("../Data/",pattern="Air")
##read in regional data
noaa<-read.csv("../Data/NOAA_JINOTEGA.csv",header=TRUE)
##add coordinates
l<-read.csv("../Data/locations.csv",header=TRUE)
noaa<-as.data.frame(cbind(Time=noaa$YEARMODA,AVE=noaa$TEMPCEL,MAX=noaa$MAXCE,Min=noaa$MINCE))
noaa$Time1<-ymd(noaa$Time)
buff.slope<-buff.se<-farmid<-maxbuff<-maxse<-minbuff<-minse<-NULL
#i<-9
for (i in 1: length(fs)){
  farmid[i]<-as.numeric((regmatches(fs[i], regexpr( "\\d+", fs[i]))))
  df<-read.csv(paste0("../Data/",fs[i]))
  setDT(df)[,paste0("Time",1:2) := tstrsplit(Time, " ")]
  df$Time1<-as.Date(df$Time1,"%d/%m/%y")
  m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius.C.))
  if (i == 4){
    df$Time1<-as.Date(df$Time1,"%d/%m/%Y")
    m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius.C.))
    m<-tail(m,-3)
    m<-head(m,-3)
    overlap<-match(m$Time1,noaa$Time1)
  }else if (i ==9){
    df$Time1<-as.Date(df$Time1,"%d/%m/%Y")
    m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius.C.))
    m<-tail(m,-3)
    m<-head(m,-3)
    overlap<-match(m$Time1,noaa$Time1)
  }else{
    m$Time1<-ymd(m$Time1)
    m<-tail(m,-3)
    m<-head(m,-3)
    overlap<-match(as.Date(m$Time1),noaa$Time1)	
  }
 
  #remove first and last 3 days of recording

  mod<-lm(m$me~noaa$AVE[overlap])
  buff.slope[i]<-coef(mod)[2]
  buff.se[i]<-summary(mod)$coefficients[2,2]
  
  maxt<-tapply(df$Celsius.C., df$Time1, max)
  maxt<-tail(maxt,-3)
  maxt<-head(maxt,-3)
  mint<-tapply(df$Celsius.C., df$Time1, min)
  mint<-tail(mint,-3)
  mint<-head(mint,-3)
  overlap2<-match(as.Date(names(maxt)),noaa$Time1)	
  mod2<-lm(maxt~noaa$MAX[overlap2])
  maxbuff[i]<-coef(mod2)[2]
  maxse[i]<-summary(mod2)$coefficients[2,2]
  mod3<-lm(mint~noaa$Min[overlap2]) 
  minbuff[i]<-coef(mod3)[2]
  minse[i]<-summary(mod3)$coefficients[2,2]
    
    
  
}


for (i in 1:10){
  f<-as.numeric(paste0(farmid[i]))
  l$AVEbuff.slope[f]<-buff.slope[i]
  l$AVEbuff.se[f]<-buff.se[i]
  l$maxbuff[f]<-maxbuff[i]
  l$maxbuff.se[f]<- maxse[i]
  l$minbuff[f]<- minbuff[i]
  l$minbuff.se[f]<- minse[i]
}
#Boxplot### MANUSCRIPT FIGURE 4##################
temp.comb<-c(buff.slope,maxbuff,minbuff)
temp.code<-c(rep("average",length(buff.slope)), rep("maximum",length(buff.slope)), rep("minimum",length(buff.slope)))
par(mar=c(5,3,1,1))
plot(temp.comb~as.factor(temp.code),
     xlab="",ylab="Buffering effect",
     ylim=c(0,1.5),horizontal=TRUE,
     boxfill="grey",
     lwd=3,lty=1,cex.axis=1.5,cex.lab=1.8
)
abline(v=1,lty="dashed")
box(lwd=2)


#rm(list = ls())
#library(dplyr)
#library(stringr)
#library(lubridate)
#read in farm data
fs<-list.files("../Data/",pattern="Soil")
noaa<-read.csv("../Data/NOAA_JINOTEGA.csv",header=TRUE)
#l<-read.csv("../Data/locations.csv",header=TRUE)
noaa<-as.data.frame(cbind(Time=noaa$YEARMODA,AVE=noaa$TEMPCEL,MAX=noaa$MAXCE,Min=noaa$MINCE))
noaa$Time1<-ymd(noaa$Time)
buff.slope<-buff.se<-farmid<-maxbuff<-maxse<-minbuff<-minse<-NULL

for (i in 1: length(fs)){
  farmid[i]<-as.numeric((regmatches(fs[i], regexpr( "\\d+", fs[i]))))
  df<-read.csv(paste0("../Data/",fs[i]))
  #df$date <- as.Date(with(df, paste(Year,Month,Day,sep="-")), "%Y-%m-%d")
  setDT(df)[,paste0("Time",1:2) := tstrsplit(Time, " ")]
  #m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius..C.))
 if (i ==5){
    df$Time1<-as.Date(df$Time1,"%d/%m/%Y")
    m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius..C.))
    m<-tail(m,-3)
    m<-head(m,-3)
    overlap<-match(m$Time1,noaa$Time1)
    }else if (i ==9){
      df$Time1<-as.Date(df$Time1,"%d/%m/%Y")
      m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius..C.))
      m<-tail(m,-3)
      m<-head(m,-3)
      overlap<-match(m$Time1,noaa$Time1)
  }else{
    m<-df %>% group_by(Time1) %>% summarise(me=mean(Celsius..C.))
    m$Time1<-ymd(m$Time1)
    m<-tail(m,-3)
    m<-head(m,-3)
    overlap<-match(as.Date(m$Time1),noaa$Time1)	
  }
 # m$Time1<-ymd(m$Time1)
  #remove first and last 3 days of recording
#  m<-tail(m,-3)
 # m<-head(m,-3)
  #overlap<-match(as.Date(m$Time1),noaa$Time1)	
  mod<-lm(m$me~noaa$AVE[overlap])
  buff.slope[i]<-coef(mod)[2]
  buff.se[i]<-summary(mod)$coefficients[2,2]
  print(i)
  maxt<-tapply(df$Celsius..C., df$Time1, max)
  maxt<-tail(maxt,-3)
  maxt<-head(maxt,-3)
  mint<-tapply(df$Celsius..C., df$Time1, min)
  mint<-tail(mint,-3)
  mint<-head(mint,-3)
  overlap2<-match(as.Date(names(maxt)),noaa$Time1)	
  mod2<-lm(maxt~noaa$MAX[overlap2])
  maxbuff[i]<-coef(mod2)[2]
  maxse[i]<-summary(mod2)$coefficients[2,2]
  mod3<-lm(mint~noaa$Min[overlap2]) 
  minbuff[i]<-coef(mod3)[2]
  minse[i]<-summary(mod3)$coefficients[2,2]
}


for (i in 1:9){
  f<-as.numeric(paste0(farmid[i]))
  l$Avesoilbuff.slope[f]<-buff.slope[i]
  l$AVEsoilbuff.se[f]<-buff.se[i]
  l$maxsoilbuff[f]<-maxbuff[i]
  l$maxsoilbuff.se[f]<- maxse[i]
  l$minsoilbuff[f]<- minbuff[i]
  l$minsoilbuff.se[f]<- minse[i]
}
#Boxplot### MANUSCRIPT FIGURE 4##################
temp.comb<-c(buff.slope,maxbuff,minbuff)
temp.code<-c(rep("average",length(buff.slope)), rep("maximum",length(buff.slope)), rep("minimum",length(buff.slope)))
par(mar=c(3,5,1,1))
plot(temp.comb~as.factor(temp.code),
     xlab="",ylab="Buffering effect",
     ylim=c(0,1),
     boxfill="grey",
     lwd=3,lty=1,cex.axis=1.5,cex.lab=1.8
)
box(lwd=2)
temp.comb<-c(buff.slope,maxbuff,minbuff)
temp.code<-c(rep("average",length(buff.slope)), rep("maximum",length(buff.slope)), rep("minimum",length(buff.slope)))
par(mar=c(3,5,1,1))
plot(temp.comb~as.factor(temp.code),
     xlab="",ylab="Buffering effect",
     ylim=c(0,1),
     boxfill="grey",
     lwd=3,lty=1,cex.axis=1.5,cex.lab=1.8
)
box(lwd=2)

##soil boxplot
temp.soil<-c(l$minsoilbuff,l$maxsoilbuff,l$Avesoilbuff.slope)
temp.soil<-temp.soil[!is.na(temp.soil)]
tc<-c(rep("minimum",length(temp.soil)/3), rep("maximum",length(temp.soil)/3), rep("average",length(temp.soil)/3))
par(mar=c(3,5,1,1))
plot(temp.soil~tc,
     xlab="",ylab="Buffering effect",horizontal=TRUE,
     ylim=c(0,1),
     boxfill="grey",
     lwd=3,lty=1,cex.axis=1.5,cex.lab=1.8
)
abline(v=1,lty="dashed")
box(lwd=2)




###otherboxplotcode
plot(temp.comb~tc,
     +      xlab="",ylab="Buffering effect",
     +      ylim=c(0,1.5),horizontal=TRUE,
     +      boxfill="grey",
     +      lwd=3,lty=1,cex.axis=1.5,cex.lab=1.8
     + )
 abline(v=1,lty="dashed") box(lwd=2)
##lm plots
par(mar=c(4,5,2,2))
par(mfrow=c(3,2))
plot(l$Forest_cover,l$AVEbuff.slope,
     main="",col=1,pch=16,cex=2,
     xlab="",ylab="Mean buffer",cex.lab=1.8,
     cex.axis=1.5,lwd=2,ylim=c(0.1,1.1)
)
points(l$Forest_cover,l$Avesoilbuff.slope,
       col=2,pch=16,cex=2,lwd=2
)
abline(lm(l$AVEbuff.slope~l$Forest_cover),
       col=1,lwd=2,lty=5)
abline(lm(l$Avesoilbuff.slope~l$Forest_cover),
       col=2,lwd=2,lty=5)

#points(l$Forest_cover~l$Avesoilbuff.slope,
 #      col=3,pch=16,cex=2,lwd=2
#)
box(lwd=2)
plot(l$Elevation,l$AVEbuff.slope,
     main="",col=1,pch=16,cex=2,
     xlab="",ylab="",cex.lab=1.8,
     cex.axis=1.5,lwd=2,ylim=c(0.1,1.1)
)
points(l$Elevation,l$Avesoilbuff.slope,
       col=2,pch=16,cex=2,lwd=2
)
abline(lm(l$Avesoilbuff.slope~l$Elevation),
       col=2,lwd=2,lty=5)
abline(lm(l$AVEbuff.slope~l$Elevation),
       col=1,lwd=2,lty=5)
box(lwd=2)
#modfc<-lm(l$AVEbuff.slope~l$Forest_cover)
#modev<-lm(l$AVEbuff.slope~l$Elevation)
#par(mfrow=c(3,2))
#plot(l$Forest_cover,l$AVEbuff.slope,
#     main="",col=1,pch=16,cex=2,
#     xlab="",ylab="Mean buffering effect",cex.lab=1.8,
#     cex.axis=1.5,lwd=2,ylim=c(0.1,1.1)
#)
#par(mfrow=c(3,2))
##maximum forest cover
plot(l$maxbuff~l$Forest_cover,main="",col=1,pch=16,
     xlab="",ylab="Maximum buffer",cex.lab=1.8,cex.axis=1.5,
    lwd=2,ylim=c(0,1.5),cex=2)
points(l$maxsoilbuff~l$Forest_cover,col=2,pch=16,cex=2,lwd=2)
abline(lm(l$maxbuff~l$Forest_cover),
       col=1,lwd=2,lty=5)
abline(lm(l$maxsoilbuff~l$Forest_cover),
       col=2,lwd=2,lty=5)
box(lwd=2)
##maximum elevation
plot(l$Elevation,l$maxbuff,
     main="",col=1,pch=16,cex=2,
     xlab="",ylab="",cex.lab=1.8,
     cex.axis=1.5,lwd=2,ylim=c(0,1.5)
)
points(l$Elevation,l$maxsoilbuff,
       col=2,pch=16,cex=2,lwd=2
)
abline(lm(l$maxbuff~l$Elevation),
       col=1,lwd=2,lty=5)
abline(lm(l$maxsoilbuff~l$Elevation),
       col=2,lwd=2,lty=5)
box(lwd=2)
##minimum forest cover
plot(l$minbuff~l$Forest_cover,main="",col=1,pch=16,
     xlab="Forest Cover (% within 1000m radius)",ylab="Minimum buffer",cex.lab=1.8,cex.axis=1.5,
     lwd=2,ylim=c(0,1.5),cex=2)
points(l$minsoilbuff~l$Forest_cover,col=2,pch=16,cex=2,lwd=2)
abline(lm(l$minbuff~l$Forest_cover),
       col=1,lwd=2,lty=5)
abline(lm(l$minsoilbuff~l$Forest_cover),
       col=2,lwd=2,lty=5)
box(lwd=2)
###minimum elevation
plot(l$Elevation,l$minbuff,
     main="",col=1,pch=16,cex=2,
     xlab="Elevation (m above sea level)",ylab="",cex.lab=1.8,
     cex.axis=1.5,lwd=2,ylim=c(0,1.5)
)
points(l$Elevation,l$minsoilbuff,
       col=2,pch=16,cex=2,lwd=2
)
abline(lm(l$minbuff~l$Elevation),
       col=1,lwd=2,lty=5)
abline(lm(l$minsoilbuff~l$Elevation),
       col=2,lwd=2,lty=5)
box(lwd=2)







plot(l$maxbuff~l$Elevation,main="",col=1,pch=16,
     xlab="Elevation (m above sea level)",ylab="Maximum buffering effect",lwd=2,ylim=c(0.5,1.5))
plot(l$minbuff~l$Forest_cover,main="",col=1,pch=16,
     xlab="Forest Cover (% within 1000m radius)",ylab="Minimum buffering effect",lwd=2,ylim=c(0.2,0.8))
plot(l$minbuff~l$Elevation,main="",col=1,pch=16,
     xlab="Elevation (m above sea level)",ylab="Minimum buffering effect",lwd=2,ylim=c(0.2,0.8))

###summary plot
library(pracma)#for detrend function
ddf<-read.csv("../Data/noaafarm4_alldata.csv",header=TRUE)
ddf<-ddf[complete.cases(ddf[ , 1]),]
setDT(ddf)[,paste0("Time",1:2) := tstrsplit(Time, " ")]
ddf$Time1<-as.Date(ddf$Time1,"%d/%m/%Y")
ddf$dt_air<-detrend(ddf$air)
ddf$dt_soil<-detrend(ddf$soil)
ddf$dt_noaa<-detrend(ddf$noaa)
##detrending
ddf$day<-yday(ddf$Time1)
day.d<-unique(ddf$day)/365
##airdetrending
obs.mean.p<-tapply(ddf$air,as.factor(ddf$day),mean)	#
model.p<-lm(obs.mean.p~sin(day.d*2*pi)+cos(day.d*2*pi))	#Models seasonal patternn alone
##soildetrending
obs.mean.s<-tapply(ddf$soil,as.factor(ddf$day),mean)	#
model.s<-lm(obs.mean.s~sin(day.d*2*pi)+cos(day.d*2*pi))	#Models seasonal pattern alone
#noaadetrending
#obs.mean.n<-tapply(na.rm=T,ddf$noaa,as.factor(ddf$day),mean)	#
#model.n<-lm(obs.mean.n~sin(day.d*2*pi)+cos(day.d*2*pi))	#Models seasonal patternn alone
res<-c(resid(model.p),resid(model.s))
height<-rep(c(1:2),each=146)
nt<-ddf$noaa[!is.na(ddf$noaa)]
noaat<-median(nt)+(resid(model.n)*2)
airt<-median(ddf$air)+(resid(model.p)*2)
soilt<-median(ddf$soil)+(resid(model.s)*2)
plot(ddf$air,lwd=2,pch=16,xaxt="n",cex.lab=1.3,cex.axis=1.2,xlab="Month",ylab=expression(Temperature^o~C),
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
points(ddf$soil,pch=16,col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3))
points(ddf$noaa,pch=16,col = rgb(red = 0, green = 1, blue = 0, alpha = 0.3))
axis(1,at=c(1,588,1304,2049,2793),
     labels=c("May","Jun","Jul","Aug","Sep"))
legend(2900,33,pch=16,bty = "n", 
       c("above ground","soil","NOAA"),col=c("blue","red","green"),
horiz=F,pt.cex=1.6)
box(lwd=2)
ddf2<-reshape(ddf,direction="long",
              varying = list(names(ddf)[3:5]),
              v.names = "Temp",timevar ="Datetime",
              idvar = c("Time1","Time2"))
new_order <- with(ddf2, reorder(Datetime , Temp, median , na.rm=T))
boxplot(temps~ds,
        ylab=expression(Temperature^o~C),
        xaxt="n",
        col=c("blue","red","green"),lwd=2,cex.axis=1.2)
axis(1,at= c(1:3),labels=c("above ground","soil","NOAA"))
box(lwd=2)


##alt plots
ms<-read.csv("../Data/modelsummary.csv",header = TRUE,row.names = 1)
ms<-as.data.frame(t(ms))
#subsetelevation
mse<-head(ms,6)

plot(y=row(mse)[,1],x=(mse$slope)*10,xlim=c(min(mse$lci)*10,max(mse$uci)*10),pch=16,ylab="",xlab="Average regression coefficient",ylim=c(6,1),main="Elevation",cex=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(mse$lci*10,row(mse),mse$uci*10,row(mse),length=0.01,code=3)
abline(v=0,lty="dotted")
box(lwd=2)

##subset forest cover
msf<-tail(ms,6)
plot(y=row(msf)[,1],x=msf$slope,
     xlim=c(min(msf$lci),max(msf$uci)),
     pch=16,ylab="",
     xlab="Average regression coefficient",
     ylim=c(6,1),main="Forest cover",
     cex=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(msf$lci,row(msf),msf$uci,row(msf),length=0.01,code=3)
abline(v=0,lty="dotted")
box(lwd=2)

tdf<-data.frame(temp=numeric(),day=integer(),farmid=integer())

##ALL FARM air temp detrended BOXPLOTS
for (i in 1:length (fs)){
  df<-read.csv(paste0("../Data/",fs[i]))
  farmid<-as.numeric((regmatches(fs[i], regexpr( "\\d+", fs[i]))))
  setDT(df)[,paste0("Time",1:2) := tstrsplit(Time, " ")]
  df <- df[c(72:(nrow(df)-72)),]
    if (i == 4){
      df$Time1<-as.Date(df$Time1,"%d/%m/%Y")
    }else if (i ==9){
      df$Time1<-as.Date(df$Time1,"%d/%m/%Y")}
  else{
  df$Time1<-as.Date(df$Time1)}
  ##detrending
  df$day<-yday(df$Time1)
  day.d<-unique(df$day)/365
  ##airdetrending
  obs.mean.p<-tapply(df$Celsius.C.,as.factor(df$day),mean)	#
  model.p<-lm(obs.mean.p~sin(day.d*2*pi)+cos(day.d*2*pi))	#Models seasonal patternn alone
  res<-c(resid(model.p))
  airt<-median(df$Celsius.C.)+(resid(model.p)*2)
  d<-cbind(airt,farmid)
  tdf<-rbind(tdf,d)
  
  

  
  
}
elev<-c(520,602,504,1101,996,1094,1036,1045,958,693)
farmid<-c(1:10)
ef<-as.data.frame(cbind(elev,farmid))
tdf2<-merge(tdf,ef)
par(mar=c(5,5,1,1))
plot(tdf2$airt~as.factor(tdf2$elev), 
     ylab=expression(Temperature^o~C),
     lwd=2,cex.axis=1.2,cex.lab=1.5,
     xlab="Elevation of farm (m.a.s.l)")
     
box(lwd=2)