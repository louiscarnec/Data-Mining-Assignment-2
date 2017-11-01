f10_11 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2010_11.csv")
f11_12 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2011_12.csv")
f12_13 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2012_13.csv")
f13_14 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2013_14.csv")
f14_15 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2014_15.csv")
f15_16 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/2015_16.csv")

#f10_11,f11_12,f12_13,f13_14,f14_15,f15_16
install.packages("ggplot2")
library(ggplot2)


install.packages("dplyr")
library(dplyr)



#----------------########----------------#

rbind_list(f10_11,f11_12,f12_13,f13_14,f14_15,f15_16)
football <- rbind_all(list(f10_11,f11_12,f12_13,f13_14,f14_15,f15_16))
football$season <- 0
football$season[1:380] <- 1
football$season[381:760] <- 2
football$season[761:1140] <- 3
football$season[1141:1520] <- 4
football$season[1521:1900] <- 5
football$season[1900:2188] <- 6


attach(football)
#
summary(football)
summary(football$FTR)

#----------------########----------------#

#Full-time results over the years
FTR_years <- rbind(summary(f10_11$FTR),summary(f11_12$FTR),summary(f12_13$FTR),summary(f13_14$FTR),summary(f14_15$FTR),summary(f15_16$FTR))
summary(FTR_years)
summary(football)

a = 644
d=562
h=982
tot <- a+d+h
a/tot
h/tot
d/tot

pdf("boxplot_FTR_years.pdf")
boxplot(FTR_years,main='Results 2010-11 to Present')
graphics.off()


pdf("pie_chart_results_all_years.pdf")
pie(summary(football$FTR),main='Pie Chart Results for All Years')
graphics.off()



#Outlier in H comes form the fact that 2015-2016 season not over. need to get rid off it.
FTR_years_10to15 <- FTR_years[-6,]
pdf("boxplot_FTR_years_10to15.pdf")
boxplot(FTR_years_10to15,main='Results 2010-11 to 2015-16')
graphics.off()

xtable(as.data.frame(tapply(FTR=="H",HomeTeam,mean)))
xtable(as.data.frame(tapply(FTR=="H",list(HomeTeam,season),mean)))
tapply(FTR=="D",HomeTeam,mean)
tapply(FTR=="D",list(HomeTeam,season),mean)
xtable(as.data.frame(tapply(FTR=="A",AwayTeam,mean)))
xtable(as.data.frame(tapply(FTR=="A",list(AwayTeam,season),mean)))

diffWin <- as.data.frame(tapply(FTR=="H",HomeTeam,mean) - tapply(FTR=="A",AwayTeam,mean))
diffLoss <- as.data.frame(tapply(FTR=="A",HomeTeam,mean) - tapply(FTR=="H",AwayTeam,mean))
xtable(diffWin)
xtable(diffLoss)

pdf("boxplotratioH.pdf")
boxplot(tapply(FTR=="H",list(HomeTeam,season),mean),main="Ratio of Home Wins",xlab="Season",ylab="Ratio")
graphics.off()
pdf("boxplotratioA.pdf")
boxplot(tapply(FTR=="A",list(AwayTeam,season),mean),main="Ratio of Home Wins",xlab="Season",ylab="Ratio")
graphics.off()

#----------------########----------------#

#Yellow Cards per Team
mean(HY)
summary(HY)
sd(HY)
mean(AY) #Away higher yellow
summary(AY)
sd(AY)
tapply(HY,HomeTeam,mean)
tapply(AY,AwayTeam,mean)
tapply(HY,B365H,mean)
hist(HY)
#----------------########----------------#

#Red Cards
mean(HR)
summary(HR)
sd(HR)
mean(AR) #away higher red
summary(AR)
sd(AR)
hist(HR)

#----------------########----------------#

tapply(HY,HomeTeam,mean)
tapply(AY,AwayTeam,mean)
cor(tapply(HY,HomeTeam,mean),tapply(AY,AwayTeam,mean))

DiffHY <- as.data.frame(tapply(HY,HomeTeam,mean)-tapply(AY,AwayTeam,mean))
xtable(DiffHY)
mean(tapply(HY,HomeTeam,mean)-tapply(AY,AwayTeam,mean))

DiffHR <- as.data.frame(tapply(HR,HomeTeam,mean)-tapply(AR,AwayTeam,mean))
xtable(DiffHR)

tapply(HR,HomeTeam,mean)
tapply(AR,AwayTeam,mean)
cor(tapply(HR,HomeTeam,mean),tapply(AR,AwayTeam,mean))

tapply(HR,HomeTeam,mean)-tapply(AR,AwayTeam,mean)
mean(tapply(HR,HomeTeam,mean)-tapply(AR,AwayTeam,mean))

#Get both less yellow and red cards when playing at home

#----------------########----------------#
#FOULS
summary(football$HF)
summary(football$AF)
pdf("histHF.pdf")
hist(HF,breaks=15,main='Home Team fouls')
graphics.off()
pdf("histAF.pdf")
hist(AF,breaks=15,main='Away Team fouls')
graphics.off()
tapply(HF,HomeTeam,mean)
tapply(AF,AwayTeam,mean)
mean(tapply(HF,HomeTeam,mean))
mean(tapply(AF,AwayTeam,mean)) #Mean Away fouls per team is higher

mean(tapply(HF,HomeTeam,mean) - tapply(AF,AwayTeam,mean))

#----------------########----------------#
#CORNERS
summary(HC)
summary(AC)

pdf("histHC.pdf")
hist(HC,breaks=15)
graphics.off()
pdf("histAC.pdf")
hist(AC,breaks=15)
graphics.off()

tapply(HC,HomeTeam,mean)
tapply(AC,AwayTeam,mean)
mean(tapply(HC,HomeTeam,mean))
mean(tapply(AC,AwayTeam,mean))

#----------------########----------------#


attach(football)
FTHG[HomeTeam=="Chelsea"]
tapply(FTHG,HTHG,mean)

#----------------########----------------#

#Avg Shots per Team
HSteam <- as.data.frame(tapply(HS,HomeTeam,mean))
xtable(HSteam)

HSTteam <- as.data.frame(tapply(HST,HomeTeam,mean))
xtable(HSTteam)

ASteam <- as.data.frame(tapply(AS,AwayTeam,mean))
xtable(ASteam)

ASTteam <- as.data.frame(tapply(AST,AwayTeam,mean))
xtable(ASTteam)

pdf("histHS.pdf")
hist(tapply(HS,HomeTeam,mean),breaks=10,main="Histogram of mean shots per home team", xlab="Mean Shots")
graphics.off()
pdf("histAS.pdf")
hist(tapply(AS,AwayTeam,mean),breaks=10,main="Histogram of mean shots per away team", xlab="Mean Shots")
graphics.off()
summary(tapply(HS,HomeTeam,mean))

tapply(f10_11$HS,f10_11$HomeTeam,mean)
tapply(f11_12$HS,f11_12$HomeTeam,mean)
tapply(f12_13$HS,f12_13$HomeTeam,mean)
tapply(f13_14$HS,f13_14$HomeTeam,mean)
tapply(f14_15$HS,f14_15$HomeTeam,mean)
tapply(f15_16$HS,f15_16$HomeTeam,mean)

HSovertime <- rbind(tapply(f10_11$HS,f10_11$HomeTeam,mean),+
                    tapply(f11_12$HS,f11_12$HomeTeam,mean),+
                    tapply(f12_13$HS,f12_13$HomeTeam,mean),+
                    tapply(f13_14$HS,f13_14$HomeTeam,mean),+
                    tapply(f14_15$HS,f14_15$HomeTeam,mean),+
                    tapply(f15_16$HS,f15_16$HomeTeam,mean))

HSovertime
Hsovertime <- as.data.frame(HSovertime)
pdf("HSovertime.pdf")
plot.ts(Hsovertime,xaxt='n',plot.type=c("single"),col = sample(colours(), 20),main="HomeTeam Shots (Last 6 Seasons to Present)",ylab="Home Team Shots")
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

HSTovertime <- rbind(tapply(f10_11$HST,f10_11$HomeTeam,mean),+
                      tapply(f11_12$HST,f11_12$HomeTeam,mean),+
                      tapply(f12_13$HST,f12_13$HomeTeam,mean),+
                      tapply(f13_14$HST,f13_14$HomeTeam,mean),+
                      tapply(f14_15$HST,f14_15$HomeTeam,mean),+
                      tapply(f15_16$HST,f15_16$HomeTeam,mean))
Hstovertime <-as.data.frame(HSTovertime)
pdf("HSTovertime.pdf")
plot.ts(Hstovertime,xaxt='n',plot.type=c("single"),col = sample(colours(), 20),main="Home Shots on Target (Last 6 Seasons to Present)",ylab="Home Team Shots on Target")
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

mean(tapply(f10_11$HS,f10_11$HomeTeam,mean))
mean(tapply(f11_12$HS,f11_12$HomeTeam,mean))
mean(tapply(f12_13$HS,f12_13$HomeTeam,mean))
mean(tapply(f13_14$HS,f13_14$HomeTeam,mean))
mean(tapply(f14_15$HS,f14_15$HomeTeam,mean))
mean(tapply(f15_16$HS,f15_16$HomeTeam,mean))

sd(tapply(f10_11$HS,f10_11$HomeTeam,mean))
sd(tapply(f11_12$HS,f11_12$HomeTeam,mean))
sd(tapply(f12_13$HS,f12_13$HomeTeam,mean))
sd(tapply(f13_14$HS,f13_14$HomeTeam,mean))
sd(tapply(f14_15$HS,f14_15$HomeTeam,mean))
sd(tapply(f15_16$HS,f15_16$HomeTeam,mean))

sdHS <- rbind(sd(tapply(f10_11$HS,f10_11$HomeTeam,mean)),+
              sd(tapply(f11_12$HS,f11_12$HomeTeam,mean)),+
              sd(tapply(f12_13$HS,f12_13$HomeTeam,mean)),+
              sd(tapply(f13_14$HS,f13_14$HomeTeam,mean)),+
              sd(tapply(f14_15$HS,f14_15$HomeTeam,mean)),+
              sd(tapply(f15_16$HS,f15_16$HomeTeam,mean)))
sdHS <- as.data.frame(sdHS)
pdf("HSsd.pdf")
plot.ts(sdHS,xaxt='n',plot.type='single',main='Home Team Shots Standard Deviation',xlab='season',ylab='HS St. Dev.',type='b')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

mean(tapply(f10_11$HST,f10_11$HomeTeam,mean))
mean(tapply(f11_12$HST,f11_12$HomeTeam,mean))
mean(tapply(f12_13$HST,f12_13$HomeTeam,mean))
mean(tapply(f13_14$HST,f13_14$HomeTeam,mean))
mean(tapply(f14_15$HST,f14_15$HomeTeam,mean))
mean(tapply(f15_16$HST,f15_16$HomeTeam,mean))

sd(tapply(f10_11$HST,f10_11$HomeTeam,mean))
sd(tapply(f11_12$HST,f11_12$HomeTeam,mean))
sd(tapply(f12_13$HST,f12_13$HomeTeam,mean))
sd(tapply(f13_14$HST,f13_14$HomeTeam,mean))
sd(tapply(f14_15$HST,f14_15$HomeTeam,mean))
sd(tapply(f15_16$HST,f15_16$HomeTeam,mean))

sdHST <- rbind(sd(tapply(f10_11$HST,f10_11$HomeTeam,mean)),+
                sd(tapply(f11_12$HST,f11_12$HomeTeam,mean)),+
                sd(tapply(f12_13$HST,f12_13$HomeTeam,mean)),+
                sd(tapply(f13_14$HST,f13_14$HomeTeam,mean)),+
                sd(tapply(f14_15$HST,f14_15$HomeTeam,mean)),+
                sd(tapply(f15_16$HST,f15_16$HomeTeam,mean)))
sdHST <- as.data.frame(sdHST)
pdf("HSTsd.pdf")
par(mar=c(8.1,4.1,2.1,1.9))
plot.ts(sdHST,xaxt='n',plot.type='single',main='Home Team Shots on Target Standard Deviation',xlab='Season',ylab='HST St. Dev.',type='b')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

tapply(AS,AwayTeam,mean)
hist(tapply(AS,AwayTeam,mean), breaks=10)
mean(tapply(AS,AwayTeam,mean))

tapply(AS,HomeTeam,mean)
hist(tapply(AS,HomeTeam,mean),breaks=10)
mean(tapply(AS,HomeTeam,mean))

tapply(HS,AwayTeam,mean)
hist(tapply(HS,AwayTeam,mean), breaks=10)
mean(tapply(HS,AwayTeam,mean))

tapply(f10_11$AS,f10_11$AwayTeam,mean)
tapply(f11_12$AS,f11_12$AwayTeam,mean)
tapply(f12_13$AS,f12_13$AwayTeam,mean)
tapply(f13_14$AS,f13_14$AwayTeam,mean)
tapply(f14_15$AS,f14_15$AwayTeam,mean)
tapply(f15_16$AS,f15_16$AwayTeam,mean)

mean(tapply(f10_11$AS,f10_11$AwayTeam,mean))
mean(tapply(f11_12$AS,f11_12$AwayTeam,mean))
mean(tapply(f12_13$AS,f12_13$AwayTeam,mean))
mean(tapply(f13_14$AS,f13_14$AwayTeam,mean))
mean(tapply(f14_15$AS,f14_15$AwayTeam,mean))
mean(tapply(f15_16$AS,f15_16$AwayTeam,mean))

sd(tapply(f10_11$AS,f10_11$AwayTeam,mean))
sd(tapply(f11_12$AS,f11_12$AwayTeam,mean))
sd(tapply(f12_13$AS,f12_13$AwayTeam,mean))
sd(tapply(f13_14$AS,f13_14$AwayTeam,mean))
sd(tapply(f14_15$AS,f14_15$AwayTeam,mean))
sd(tapply(f15_16$AS,f15_16$AwayTeam,mean))

sdAS <- rbind(sd(tapply(f10_11$AST,f10_11$AwayTeam,mean)),+
                 sd(tapply(f11_12$AS,f11_12$AwayTeam,mean)),+
                 sd(tapply(f12_13$AS,f12_13$AwayTeam,mean)),+
                 sd(tapply(f13_14$AS,f13_14$AwayTeam,mean)),+
                 sd(tapply(f14_15$AS,f14_15$AwayTeam,mean)),+
                 sd(tapply(f15_16$AS,f15_16$AwayTeam,mean)))
sdAS <- as.data.frame(sdAS)

pdf("ASsd.pdf")
par(mar=c(8.1,4.1,2.1,1.9))
plot.ts(sdAS,xaxt='n',plot.type='single',main='Away Team Shots Standard Deviation',xlab='season',ylab='HTS St. Dev.',type='b')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

tapply(f10_11$AST,f10_11$AwayTeam,mean)
tapply(f11_12$AST,f11_12$AwayTeam,mean)
tapply(f12_13$AST,f12_13$AwayTeam,mean)
tapply(f13_14$AST,f13_14$AwayTeam,mean)
tapply(f14_15$AST,f14_15$AwayTeam,mean)
tapply(f15_16$AST,f15_16$AwayTeam,mean)

mean(tapply(f10_11$AST,f10_11$AwayTeam,mean))
mean(tapply(f11_12$AST,f11_12$AwayTeam,mean))
mean(tapply(f12_13$AST,f12_13$AwayTeam,mean))
mean(tapply(f13_14$AST,f13_14$AwayTeam,mean))
mean(tapply(f14_15$AST,f14_15$AwayTeam,mean))
mean(tapply(f15_16$AST,f15_16$AwayTeam,mean))

sd(tapply(f10_11$AST,f10_11$AwayTeam,mean))
sd(tapply(f11_12$AST,f11_12$AwayTeam,mean))
sd(tapply(f12_13$AST,f12_13$AwayTeam,mean))
sd(tapply(f13_14$AST,f13_14$AwayTeam,mean))
sd(tapply(f14_15$AST,f14_15$AwayTeam,mean))
sd(tapply(f15_16$AST,f15_16$AwayTeam,mean))

sdAST <- rbind(sd(tapply(f10_11$AST,f10_11$AwayTeam,mean)),+
               sd(tapply(f11_12$AST,f11_12$AwayTeam,mean)),+
               sd(tapply(f12_13$AST,f12_13$AwayTeam,mean)),+
               sd(tapply(f13_14$AST,f13_14$AwayTeam,mean)),+
               sd(tapply(f14_15$AST,f14_15$AwayTeam,mean)),+
               sd(tapply(f15_16$AST,f15_16$AwayTeam,mean)))
sdAST <- as.data.frame(sdAST)

pdf("ASTsd.pdf")
par(mar=c(8.1,4.1,2.1,1.9))
plot.ts(sdAST,xaxt='n',plot.type='single',main='Away Team Shots on Target Standard Deviation',xlab='season',ylab='HTS St. Dev.',type='b')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

ASovertime <- rbind(tapply(f10_11$AS,f10_11$AwayTeam,mean),+
                    tapply(f11_12$AS,f11_12$AwayTeam,mean),+
                    tapply(f12_13$AS,f12_13$AwayTeam,mean),+
                    tapply(f13_14$AS,f13_14$AwayTeam,mean),+
                    tapply(f14_15$AS,f14_15$AwayTeam,mean),+
                    tapply(f15_16$AS,f15_16$AwayTeam,mean))
Asovertime <- as.data.frame(ASovertime)

pdf("ASovertime.pdf")
par(mar=c(8.1,4.1,2.1,1.9))
plot.ts(Asovertime,xaxt='n',plot.type=c("single"),col = sample(colours(), 20),main="Away Team Shots (Last 6 Seasons to Present)",ylab="Away Team Shots",xlab='Season')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

ASTovertime <- rbind(tapply(f10_11$AST,f10_11$AwayTeam,mean),+
                      tapply(f11_12$AST,f11_12$AwayTeam,mean),+
                      tapply(f12_13$AST,f12_13$AwayTeam,mean),+
                      tapply(f13_14$AST,f13_14$AwayTeam,mean),+
                      tapply(f14_15$AST,f14_15$AwayTeam,mean),+
                      tapply(f15_16$AST,f15_16$AwayTeam,mean))
Astovertime <- as.data.frame(ASTovertime)

pdf("ASTovertime.pdf")
par(mar=c(8.1,4.1,2.1,1.9))
plot.ts(Astovertime,xaxt='n',plot.type=c("single"),col = sample(colours(), 20),main="Away Team Shots on Target (Last 6 Seasons to Present)",ylab="Away Team Shots on Target",xlab='Season')
axis(1, at=1:6, labels=c('2010-2011','2011-2012','2012-2013','2013-2014','2014-2015','2015-present'),las=2)
graphics.off()

##Boxplot
pdf("boxHS.pdf")
boxplot(HS~HomeTeam,data=football,las=2,main="Home shots by home team")
graphics.off()
pdf("boxAS.pdf")
boxplot(AS~AwayTeam,data=football,las=2,main="Away shots by away team")
graphics.off()
pdf("boxHS.pdf")
boxplot(HS~HomeTeam,data=football,las=2,main="Home shots by home team")
graphics.off()
pdf("boxAS.pdf")
boxplot(AS~AwayTeam,data=football,las=2,main="Away shots by away team")
graphics.off()


#----------------########----------------#

#ON TARGET

##Boxplot
pdf('BoxplotHST.pdf')
par(mar=c(7.1,5.1,2.1,1.9))
boxplot(HST~HomeTeam,data=football,las=2,ylab='HST',main='HST Distribution per Team')
graphics.off()

pdf('BoxplotAST.pdf')
par(mar=c(7.1,5.1,2.1,1.9))
boxplot(AST~AwayTeam,data=football,las=2,ylab='AST',main='AST Distribution per Team')
graphics.off()

#----------------########----------------#
#Ratio on Target

install.packages("hexbin")
library(hexbin)
pdf("RatioHSTHS.pdf")
bin <- hexbin(HS,HST,xbins=50)
pdf("RatioHSTHS.pdf")
plot(bin)
graphics.off()
pdf("RatioASTAS.pdf")
bin2 <- hexbin(AS,AST,xbins=50)
pdf("RatioASTAS.pdf")
plot(bin2)
graphics.off()

#----------------########----------------#
#FTHG
summary(FTHG)
sd(FTHG)

hist(FTHG,breaks=8)
axis(1, at=1:8)

#FTAG
summary(FTAG)
sd(FTAG)

hist(FTAG,breaks=8)
axis(1, at=1:8)

FTHGHOMETEAM <- as.data.frame(tapply(FTHG,HomeTeam,mean))
xtable(FTHGHOMETEAM)
mean(tapply(FTHG,HomeTeam,mean))
sd(tapply(FTHG,HomeTeam,mean))
par(mar=c(7.1,5.1,2.1,1.9))
boxplot(FTHG~HomeTeam,data=football,las=2,ylab='FTHG',main='FTHG Distribution per Team')

FTAGAWAYTEAM <- as.data.frame(tapply(FTAG,AwayTeam,mean))
xtable(FTAGAWAYTEAM)
mean(tapply(FTAG,HomeTeam,mean))
sd(tapply(FTAG,HomeTeam,mean))
par(mar=c(7.1,5.1,2.1,1.9))
boxplot(FTAG~AwayTeam,data=football,las=2,ylab='FTAG',main='FTAG Distribution per Team')

FTHGdf <- as.data.frame(tapply(FTHG,list(season,HomeTeam),mean))


FTAGdf <- as.data.frame(tapply(FTAG,list(season,AwayTeam),mean))
plot.ts(FTAGnoNA,plot.type=c('single'))

#Plot of FTHG for Teams that have played consistenly in premier division for the past 6 years
OldTimersFTHG <- FTHGdf[,complete.cases(t(FTHGdf))]
pdf("OldTimerFTHG.pdf")
plot.ts(OldTimersFTHG,plot.type=c('single'),type='b',col = sample(colours(), 12),main="Home Team Goals per season",xlab="Season",ylab="Mean Goals")
graphics.off()

#Plot of FTAG for Teams that have played consistenly in premier division for the past 6 years
OldTimersFTAG <- FTAGdf[,complete.cases(t(FTAGdf))]
pdf("OldTimerFTAG.pdf")
plot.ts(OldTimersFTAG,plot.type=c('single'),type='b',col = sample(colours(), 12), main="Away Team Goals per season",xlab="Season",ylab="Mean Goals")
graphics.off()

mean(OldTimers[1,])
#FTHG vs FTAG
library(hexbin)
bin3 <- hexbin(FTHG,FTAG,xbins=20)
pdf("hexbinFTHGFTAG.pdf")
plot(bin3)
graphics.off()

#FTHG vs HTHG
bin4 <- hexbin(HTHG,FTHG,xbins=20)
plot(bin4)

#FTAG vs HTAG
bin5 <- hexbin(HTAG,FTAG,xbins=20)
plot(bin5)

#FTHG Goals per Team
tapply(FTHG,HomeTeam,sum)
seasonsHG <- as.data.frame(tapply(FTHG,list(HomeTeam,season),sum))
seasonsHG <- seasonsHG[complete.cases(seasonsHG),]
xtable(seasonsHG)

for (i in 1:12){
  print(sum(seasonsHG[i,]))
  }

#FTAG Goals per Team
tapply(FTAG,AwayTeam,sum)
seasonsAG <- as.data.frame(tapply(FTAG,list(AwayTeam,season),sum))
seasonsAG <- seasonsAG[complete.cases(seasonsAG),]
xtable(seasonsAG)
tapply(FTAG,list(AwayTeam,season),sum)

tapply(FTHG,list(HomeTeam,season),sum)
tapply(FTAG,list(AwayTeam,season),sum)



GOALS <- tapply(FTHG,list(HomeTeam,season),sum) + tapply(FTAG,list(AwayTeam,season),sum)
ratioHG <- as.data.frame(tapply(FTHG,list(HomeTeam,season),sum) / GOALS)
ratioAG <- as.data.frame(tapply(FTAG,list(AwayTeam,season),sum)/GOALS)
xtable(ratioHG)
xtable(ratioAG)

pdf("ratioHG.pdf")
boxplot(ratioHG,main="Ratio Home Goals to Goals", xlab="season",ylab="Season Home Goals / Sum of Season Goals")
graphics.off()
pdf("ratioAG.pdf")
boxplot(ratioAG,main="Ratio Away Goals to Goals", xlab="season",ylab="Season Away Goals / Sum of Season Goals")
graphics.off()
#----------------########----------------#
#How good odds post?
tapply(B365H,list(FTR=="H",HomeTeam),mean)
tapply(B365A,list(FTR=="A",AwayTeam),mean)
tapply(B365D,list(FTR=="D",HomeTeam),mean)
tapply(B365D,list(FTR=="D",AwayTeam),mean)


hist(tapply(B365H,HomeTeam,mean),breaks=15)
hist(tapply(B365A,AwayTeam,mean),breaks=15)


#ODDS
#Comparing Home Win Odds
mean(B365H)
sd(B365H)
pdf("B365H.pdf")
hist(B365H,breaks=15)
graphics.off()

mean(B365A)
sd(B365A)
pdf("B365A.pdf")
hist(B365A,breaks=15)
graphics.off()

mean(B365D)
sd(B365D)
pdf("B365D.pdf")
hist(B365D,breaks=15)
graphics.off()


mean(BbAvH, na.ram=TRUE)
pdf("BbAvH.pdf")
hist(BbAvH)
graphics.off()

mean(BbAvA, na.ram=TRUE)
pdf("BbAvA.pdf")
hist(BbAvA)
graphics.off()

mean(BbAvD, na.ram=TRUE)
pdf("BbAvD.pdf")
hist(BbAvD)
graphics.off()


football$AvHomeWinOdds <- ((B365H+BSH+BWH+GBH+IWH+LBH+SBH+SJH+VCH+WHH/10))

#----------------########----------------#
#----------------########----------------#
#----------------########----------------#
library(arules)

#Try an Association analysis
associationfootball <- football[3:4]
associationfootball["FTR"] <- football[7]
associationfootball["season"] <- football[75]
associationfootball$season <- factor(associationfootball$season)
levels(associationfootball$season)

library(arules)
str(associationfootball)
#playlist <- lapply(playlist,unique)
associationfootball <- as.data.frame(associationfootball)
associationfootball <- as(associationfootball,"transactions")
summary(associationfootball)
itemFrequencyPlot(associationfootball[, itemFrequency(associationfootball) > 0.2], cex.names = 1)
rules <- apriori(associationfootball,parameter = list(support = 0.01,confidence = 0.5))
summary(rules)
inspect(rules)

associationfootball1 <- football[3:4]
associationfootball1["FTR"] <- football[7]
associationfootball1 <- as.data.frame(associationfootball1)
associationfootball1 <- as(associationfootball1,"transactions")
summary(associationfootball1)
itemFrequencyPlot(associationfootball1[, itemFrequency(associationfootball1) > 0.2], cex.names = 1)
rules1 <- apriori(associationfootball1,parameter = list(support = 0.005,confidence = 0.4))
summary(rules1)
inspect(rules1)


####################
football[ , colSums(is.na(football)) == 0]

test <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/test.csv")
test2 <- read.csv("/Users/Carnec/Desktop/Business_Analytics/Data Mining/Assignment2/test2.csv")


library(tree)
myFormula = FTR~HTHG + HTAG
tree.halftimegoal = tree(myFormula, football)
summary(tree.halftimegoal)
plot(tree.halftimegoal)
text(tree.halftimegoal,all=TRUE,cex=0.5)
footballhalftime.train = predict(tree.halftimegoal, football, type='class')
table(footballhalftime.train, football$FTR)

matchstatFormula = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstat = tree(matchstatFormula, football)
tree.matchstat
summary(tree.matchstat)
pdf("treematchstats.pdf")
plot(tree.matchstat)
text(tree.matchstat,all=TRUE,cex=0.5)
graphics.off()
football.train = predict(tree.matchstat, football, type='class')
xtable(table(football.train, football$FTR))
pdf("howgoodtreematchstats.pdf")
plot(football$AST~football$HST, pch=21,bg=c("red","green3","blue")[unclass(football$FTR)])
partition.tree(tree.matchstat,add=TRUE)
graphics.off()

football.test <- predict(tree.matchstat,test,type='class')
summary(football.test)
xtable(table(football.test,test$FTR))

football.test2 <- predict(tree.matchstat,test2,type='class')
summary(football.test2)
xtable(table(football.test2,test$FTR))

matchstatFormula1 = FTR ~ AC + HC + HF + AF + HY + AY + AR + HR + season
tree.matchstat1 = tree(matchstatFormula1, football)
tree.matchstat1
summary(tree.matchstat1)
plot(tree.matchstat1)
text(tree.matchstat1,all=TRUE,cex=0.5)


oddsFormula = FTR ~ B365H + B365A + B365D
tree.odds = tree(oddsFormula, football)
tree.odds
summary(tree.odds)
plot(tree.odds)
text(tree.odds,all=TRUE,cex=0.5)
footballOdds.train = predict(tree.odds, football, type='class')
table(footballOdds.train, football$FTR)

football2Formula = FTR ~ HST+ AST + B365H+ B365A + B365D + BSH + BSD + BSA + BWH + BWA + BWD
tree.final = tree(football2Formula, football)
summary(tree.final)
plot(tree.final)
text(tree.final,all=TRUE,cex=0.5)

football3Formula = FTR ~ AST + HST + AST + HC
tree.final1 = tree(football3Formula, football)
summary(tree.final1)
plot(tree.final1)
text(tree.final1,all=TRUE,cex=0.5)

BetBrainFormula = FTR ~ BbMxH + BbAvH + BbMxD + BbAvD + BbMxA + BbAvA
tree.BetBrain = tree(BetBrainFormula, football[season=="1",])
summary(tree.BetBrain)
pdf("BetBrain.pdf")
plot(tree.BetBrain)
text(tree.BetBrain,all=TRUE,cex=0.5)
graphics.off()

footballBetBrain.test <- predict(tree.BetBrain,test2,type='class')
summary(footballBetBrain.test)
xtable(table(footballBetBrain.test,test2$FTR))

pdf("howgoodtreematchstats.pdf")
plot(football$BbMxA~football$BbAvA, pch=21,bg=c("red","green3","blue")[unclass(football$FTR)])
partition.tree(tree.BetBrain,add=TRUE)
graphics.off()

#season 1
football[season=="1",]
matchstatFormulaseason1 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason1 = tree(matchstatFormulaseason1, football[season=="1",])
tree.matchstatseason1
summary(tree.matchstatseason1)
plot(tree.matchstatseason1)
text(tree.matchstatseason1,all=TRUE,cex=0.5)

footballseason1.train = predict(tree.matchstatseason1, football, type='class')
table(footballseason1.train, football$FTR)

cv.footballseason1 = cv.tree(tree.matchstatseason1,FUN=prune.misclass)
names(cv.footballseason1)
cv.footballseason1

prune.footballseason1 = prune.misclass(tree.matchstatseason1,best=5)
summary(prune.footballseason1)

pdf("season1tree.pdf")
plot(prune.footballseason1)
text(prune.footballseason1,all=TRUE,cex=0.5)
graphics.off()
footballseason1prune.train = predict(prune.footballseason1, football, type='class')
table(footballseason1prune.train, football$FTR)

#season 2
football[season=="2",]
matchstatFormulaseason2 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason2 = tree(matchstatFormulaseason2, football[season=="2",])
tree.matchstatseason2
summary(tree.matchstatseason2)

plot(tree.matchstatseason2)
text(tree.matchstatseason2,all=TRUE,cex=0.5)

footballseason2.train = predict(tree.matchstatseason2, football, type='class')
table(footballseason2.train, football$FTR)

cv.footballseason2 = cv.tree(tree.matchstatseason2,FUN=prune.misclass)
names(cv.footballseason1)
cv.footballseason2

prune.footballseason2 = prune.misclass(tree.matchstatseason2,best=5)
summary(prune.footballseason2)

pdf("season2tree.pdf")
plot(prune.footballseason2)
text(prune.footballseason2,all=TRUE,cex=0.5)
graphics.off()

footballseason2prune.train = predict(prune.footballseason2, football, type='class')
table(footballseason2prune.train, football$FTR)

#season 3
football[season=="3",]
matchstatFormulaseason3 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason3 = tree(matchstatFormulaseason3, football[season=="3",])
tree.matchstatseason3
summary(tree.matchstatseason3)
plot(tree.matchstatseason3)
text(tree.matchstatseason3,all=TRUE,cex=0.5)

footballseason3.train = predict(tree.matchstatseason3, football, type='class')
table(footballseason3.train, football$FTR)

cv.footballseason3 = cv.tree(tree.matchstatseason3,FUN=prune.misclass)
names(cv.footballseason3)
cv.footballseason3

prune.footballseason3 = prune.misclass(tree.matchstatseason3,best=5)
summary(prune.footballseason3)
pdf("season3tree.pdf")
plot(prune.footballseason3)
text(prune.footballseason3,all=TRUE,cex=0.5)
graphics.off()
footballseason3prune.train = predict(prune.footballseason3, football, type='class')
table(footballseason3prune.train, football$FTR)

#season 4
football[season=="4",]
matchstatFormulaseason4 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason4 = tree(matchstatFormulaseason4, football[season=="4",])
tree.matchstatseason4
summary(tree.matchstatseason4)
plot(tree.matchstatseason4)
text(tree.matchstatseason4,all=TRUE,cex=0.5)

prune.footballseason4 = prune.misclass(tree.matchstatseason4,best=5)
summary(prune.footballseason4)
pdf("season4tree.pdf")
plot(prune.footballseason4)
text(prune.footballseason4,all=TRUE,cex=0.5)
graphics.off()
footballseason4prune.train = predict(prune.footballseason4, football, type='class')
table(footballseason4prune.train, football$FTR)

#season 5
football[season=="5",]
matchstatFormulaseason5 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason5 = tree(matchstatFormulaseason4, football[season=="5",])
tree.matchstatseason5
summary(tree.matchstatseason5)
plot(tree.matchstatseason5)
text(tree.matchstatseason5,all=TRUE,cex=0.5)

prune.footballseason5 = prune.misclass(tree.matchstatseason5,best=5)
summary(prune.footballseason5)
pdf("season5tree.pdf")
plot(prune.footballseason5)
text(prune.footballseason5,all=TRUE,cex=0.5)
graphics.off()
footballseason5prune.train = predict(prune.footballseason5, football, type='class')
table(footballseason5prune.train, football$FTR)

#season 6
football[season=="6",]
matchstatFormulaseason6 = FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.matchstatseason6 = tree(matchstatFormulaseason6, football[season=="6",])
tree.matchstatseason6
summary(tree.matchstatseason6)
plot(tree.matchstatseason6)
text(tree.matchstatseason6,all=TRUE,cex=0.5)

prune.footballseason6 = prune.misclass(tree.matchstatseason6,best=5)
summary(prune.footballseason6)
pdf("season6tree.pdf")
plot(prune.footballseason6)
text(prune.footballseason6,all=TRUE,cex=0.5)
graphics.off()
footballseason6prune.train = predict(prune.footballseason6, football, type='class')
table(footballseason6prune.train, football$FTR)


season6.test <- predict(prune.footballseason6,test,type='class')
summary(season6.test)
xtable(table(season6.test,test$FTR))

season6.test2 <- predict(prune.footballseason6,test2,type='class')
summary(season6.test2)
xtable(table(season6.test2,test2$FTR))

####################
#Tree for HomeTeam

tapply(FTR,HomeTeam,mean)

###Sat 19
#Everton
football[HomeTeam=="Everton",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Everton = tree(matchstat, football[HomeTeam=="Everton",])
tree.Everton
summary(tree.Everton)
plot(tree.Everton)
text(tree.Everton,all=TRUE,cex=0.5)
prune.Everton = prune.misclass(tree.Everton,best=5)
summary(prune.Everton)
pdf("EvertonHome.pdf")
plot(prune.Everton)
text(prune.Everton,all=TRUE,cex=0.5)
graphics.off()

tree.ArsenalA = tree(matchstat, football[AwayTeam=="Arsenal",])
tree.ArsenalA
prune.ArsenalA = prune.misclass(tree.ArsenalA,best=5)
summary(prune.ArsenalA)
pdf("ArsenalAway.pdf")
plot(prune.ArsenalA)
text(prune.ArsenalA,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(HomeTeam=="Everton",season),mean)
tapply(HST,HomeTeam=="Everton",mean)
tapply(AS,list(HomeTeam=="Everton",season),mean)
tapply(AC,list(HomeTeam=="Everton",season),mean)
tapply(HC,list(HomeTeam=="Everton",season),mean)
tapply(HF,list(HomeTeam=="Everton",season),mean)

tapply(HST,list(AwayTeam=="Arsenal",season),mean)
tapply(HST,AwayTeam=="Arsenal",mean)
tapply(AST,list(AwayTeam=="Arsenal",season),mean)


#Chelsea
football[HomeTeam=="Chelsea",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Chelsea = tree(matchstat, football[HomeTeam=="Chelsea",])
tree.Chelsea
summary(tree.Chelsea)
plot(tree.Chelsea)
text(tree.Chelsea,all=TRUE,cex=0.5)
prune.Chelsea = prune.misclass(tree.Chelsea,best=5)
summary(prune.Chelsea)

pdf("ChelseaHome.pdf")
plot(prune.Chelsea)
text(prune.Chelsea,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(HomeTeam=="Chelsea",season),mean)
tapply(AST,HomeTeam=="Chelsea",mean)
tapply(HST,list(HomeTeam=="Chelsea",season),mean)
tapply(HST,HomeTeam=="Chelsea",mean)
tapply(HF,list(HomeTeam=="Chelsea",season),mean)


tree.WestHamA = tree(matchstat, football[AwayTeam=="West Ham",])
tree.WestHamA
summary(tree.WestHamA)
plot(tree.WestHamA)
text(tree.WestHamA,all=TRUE,cex=0.5)
prune.WestHamA = prune.misclass(tree.WestHamA,best=5)

pdf("WestHamA.pdf")
plot(prune.WestHamA)
text(prune.WestHamA,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(AwayTeam=="West Ham",season),mean)
tapply(HST,AwayTeam=="West Ham",mean)
tapply(AST,list(AwayTeam=="West Ham",season),mean)
tapply(AST,AwayTeam=="West Ham",mean)
tapply(HF,list(AwayTeam=="West Ham",season),mean)
tapply(HF,AwayTeam=="West Ham",mean)
tapply(HC,list(AwayTeam=="West Ham",season),mean)
tapply(HC,AwayTeam=="West Ham",mean)

#Crystal Palace
football[HomeTeam=="Crystal Palace",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.CrystalPalace= tree(matchstat, football[HomeTeam=="Crystal Palace",])
tree.CrystalPalace
summary(tree.CrystalPalace)
plot(tree.CrystalPalace)
text(tree.CrystalPalace,all=TRUE,cex=0.5)
prune.CrystalPalace = prune.misclass(tree.CrystalPalace,best=5)
summary(prune.CrystalPalace)
pdf("CrystalPalace.pdf")
plot(prune.CrystalPalace)
text(prune.CrystalPalace,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(HomeTeam=="Crystal Palace",season),mean)
tapply(HST,HomeTeam=="Crystal Palace",mean)
tapply(AST,list(HomeTeam=="Crystal Palace",season),mean)
tapply(AST,HomeTeam=="Crystal Palace",mean)

tree.LeicesterA= tree(matchstat, football[AwayTeam=="Leicester",])
tree.LeicesterA
summary(tree.LeicesterA)
plot(tree.LeicesterA)
text(tree.LeicesterA,all=TRUE,cex=0.5)
prune.LeicesterA = prune.misclass(tree.LeicesterA,best=5)
summary(prune.LeicesterA)
pdf("LeicesterA.pdf")
plot(prune.LeicesterA)
text(prune.LeicesterA,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(AwayTeam=="Leicester",season),mean)
tapply(HST,AwayTeam=="Leicester",mean)
tapply(HC,list(AwayTeam=="Leicester",season),mean)
tapply(HC,AwayTeam=="Leicester",mean)
tapply(HF,list(AwayTeam=="Leicester",season),mean)
tapply(HF,AwayTeam=="Leicester",mean)
tapply(AF,list(AwayTeam=="Leicester",season),mean)
tapply(AF,AwayTeam=="Leicester",mean)

#Watford
football[HomeTeam=="Watford",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Watford = tree(matchstat, football[HomeTeam=="Watford",])
tree.Watford
summary(tree.Watford)
plot(tree.Watford)
text(tree.Watford,all=TRUE,cex=0.5)
prune.Watford = prune.misclass(tree.Watford,best=5)
summary(prune.Watford)
pdf("watford.pdf")
plot(prune.Watford)
text(prune.Watford,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(HomeTeam=="Watford",season),mean)
tapply(HST,HomeTeam=="Watford",mean)

tree.Stoke = tree(matchstat, football[AwayTeam=="Stoke",])
prune.Stoke = prune.misclass(tree.Stoke,best=5)
pdf("StokeA.pdf")
plot(prune.Stoke)
text(prune.Stoke,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(AwayTeam=="Stoke",season),mean)
tapply(HST,AwayTeam=="Stoke",mean)
tapply(AST,list(AwayTeam=="Stoke",season),mean)
tapply(AST,AwayTeam=="Stoke",mean)

#West Brom
football[HomeTeam=="West Brom",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.WestBrom = tree(matchstat, football[HomeTeam=="West Brom",])
tree.WestBrom
summary(tree.WestBrom)
plot(tree.WestBrom)
text(tree.WestBrom,all=TRUE,cex=0.5)
prune.WestBrom = prune.misclass(tree.WestBrom,best=5)
summary(prune.WestBrom)
pdf("WestBrom.pdf")
plot(prune.WestBrom)
text(prune.WestBrom,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(HomeTeam=="West Brom",season),mean)
tapply(AST,HomeTeam=="West Brom",mean)
tapply(HST,list(HomeTeam=="West Brom",season),mean)
tapply(HST,HomeTeam=="West Brom",mean)
tapply(AC,list(HomeTeam=="West Brom",season),mean)
tapply(AC,HomeTeam=="West Brom",mean)
tapply(HF,list(HomeTeam=="West Brom",season),mean)
tapply(HF,HomeTeam=="West Brom",mean)

tree.NorwichA = tree(matchstat, football[AwayTeam=="Norwich",])
prune.NorwichA = prune.misclass(tree.NorwichA,best=5)
pdf("NorwichA.pdf")
plot(prune.NorwichA)
text(prune.NorwichA,all=TRUE,cex=0.5)
graphics.off()

tapply(AS,list(AwayTeam=="Norwich",season),mean)
tapply(AS,AwayTeam=="Norwich",mean)
tapply(AF,list(AwayTeam=="Norwich",season),mean)
tapply(AF,AwayTeam=="Norwich",mean)
tapply(HS,list(AwayTeam=="Norwich",season),mean)
tapply(HS,AwayTeam=="Norwich",mean)

#Swansea
football[HomeTeam=="Swansea",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Swansea = tree(matchstat, football[HomeTeam=="Swansea",])
tree.Swansea
summary(tree.Swansea)
plot(tree.Swansea)
text(tree.Swansea,all=TRUE,cex=0.5)
prune.Swansea = prune.misclass(tree.Swansea,best=5)
summary(prune.Swansea)
pdf("Swansea.pdf")
plot(prune.Swansea)
text(prune.Swansea,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(HomeTeam=="Swansea",season),mean)
tapply(AST,HomeTeam=="Swansea",mean)
tapply(HST,list(HomeTeam=="Swansea",season),mean)
tapply(HST,HomeTeam=="Swansea",mean)
tapply(HF,list(HomeTeam=="Swansea",season),mean)
tapply(HF,HomeTeam=="Swansea",mean)
tapply(AS,list(HomeTeam=="Swansea",season),mean)
tapply(AS,HomeTeam=="Swansea",mean)
tapply(AC,list(HomeTeam=="Swansea",season),mean)
tapply(AC,HomeTeam=="Swansea",mean)

tree.AstonVillaA = tree(matchstat, football[AwayTeam=="Aston Villa",])
prune.AstonVillaA  = prune.misclass(tree.AstonVillaA,best=5)
pdf("AstonVillaA.pdf")
plot(prune.AstonVillaA)
text(prune.AstonVillaA,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(AwayTeam=="Aston Villa",season),mean)
tapply(HST,AwayTeam=="Aston Villa",mean)
tapply(AST,list(AwayTeam=="Aston Villa",season),mean)
tapply(AST,AwayTeam=="Aston Villa",mean)
tapply(AS,list(AwayTeam=="Aston Villa",season),mean)
tapply(AS,AwayTeam=="Aston Villa",mean)
tapply(AF,list(AwayTeam=="Aston Villa",season),mean)
tapply(AF,AwayTeam=="Aston Villa",mean)

###Sun 20
#Newcastle
football[HomeTeam=="Newcastle",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Newcastle = tree(matchstat, football[HomeTeam=="Newcastle",])
tree.Newcastle
summary(tree.Newcastle)
plot(tree.Newcastle)
text(tree.Newcastle,all=TRUE,cex=0.5)
prune.Newcastle = prune.misclass(tree.Newcastle,best=5)
summary(prune.Newcastle)

pdf("Newcastle.pdf")
plot(prune.Newcastle)
text(prune.Newcastle,all=TRUE,cex=0.5)
graphics.off()

tapply(HF,list(HomeTeam=="Newcastle",season),mean)
tapply(HF,HomeTeam=="Newcastle",mean)
tapply(AST,list(HomeTeam=="Newcastle",season),mean)
tapply(AST,HomeTeam=="Newcastle",mean)
tapply(HST,list(HomeTeam=="Newcastle",season),mean)
tapply(HST,HomeTeam=="Newcastle",mean)

tree.SunderlandA = tree(matchstat, football[AwayTeam=="Sunderland",])
prune.SunderlandA  = prune.misclass(tree.SunderlandA,best=5)
pdf("SunderlandA.pdf")
plot(prune.SunderlandA)
text(prune.SunderlandA,all=TRUE,cex=0.5)
graphics.off()

tapply(HR,list(AwayTeam=="Sunderland",season),mean)
tapply(HR,AwayTeam=="Sunderland",mean)

#Southampton
football[HomeTeam=="Southampton",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Southampton = tree(matchstat, football[HomeTeam=="Southampton",])
tree.Southampton
summary(tree.Southampton)
plot(tree.Southampton)
text(tree.Southampton,all=TRUE,cex=0.5)
prune.Southampton = prune.misclass(tree.Southampton,best=3)
summary(prune.Southampton)
pdf("Southampton.pdf")
plot(prune.Southampton)
text(prune.Southampton,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(HomeTeam=="Southampton",season),mean)
tapply(AST,HomeTeam=="Southampton",mean)
tapply(AS,list(HomeTeam=="Southampton",season),mean)
tapply(AS,HomeTeam=="Southampton",mean)
tapply(HST,list(HomeTeam=="Southampton",season),mean)
tapply(HST,HomeTeam=="Southampton",mean)

tree.LiverpoolA = tree(matchstat, football[AwayTeam=="Liverpool",])
prune.LiverpoolA  = prune.misclass(tree.LiverpoolA,best=5)
pdf("LiverpoolA.pdf")
plot(prune.LiverpoolA)
text(prune.LiverpoolA,all=TRUE,cex=0.5)
graphics.off()

tapply(AS,list(AwayTeam=="Liverpool",season),mean)
tapply(AS,AwayTeam=="Liverpool",mean)
tapply(HC,list(AwayTeam=="Liverpool",season),mean)
tapply(HC,AwayTeam=="Liverpool",mean)
tapply(HST,list(AwayTeam=="Liverpool",season),mean)
tapply(HST,AwayTeam=="Liverpool",mean)

#Man City
football[HomeTeam=="Man City",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.ManCity = tree(matchstat, football[HomeTeam=="Man City",])
tree.ManCity
summary(tree.ManCity)
plot(tree.ManCity)
text(tree.ManCity,all=TRUE,cex=0.5)
prune.ManCity = prune.misclass(tree.ManCity,best=5)
summary(prune.ManCity)
pdf("ManCity.pdf")
plot(prune.ManCity)
text(prune.ManCity,all=TRUE,cex=0.5)
graphics.off()

tapply(HC,list(HomeTeam=="Man City",season),mean)
tapply(HC,HomeTeam=="Man City",mean)
tapply(HF,list(HomeTeam=="Man City",season),mean)
tapply(HF,HomeTeam=="Man City",mean)

tree.ManUtdA = tree(matchstat, football[AwayTeam=="Man United",])
prune.ManUtdA  = prune.misclass(tree.ManUtdA,best=5)
pdf("ManUtdA.pdf")
plot(prune.ManUtdA)
text(prune.ManUtdA,all=TRUE,cex=0.5)
graphics.off()

tapply(AF,list(AwayTeam=="Man United",season),mean)
tapply(AF,AwayTeam=="Man United",mean)
tapply(HC,list(AwayTeam=="Man United",season),mean)
tapply(HC,AwayTeam=="Man United",mean)

#Tottenham
football[HomeTeam=="Tottenham",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Tottenham = tree(matchstat, football[HomeTeam=="Tottenham",])
tree.Tottenham
summary(tree.Tottenham)
plot(tree.Tottenham)
text(tree.Tottenham,all=TRUE,cex=0.5)
prune.Tottenham = prune.misclass(tree.Tottenham,best=3)
summary(prune.Tottenham)
pdf("Tottenham.pdf")
plot(prune.Tottenham)
text(prune.Tottenham,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(HomeTeam=="Tottenham",season),mean)
tapply(HST,HomeTeam=="Tottenham",mean)
tapply(HC,list(HomeTeam=="Tottenham",season),mean)
tapply(HC,HomeTeam=="Tottenham",mean)

tree.BournemouthA = tree(matchstat, football[AwayTeam=="Bournemouth",])
prune.BournemouthA  = prune.misclass(tree.BournemouthA,best=5)
pdf("BournemouthA.pdf")
plot(prune.BournemouthA)
text(prune.BournemouthA,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(AwayTeam=="Bournemouth",season),mean)
tapply(AF,AwayTeam=="Bournemouth",mean)

###Sat 2
#Arsenal
football[HomeTeam=="Arsenal",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Arsenal = tree(matchstat, football[HomeTeam=="Arsenal",])
tree.Arsenal
summary(tree.Arsenal)
plot(tree.Arsenal)
text(tree.Arsenal,all=TRUE,cex=0.5)
prune.Arsenal = prune.misclass(tree.Arsenal,best=5)
summary(prune.Arsenal)
pdf("Arsenal.pdf")
plot(prune.Arsenal)
text(prune.Arsenal,all=TRUE,cex=0.5)
graphics.off()

tapply(HF,list(HomeTeam=="Arsenal",season),mean)
tapply(HF,HomeTeam=="Arsenal",mean)
tapply(AS,list(AwayTeam=="Watford",season),mean)
tapply(AS,AwayTeam=="Watford",mean)
tapply(AY,list(AwayTeam=="Watford",season),mean)
tapply(AY,AwayTeam=="Watford",mean)

#AstonVilla
football[HomeTeam=="Aston Villa",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.AstonVilla = tree(matchstat, football[HomeTeam=="Aston Villa",])
tree.AstonVilla
summary(tree.AstonVilla)
plot(tree.AstonVilla)
text(tree.AstonVilla,all=TRUE,cex=0.5)
prune.AstonVilla = prune.misclass(tree.AstonVilla,best=5)
summary(prune.AstonVilla)
pdf("AstonVilla.pdf")
plot(prune.AstonVilla)
text(prune.AstonVilla,all=TRUE,cex=0.5)
graphics.off()

tapply(HST,list(HomeTeam=="Aston Villa",season),mean)
tapply(HST,HomeTeam=="Aston Villa",mean)
tapply(AF,list(AwayTeam=="Chelsea",season),mean)
tapply(AF,AwayTeam=="Chelsea",mean)
tapply(AS,list(AwayTeam=="Chelsea",season),mean)
tapply(AS,AwayTeam=="Chelsea",mean)


#Bournemouth
football[HomeTeam=="Bournemouth",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Bournemouth = tree(matchstat, football[HomeTeam=="Bournemouth",])
tree.Bournemouth
summary(tree.Bournemouth)
plot(tree.Bournemouth)
text(tree.Bournemouth,all=TRUE,cex=0.5)
prune.Bournemouth = prune.misclass(tree.Bournemouth,best=4)
summary(prune.Bournemouth)
pdf("Bournemouth.pdf")
plot(prune.Bournemouth)
text(prune.Bournemouth,all=TRUE,cex=0.5)
graphics.off()

tapply(AS,list(AwayTeam=="Man City",season),mean)
tapply(AS,AwayTeam=="Man City",mean)
tapply(AF,list(AwayTeam=="Man City",season),mean)
tapply(AF,AwayTeam=="Man City",mean)

#Norwich
football[HomeTeam=="Norwich",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Norwich = tree(matchstat, football[HomeTeam=="Norwich",])
tree.Norwich
summary(tree.Norwich)
plot(tree.Norwich)
text(tree.Norwich,all=TRUE,cex=0.5)
prune.Norwich = prune.misclass(tree.Norwich,best=4)
summary(prune.Norwich)
pdf("Norwich.pdf")
plot(prune.Norwich)
text(prune.Norwich,all=TRUE,cex=0.5)

tapply(AS,list(AwayTeam=="Newcastle",season),mean)
tapply(AS,AwayTeam=="Newcastle",mean)
tapply(HST,list(HomeTeam=="Norwich",season),mean)
tapply(HST,HomeTeam=="Norwich",mean)
tapply(AC,list(AwayTeam=="Newcastle",season),mean)
tapply(AC,AwayTeam=="Newcastle",mean)
tapply(AST,list(AwayTeam=="Newcastle",season),mean)
tapply(AST,AwayTeam=="Newcastle",mean)
tapply(AF,list(AwayTeam=="Newcastle",season),mean)
tapply(AF,AwayTeam=="Newcastle",mean)

#Stoke
football[HomeTeam=="Stoke",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.StokeHome = tree(matchstat, football[HomeTeam=="Stoke",])
tree.StokeHome
summary(tree.StokeHome)
plot(tree.StokeHome)
text(tree.StokeHome,all=TRUE,cex=0.5)
prune.StokeHome = prune.misclass(tree.StokeHome,best=5)
summary(prune.StokeHome)
pdf("Stoke.pdf")
plot(prune.StokeHome)
text(prune.StokeHome,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(AwayTeam=="Swansea",season),mean)
tapply(AST,AwayTeam=="Swansea",mean)
tapply(HR,list(HomeTeam=="Stoke",season),mean)
tapply(HR,HomeTeam=="Stoke",mean)
tapply(AS,list(AwayTeam=="Swansea",season),mean)
tapply(AS,AwayTeam=="Swansea",mean)
tapply(HST,list(HomeTeam=="Stoke",season),mean)
tapply(HST,HomeTeam=="Stoke",mean)
tapply(HF,list(HomeTeam=="Stoke",season),mean)
tapply(HF,HomeTeam=="Stoke",mean)

#Sunderland
football[HomeTeam=="Sunderland",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Sunderland = tree(matchstat, football[HomeTeam=="Sunderland",])
tree.Sunderland
summary(tree.Sunderland)
plot(tree.Sunderland)
text(tree.Sunderland,all=TRUE,cex=0.5)
prune.Sunderland = prune.misclass(tree.Sunderland,best=5)
summary(prune.Sunderland)
pdf("Sunderland.pdf")
plot(prune.Sunderland)
text(prune.Sunderland,all=TRUE,cex=0.5)
graphics.off()

tapply(AS,list(AwayTeam=="West Brom",season),mean)
tapply(AS,AwayTeam=="West Brom",mean)



#West Ham
football[HomeTeam=="West Ham",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.WestHam = tree(matchstat, football[HomeTeam=="West Ham",])
tree.WestHam
summary(tree.WestHam)
plot(tree.WestHam)
text(tree.WestHam,all=TRUE,cex=0.5)
prune.WestHam = prune.misclass(tree.WestHam,best=3)
summary(prune.WestHam)
pdf("WestHam.pdf")
plot(prune.WestHam)
text(prune.WestHam,all=TRUE,cex=0.5)
graphics.off()

tapply(AST,list(AwayTeam=="Crystal Palace",season),mean)
tapply(AST,AwayTeam=="Crystal Palace",mean)

#Liverpool
football[HomeTeam=="Liverpool",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Liverpool = tree(matchstat, football[HomeTeam=="Liverpool",])
tree.Liverpool
summary(tree.Liverpool)
plot(tree.Liverpool)
text(tree.Liverpool,all=TRUE,cex=0.5)
prune.Liverpool = prune.misclass(tree.Liverpool,best=5)
summary(prune.Liverpool)
pdf("Liverpool.pdf")
plot(prune.Liverpool)
text(prune.Liverpool,all=TRUE,cex=0.5)
graphics.off()
tapply(HST,list(HomeTeam=="Liverpool",season),mean)
tapply(HST,HomeTeam=="Liverpool",mean)
tapply(HC,list(HomeTeam=="Liverpool",season),mean)
tapply(HC,HomeTeam=="Liverpool",mean)
tapply(HS,list(HomeTeam=="Liverpool",season),mean)
tapply(HS,HomeTeam=="Liverpool",mean)

###Sun 3
#Leicester
football[HomeTeam=="Leicester",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.Leicester = tree(matchstat, football[HomeTeam=="Leicester",])
tree.Leicester
summary(tree.Leicester)
plot(tree.Leicester)
text(tree.Leicester,all=TRUE,cex=0.5)
prune.Leicester = prune.misclass(tree.Leicester,best=3)
summary(prune.Leicester)
pdf("Leicester.pdf")
plot(prune.Leicester)
text(prune.Leicester,all=TRUE,cex=0.5)
graphics.off()

tapply(AC,list(AwayTeam=="Southampton",season),mean)
tapply(AC,AwayTeam=="Southampton",mean)
tapply(HC,list(HomeTeam=="Leicester",season),mean)
tapply(HC,HomeTeam=="Leicester",mean)
#Man Utd
football[HomeTeam=="Man United",]
matchstat= FTR ~ HS + AS + HST + AST+ AC + HC + HF + AF + HY + AY + HR + AR
tree.ManUtd = tree(matchstat, football[HomeTeam=="Man United",])
tree.ManUtd
summary(tree.ManUtd)
plot(tree.ManUtd)
text(tree.ManUtd,all=TRUE,cex=0.5)
prune.ManUtd = prune.misclass(tree.ManUtd,best=3)
summary(prune.ManUtd)
pdf("manutd.pdf")
plot(prune.ManUtd)
text(prune.ManUtd,all=TRUE,cex=0.5)
graphics.off()
tapply(HST,list(HomeTeam=="Man United",season),mean)
tapply(HST,HomeTeam=="Man United",mean)
tapply(AF,list(AwayTeam=="Everton",season),mean)
tapply(AF,AwayTeam=="Everton",mean)

####################
library(RWeka)
help(J48)
footballJ48 = J48(FTR~., data=football[,12:23], control=Weka_control(C=0.0001))
summary(footballJ48)
footballJ48
install.packages("partykit")
library(partykit)
pdf("footballJ48.pdf")
plot(footballJ48)
graphics.off()
eval_footballj48 <- evaluate_Weka_classifier(footballJ48, numFolds = 10, complexity = FALSE, seed = 1, class = TRUE)
eval_footballj48
eval_footballj48test = evaluate_Weka_classifier(footballJ48, newdata=test, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
eval_footballj48test

test3 <- test2[2:23]
test3 <- test3[-1]
test3 <- test3[-3]
test3 <- test3[-3]
test3 <- test3[-4]


eval_footballj48test2 = evaluate_Weka_classifier(footballJ48, newdata=test3, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
eval_footballj48test2

#Just using FTHG
footballJ48FTHG = J48(FTR~., data=football[,5:6], control=Weka_control(M=5))
summary(footballJ48FTHG)
footballJ48FTHG
plot(footballJ48FTHG)

#Just using Odds
footballJ48Odds = J48(FTR~., data=football[,24:50], control=Weka_control(C=0.005))
summary(footballJ48Odds)
footballJ48Odds
plot(footballJ48FTHG)




footballJ48Oddstest2 = evaluate_Weka_classifier(footballJ48Odds, newdata=test4, numFolds = 0, complexity = FALSE, seed = 1, class = TRUE)
footballJ48Oddstest2

#Just using BetBrain
which(colnames(football)=="BbMxH")
which(colnames(football)=="BbAvA")
footballJ48BetBrain = J48(FTR~., data=football[,55:60], control=Weka_control(C=0.005))
summary(footballJ48BetBrain)
footballJ48BetBrain
plot(footballJ48BetBrain)

#Just using Half Time goals
which(colnames(football)=="HTHG")
footballJ48HG = J48(FTR~., data=football[,8:9], control=Weka_control(C=0.005))
summary(footballJ48HG)
footballJ48HG
plot(footballJ48HG)

#Just using Half Time goals
which(colnames(football)=="HS")
footballJ48Score = J48(FTR~., data=football[,12:15], control=Weka_control(C=0.005))
summary(footballJ48Score)
footballJ48Score
plot(footballJ48Score)

??RWeka
####################
library(stats)
football1  = football
football1$FTR = NULL

#HST, AST, HS, AS
kmeans.result1114 = kmeans(football1[,11:14],3, nstart=200)
kmeans.result1114
xtable(table(football$FTR, kmeans.result1114$cluster))
pdf("HSTHS.pdf")
plot(football1[c("HST", "HS")], col = kmeans.result1114$cluster)
points(kmeans.result1114$centers[,c("HST","HS")], col = 1:3,pch = 8, cex=2)
graphics.off()
pdf("ASTAS.pdf")
plot(football1[c("AST", "AS")], col = kmeans.result1114$cluster)
points(kmeans.result1114$centers[,c("AST","AS")], col = 1:3,pch = 8, cex=2)
graphics.off()

test5 <- test2
test5$FTR = NULL
kmeans.result1114test = kmeans(test5[,11:14],3, nstart=20)
kmeans.result1114test
xtable(table(test2$FTR, kmeans.result1114test$cluster))
pdf("HSTHStest.pdf")
plot(test2[c("HST", "HS")], col = kmeans.result1114test$cluster)
points(kmeans.result1114test$centers[,c("HST","HS")], col = 1:3,pch = 8, cex=2)
graphics.off()

pdf("ASTAStest.pdf")
plot(test2[c("AST", "AS")], col = kmeans.result1114test$cluster)
points(kmeans.result1114test$centers[,c("AST","AS")], col = 1:3,pch = 8, cex=2)
graphics.off()

library(cluster)
pam.result = pam(football1[,11:14],3)
pam.result
xtable(table(pam.result$clustering,football$FTR))
pdf("kmedoid.pdf")
plot(pam.result)
graphics.off()
pam.resulttest = pam(test5[,11:14],3)
pam.resulttest
xtable(table(pam.resulttest$clustering,test2$FTR))
pdf("kmedoidtest.pdf")
plot(pam.resulttest)
graphics.off()

pam.result1 = pam(football1[,11:22],3)
pam.result1
xtable(table(pam.result1$clustering,football$FTR))
pdf("kmedoidstat.pdf")
plot(pam.result1)
graphics.off()
pam.resulttest1 = pam(test5[,11:22],3)
pam.resulttest1
xtable(table(pam.resulttest1$clustering,test2$FTR))
pdf("kmedoidstattest.pdf")
plot(pam.result1)
graphics.off()

#Fouls
kmeans.resultFouls = kmeans(football1[,15:16],3, nstart=20)
kmeans.resultFouls
xtable(table(football$FTR, kmeans.resultFouls$cluster))

pdf("foulkmedoid.pdf")
plot(football1[c("HF", "AF")], col = kmeans.resultFouls$cluster)
points(kmeans.resultFouls$centers[,c("HF","AF")], col = 1:3,pch = 8, cex=2)
graphics.off()
kmeans.resultFoulstest = kmeans(test5[,15:16],3, nstart=20)
kmeans.resultFoulstest
xtable(table(test2$FTR, kmeans.resultFoulstest$cluster))
pdf("foulkmedoidtest.pdf")
plot(test2[c("HF", "AF")], col = kmeans.resultFoulstest$cluster)
points(kmeans.resultFoulstest$centers[,c("HF","AF")], col = 1:3,pch = 8, cex=2)
graphics.off()
#Corners
kmeans.resultcorners = kmeans(football1[,17:18],3, nstart=20)
kmeans.resultcorners
xtable(table(football$FTR, kmeans.resultcorners$cluster))

pdf("cornerkmeans.pdf")
plot(football1[c("HC", "AC")], col = kmeans.resultcorners$cluster)
points(kmeans.resultcorners$centers[,c("HC","AC")], col = 1:3,pch = 8, cex=2)
graphics.off()
kmeans.resultcornerstest = kmeans(test5[,17:18],3, nstart=20)
kmeans.resultcornerstest
xtable(table(test2$FTR, kmeans.resultcornerstest$cluster))
pdf("cornerkmeanstest.pdf")
plot(test2[c("HC", "AC")], col = kmeans.resultcornerstest$cluster)
points(kmeans.resultcornerstest$centers[,c("HC","AC")], col = 1:3,pch = 8, cex=2)
graphics.off()

help("SimpleKMeans")
simplekmeans1 <- SimpleKMeans(football1[,17:18],Weka_control(N=3))
simplekmeans1
table(predict(simplekmeans1), football$FTR)

simplekmeansGoals <- SimpleKMeans(football1[,5:6],Weka_control(N=3))
simplekmeansGoals
table(predict(simplekmeansGoals), football$FTR)

simplekmeansOdds <- SimpleKMeans(football1[,24:26],Weka_control(N=3))
simplekmeansOdds
table(predict(simplekmeansOdds), football$FTR)

simplekmeansBb <- SimpleKMeans(football1[,54:59],Weka_control(N=3))
simplekmeansBb
table(predict(simplekmeansBb), football$FTR)

####################
#Hierarchical
idx <- sample(1:dim(football)[1], 50) #sample with 300 observations
footballSample <- football[idx,]
footballSample$FTR <- NULL
hc <- hclust(dist(footballSample), method="ave")
plot(hc, hang = -1, labels=football$FTR[idx])
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)
groups
table(hc,football$FTR)

#Density Based
library(fpc)
football2 <- football[-1:-11] # remove class tags
which(colnames(football)=="season")
football2 <- football2[-13:-65] # remove class tags

ds <- dbscan(football2, eps=0.001, MinPts=20)
# compare clusters with original class labels
table(ds$cluster, football$FTR)
plot(ds, football2)
plot(ds, football2[c(1,4)])
plotcluster(football2, ds$cluster)
####################
countH <- 0
countD <- 0
countA <- 0
for (val in FTR[AwayTeam=="Chelsea"])
{if(val == "H") {countH=countH+1} 
  else if(val == "D"){countD=countD+1}
  else if(val == "A"){countA=countA+1}}
#####################


