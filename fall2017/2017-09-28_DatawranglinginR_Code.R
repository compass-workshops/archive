#### p.27
rm(list=ls())
## Delete your workspace
getwd()
## Check your current working directory
setwd("<location of your dataset>")
## Set your working directory
poll = read.csv("09262016.csv")
## Load data

#### p.29
dfex <- data.frame(mode = c("phone","Internet","Internet"), DVotes = c(40,50,60), RVotes = c(60,50,40))
dfex

#### p.31
View(poll)
## Spreadsheet-style data viewer
summary(poll)
## Summarize variables on your console 
names(poll)
## Names of all variables
dim(poll)
nrow(poll)
ncol(poll)
## Dimensional information 
head(poll)
tail(poll)

#### p.32
poll$Affiliation

#### p.34
dfex[c(1,3),]

#### p.35
dfex[,c(2,3)]
dfex[,c("DVotes","RVotes")]

#### p.36
dfex[c(1,3),c(2,3)]
dfex[c(1,3),c("DVotes","RVotes")]

#### p.39
poll$Affiliation == "Dem" 
poll$Affiliation != "Dem" 
poll$Clinton > 50 
poll$Clinton >= 40 & poll$Clinton <= 60 
poll$Affiliation == "Dem" | poll$Clinton > 50 

#### p.44
test1 <- poll[poll$Clinton >= 40 & poll$Clinton <= 60,]
dim(test1)
summary(test1$Clinton)

#### p.45 - 46
test1 <- subset(poll,Clinton >= 40 & poll$Clinton <= 60, 
select=c(Pollster, Clinton))
summary(test1$Clinton)
dim(test1)
names(test1)

test2 <- subset(poll,Clinton >= 40 & poll$Clinton <= 60,names(poll)!="Trump")
summary(test2$Clinton)
dim(test2)
names(test2)

### p.47
dfex1 <- data.frame(ID = c(1,2,3), DVotes = c(40,50,60), RVotes = c(60,50,40))
dfex1

dfex2 <- data.frame(ID = c(1,3,2), mode = c("phone","Internet","Internet"))
dfex2

dfex.total <- merge(dfex1,dfex2,by="ID")
dfex.total

### p.48
save(poll, file="pollonly.Rdata")
dir()

save.image("everything.RData")

### p.50
summary(poll$Population)
poll_rep <- subset(poll, Population=="Likely Voters - Republican") 
poll_dem <- subset(poll, Population=="Likely Voters - Democrat")
par(mfrow=c(1,2))
## 1 by 2 subplots
plot(as.Date(poll_rep$End.Date),poll_rep$Clinton, col = "red")
plot(as.Date(poll_dem$End.Date),poll_dem$Clinton, col = "blue")
## as.Date: Date operator for date variables

### p.51
TrumpWin <- (poll$Clinton < poll$Trump)
## Create an indicator variable for win/lose status
poll$TrumpWin <- TrumpWin
## Add the created variable to poll data frame
names(poll)[names(poll) == "TrumpWin"] <- "TW"
## Rename variable

### p.52
summary(poll$Population)
poll_rep <- subset(poll, Population=="Likely Voters - Republican") 
poll_dem <- subset(poll, Population=="Likely Voters - Democrat")
par(mfrow=c(1,2))
## 1 by 2 subplots
plot(as.Date(poll_rep$End.Date),poll_rep$Trump, col = "red")
## as.Date: Date operator for date variables
plot(as.Date(poll_dem$End.Date),poll_dem$Trump, col = "blue")

### p.53
summary(poll$Affiliation)
summary(poll$Population)
poll.rep <- subset(poll, Affiliation=="Rep" & Population=="Likely Voters") 
poll.dem <- subset(poll, Affiliation=="Dem" & Population=="Likely Voters") 
poll.none <- subset(poll, Affiliation!="Rep" & Affiliation!="Dem" & Population=="Likely Voters")
par(mfrow=c(1,3))
## 1 by 3 subplots
plot(as.Date(poll.rep$End.Date),poll.rep$Trump, col = "red")
plot(as.Date(poll.dem$End.Date),poll.dem$Trump, col = "blue")
plot(as.Date(poll.none$End.Date),poll.none$Trump, col = "green")
