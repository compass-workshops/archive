rm(list=ls());
## Delete your workspace;
getwd();
## Check your current working directory
setwd("<location of your dataset>");
## Set your working directory;
polls = read.csv("2016_StatePolls.csv");
## Load data;

summary(polls$other);
summary(polls$undecided);
polls$etc<-100-polls$trump-polls$clinton

table(polls$State);
barplot(table(polls$State),main="Unordered");
## simple bar plot (Q: what is the order in the x values?);

barplot(table(polls$State),main="Unordered",ylab="Poll freq.");
## simple bar plot (Q: what is the order in the x values?)
polls_r <- transform(polls,State = reorder(State, trump));
## reorder states by Trump support rate
barplot(table(polls_r$State),main="Ordered by %Trump", ylab="Poll freq.");
## ordered plot!

par(mfrow=c(2,1))
barplot(table(polls$State),main="Simple Bar Plot");
barplot(table(polls_r$State),main="Ordered by %Trump");
## with subplot function

install.packages("Lock5Data"); 
## Install package Lock5Data which contains the Hollywood dataset;
library("Lock5Data"); 
data(HollywoodMovies2011);
## Load data;
movies<- na.omit(HollywoodMovies2011);
## drop all observations with at least one NA

hist(movies$RottenTomatoes, breaks=10, col="red", xlab="Rating", main="Colored histogram with 10 bins"); 

par(mfrow=c(1,2));
## 1 by 2 subplots;
plot(movies$RottenTomatoes,log10(movies$WorldGross));
plot(log10(movies$Budget),log10(movies$WorldGross));
## log10: logarithm function with base 10;

par(mfrow=c(1,2));
## 1 by 2 subplots;
plot(movies$RottenTomatoes,log10(movies$WorldGross));
plot(log10(movies$Budget),log10(movies$WorldGross));
## log10: logarithm function with base 10;

par(mfrow=c(1,2));
## 1 by 2 subplots;
plot(movies$RottenTomatoes,log10(movies$WorldGross),col=movies$Genre);
plot(log10(movies$Budget),log10(movies$WorldGross),col=movies$Genre);
## log10: logarithm function with base 10;

dev.off()

mod1 <- lm(log10(movies$WorldGross) ~ movies$RottenTomatoes); 
## Linear regression
preds1 <- predict(mod1); 
## predicted value obtained by linear regression
plot(movies$RottenTomatoes,log10(movies$WorldGross));
lines(movies$RottenTomatoes, preds1);

mod2 <- lm(log10(movies$WorldGross) ~ log10(movies$Budget)); 
## Linear regression
preds2 <- predict(mod2); 
## predicted value obtained by linear regression
plot(log10(movies$Budget),log10(movies$WorldGross));
lines(log10(movies$Budget), preds2);

par(las=2);
## horizontal text;
par(mfrow=c(1,2));
boxplot(movies$RottenTomatoes~movies$Genre,xlab="Genre",ylab="Rating");
## Genre VS Rating;
boxplot(movies$Budget~movies$Genre,xlab="Genre",ylab="Budget");
## Genre VS Budget;

dev.off()

R.Drama <- density(movies$Budget[which(movies$Genre=="Drama")]);
R.Ani <- density(movies$Budget[which(movies$Genre=="Animation")]); 
plot(R.Drama, col="green4",lwd=2,main="Drama VS Animation",xlab="Budget",xlim=c(-10,250));
lines(R.Ani,col="orange",lwd=2,lty=2);
## overlay lines over plot

R.Drama <- density(movies$Budget[which(movies$Genre=="Drama")]);
R.Ani <- density(movies$Budget[which(movies$Genre=="Animation")]); 
plot(R.Drama, col="green4",lwd=2,main="Drama VS Animation",xlab="Budget",xlim=c(-10,250));
lines(R.Ani,col="orange",lwd=2,lty=2);
## overlay lines over plot
text(c(0,200),c(0.025,0.007),c("Drama","Animation"),col=c("green4","orange"));
## add line labels

pdf("dens_plot.pdf");
plot(R.Drama, col="green4",lwd=2,main="Drama VS Animation", xlab="Budget",xlim=c(-10,250));
lines(R.Ani,col="orange",lwd=2,lty=2);
text(c(0,200),c(0.025,0.007),c("Drama","Animation"),col=c("green4","orange"));
dev.off()





