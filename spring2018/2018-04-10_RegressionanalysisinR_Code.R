load("countries.RData")
View(countries)
attach(countries)

fit <- lm(LifeExpectancy ~ Developed)
summary(fit)

R <- 3000 #number of repetitions
n <- 2000 #number of observations
beta <- 1 #true coefficient
boutcome <- rep(0,R) #beta estimates
bcummean <- rep(0,R) #beta cumulative mean estimates

for(r in c(1:R)){
X <- rnorm(n) #generate X
Y <- beta * X + rnorm(n, mean = 0, sd = 0.01) #generate Y
fit <- lm(Y ~ X) #run regression
boutcome[r] <- summary(fit)$coefficients[2] #beta estimates
bcummean[r] <- mean(boutcome[1:r]) #beta cumulative mean estimates
}

plot(bcummean,xlab="Repetition",ylab="beta hat")
lines(c(1,R),c(1,1),col='red')

fit <- lm(LifeExpectancy ~ Developed)
summary(fit)
coefficients(fit)
confint(fit, level=0.95)

fit <- lm(LifeExpectancy ~ Health)
summary(fit)
confint(fit, level=0.95)

fit <- lm(LifeExpectancy ~  LandArea)
summary(fit)
confint(fit, level=0.95)

fit <- lm(LifeExpectancy ~ Health)
fitted(fit)
plot(Health,LifeExpectancy)
lines(Health,fitted(fit), col="red")

plot(Health,LifeExpectancy)
abline(fit, col="red")

install.packages('visreg')
require(visreg)

fit <- lm(LifeExpectancy ~ Developed)
visreg(fit)

fit <- lm(LifeExpectancy ~ Health)
visreg(fit)

fit <- lm(LifeExpectancy ~  LandArea)
visreg(fit)
