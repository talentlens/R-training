#R training day 2

setwd("C:/Users/morgstro/Desktop/R Training")

# Stats recap ---------------------------------------------------------

#Normal distribution functions

#Density function (i.e. PDF, probability density function)
dnorm(x = 0, mean = 0, sd = 1) # f(z = 0) = exp(-(1/2) * 0^2) / sqrt(2 * pi)
#Plot the density function
curve(dnorm, from = -3, to = 3)

#Distribution function (i.e. CDF, cumulative distribution function)
pnorm(q = 0, mean = 0, sd = 1) # Pr(z <= 0)
curve(pnorm, from = -3, to = 3)

#Quantile function (inverse cumulative distribution function)
qnorm(p = 0.5, mean = 0, sd = 1) # F^-1(p = 0.5)
curve(qnorm, from = 0, to = 1)

#Random number generation
x <- rnorm(n = 100, mean = 0, sd = 1) # X ~ N(0, 1)
hist(x)


#Other distribution functions
?dt
?dchisq
?df
?dunif
?dbeta
?dexp
?dlnorm
?dbinom
?dpois


#Statistical inference ------------------------------------------------

mean(x) #sample mean
var(x) #sample variance
sd(x) #sample standard deviation
sem <- sd(x) / sqrt(length(x)) #sample SEM
mean(x) - 1.96 * sem #95% CI lower bound
mean(x) + 1.96 * sem #95% CI upper bound

mean(x, na.rm = TRUE) #if you have missing data that you want to ignore

#Other useful statistics
median(x)
quantile(x)
quantile(x)
max(x)
min(x)


#Correlation example

#Sample two vectors from independent distributions
x <- rnorm(100, mean=50, sd=20)
y <- rnorm(100, mean=5.5, sd=2)

cor.test(x, y, method = "pearson") # continuous data
plot(x, y, pch = 20, col = "steelblue")


#Regression example

reg1 <- lm(y ~ x)
summary(reg1)
confint(reg1)

abline(reg1, col = "black", lty = 2)


#Exercise: Athena study
library(dplyr)
athena <- read.csv("athena.csv")
cubiks <- read.csv("cubiks.csv")
study <- left_join(cubiks, athena, by = c("ID" = "ID", "Group" = "Group"))

#Calculate correlation
cor(study$Athena.Raw, study$Cubiks.Raw)
#Bivariate scatterplot
plot(study$Athena.Raw, study$Cubiks.Raw, pch = 20, col = "steelblue")
#Add simple linear regression model to the plot
reg2 <- lm(Cubiks.Raw ~ Athena.Raw, study)
summary(reg2)
abline(reg2, lty = 2, col = "black")



#Significance testing --------------------------------------------------

#Using Student's t-distribution, 
#this is the p-value for the slope of Athena.Raw
1.1308/0.1294
2 * pt(q = 8.739, df = 87, lower.tail = FALSE)
summary(reg2)

#Case: two groups t-test
g <- gl(n = 2, k = 50) #Generate 2 groups of size 50

t.test(formula = x ~ g) 
#t.test(x ~ g, paired = TRUE)

boxplot(x ~ g) # Box plot of x by group g
by(x, g, mean) # Means for the two groups

#Are t-tests a linear model??
summary(lm(x ~ g))

#Graph for the linear model
plot(as.integer(g) - 1, x, xlab = "g", ylab = "x",
     pch = 20, col = "steelblue", lab = c(1, 5, 1))
abline(lm(x ~ g), lty = 2)


#Case: ANOVA for 3 or more groups
g <- gl(n = 4, k = 25) #Generate 4 groups of size 25

aov1 <- aov(x ~ g)
summary(aov1)
boxplot(x ~ g) # Box plot of x by g
by(x, g, mean) #Group wise means

#Model coefficients
summary.lm(aov1)
#What's up with this??
#Anova = linear model!

#Graph with points
plot(as.integer(g), x, xlab = "g", ylab = "x",
     pch = 20, col = "steelblue", lab = c(3, 5, 1))
m <- by(x, g, mean)
lines(1:4, m, lty = 2)
points(1:4, m, pch = 20, col = "red", cex = 2)

#This is how the group factor is represented (dummy variables)
contrasts(g) #Group membership coded as 1. 
#Group 1 is the intercept/baseline that the other groups are compared to 

#Follow up test: Tukey's Honest Significant Differences
TukeyHSD(aov1)


#Exercise: Athena study
cor.test(study$Athena.Raw, study$Cubiks.Raw)
cor.test(study$Athena.Raw, study$Athena.Time)
aov2 <- aov(Athena.Raw ~ Group, data = study)
summary(aov2)
boxplot(Athena.Raw ~ Group, data = study, las = 2)


#Statistical prediction ------------------------------------------------

#What's the most likely score on Cubiks, given the score of 20 on Athena?
1.3968 + 1.1308 * 20

#Using the predict function
new_data <- data.frame(Athena.Raw = 20)
predict(reg2, newdata = new_data)
predict(reg2, newdata = new_data, interval = "prediction")
predict(reg2, newdata = new_data, interval = "confidence") #Not the same thing!
#Confidence intervals are narrower - the model is optimized for the observed data
#Prediction intervals are wider - reflecting the fact that the model hasn't been 
#optimized using the new data

#Plot predictions for a range of Athena scores [0 20]
athena_range <- seq(0, 20, 0.2)
new_data <- data.frame(Athena.Raw = athena_range)
pred_ci <- predict(reg2, new_data, interval = "confidence")
pred_pi <- predict(reg2, new_data, interval = "prediction")
plot(new_data$Athena.Raw, pred_ci[, "fit"], type = "l",
     main = "Statistical Prediction",
     xlab = "Observed Athena Score", ylab = "Predicted Cubiks Score")
lines(new_data$Athena.Raw, pred_ci[, "lwr"], lty = 2)
lines(new_data$Athena.Raw, pred_ci[, "upr"], lty = 2)
lines(new_data$Athena.Raw, pred_pi[, "lwr"], lty = 3)
lines(new_data$Athena.Raw, pred_pi[, "upr"], lty = 3)
legend("bottomright", legend = c("Prediction", "95% CI", "95% PI"), lty = 1:3)

#Future -------------------------------------------------------

install.packages("devtools")
devtools::install_github("talentlens/talentlens")

#Bonus --------------------------------------------------------

#Bootstrapped sampling distribution of the regression slope

boot_beta <- numeric(9999)
for (i in 1:9999) {
  boot_sample <- sample(1:nrow(study), replace = TRUE)
  x_boot <- study[boot_sample, "Athena.Raw"]
  y_boot <- study[boot_sample, "Cubiks.Raw"]
  boot_beta[i] <- cor(x_boot, y_boot) * sd(y_boot) / sd(x_boot)
}

#Plot sampling distribution
hist(boot_beta)
#Calculate mean = estimated value of the regression slope
mean(boot_beta)
#Calculate sd = standard error of the estimation
sd(boot_beta)
#Calculate confidence intervals
quantile(boot_beta, probs = c(0.025, 0.975))
