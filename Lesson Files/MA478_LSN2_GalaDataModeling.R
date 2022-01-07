library(faraway)
library(tidyverse)
library(reshape2)

# Load the data

data(gala)
head(gala)
help(gala)

# Research Question: Is there a relationship between Species and Elevation?

gala_new <- gala[,-2] #remove the 'Endemics" column

gala_new %>%
  group_by(Elevation)

# Plot them

qplot(gala$Elevation, gala$Species)

qplot(gala$Species, geom = "boxplot")

# Look at the distribution of the data
qplot(gala$Species,
      geom = "histogram",
      main = "Histogram for Species",
      xlab = "Species")

qplot(gala$Species,
      geom = "density",
      main = "Estimated Density for Species",
      xlab = "Species")

# Does it look normal?
summary(gala$Species)

# While it doesn't look normal, let's keep moving forward
# Let's take a look at the predictors
summary(gala_new[,-1])

# What kind of predictors do we have here (a visual representation)?
ggplot(data = melt(gala_new[,-1]), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill=variable))

# Look at the relationship between all of them
# How do they relate to each other?
pairs(gala_new)

# Build a linear model

lmod <- lm(Species ~ ., data = gala_new)
summary(lmod)

# Update the model to include only Elevation and Area of Adjacent Island

lmod_new <- lm(Species ~ Adjacent + Elevation, data = gala_new)

# Let's look at the regression output (i.e., the coefficients)
summary(lmod_new)

# 95% confidence interval for mean values of Adjacent and Elevation

predict(lmod_new, data.frame(Adjacent = mean(gala$Adjacent), 
                            Elevation = mean(gala$Elevation)),
        interval = "confidence")
## 95% confidence that the population mean number of Species for those values
### is between lwr and upr
predict(lmod_new, data.frame(Adjacent = mean(gala$Adjacent), 
                             Elevation = mean(gala$Elevation)),
        interval = "predict")
## This predicts that the number of species will lie between these values

# Conduct model checking

hist(rstandard(lmod_new))
## still have the right skew
plot(lmod_new)

# Look at predicted values y-hat
qplot(predict(lmod_new), gala$Species)+
  geom_abline(slope = 1, color = "red")

# look at residual values: e-hat = y - y-hat
residuals(lmod_new)[1:10]
qplot(predict(lmod_new), residuals(lmod_new))
## What does this plot tell us?

# Residual Sum of Squares (RSS) NOTE: linear models -> deviance = RSS
deviance(lmod_new)

# degrees of freedom: observations minus number of coefficients
df.residual(lmod_new)
nrow(gala) - length(coef(lmod_new))

# Variance(e-hat) = sigma^2 = sqrt(RSS/df)
sqrt(deviance(lmod_new)/df.residual(lmod_new))

# store the summary 
lmodsum <- summary(lmod_new)
lmodsum$sigma

# R^2 = 1 - RSS/TSS
lmodsum$r.squared

# Also think of R^2 as the squared correlation between y-hat and y
cor(predict(lmod_new),gala$Species)^2

# Adjusted R^2 penalizes for additional parameters
lmodsum$adj.r.squared
