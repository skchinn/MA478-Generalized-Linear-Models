library(tidyverse)
library(reshape2)
library(faraway)

# Load the data
houses <- read_csv("./Lesson Files/MA478_Data_Houses.csv")
head(houses)

# Research Question: Is there a relationship between Selling Price and Size/New?

# EXPLORE THE DATA
houses %>%
  group_by(new) %>%
  tally()

# Plot them
ggplot(houses, aes(x=size, y = price, color = new))+
  geom_point()

# Explore the distribution of Y (price)
qplot(houses$price, geom = "boxplot")

# Look at the distribution of the data
qplot(houses$price,
      geom = "histogram",
      main = "Histogram for Price",
      xlab = "Price")

qplot(houses$price,
      geom = "density",
      main = "Density for Price",
      xlab = "Price")
# Does it look normal?
summary(houses$price)

# While it doesn't look normal, let's keep moving forward
summary(houses)

# Boxplots
ggplot(data = melt(houses[,-c(1,3,4,5,6)]), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill=variable))

ggplot(data = melt(houses[,-c(1,2,6,7)]), aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill=variable))

# Look at the relationship between all of them
pairs(houses)

# BUILD AND INTERPRET THE MODEL

# Build a linear model
lmod <- lm(price ~ size + new, data = houses)
summary(lmod)
# Identify some things in the summary: R^2, F-test, MUT

# ASSESS MODEL
hist(rstandard(lmod))
## still have the right skew
plot(lmod)

# 95% confidence interval for average price given mean value of house size and new house
predict(lmod, data.frame(size = mean(houses$size), 
                          new = 1),
        interval = "confidence")

## 95% prediction interval for average price given mean value of house size and new house
predict(lmod, data.frame(size = mean(houses$size), 
                         new = 1),
        interval = "predict")


# Research Question: Are new houses more expensive per square foot than old houses?
houses %>%
  mutate(price_per_sqft = price/size) %>%
  group_by(new)%>%
  summarize(med_ppsft = median(price_per_sqft)) 


# Research Question: How does taxes, #bed rooms, #baths, and new-ness impact
##                 price per square foot?
houses_new <- houses %>%
  mutate(pps = price/size)
summary(houses_new)

# Plot
ggplot(houses_new, aes(x = taxes, y = pps, color = new))+
  geom_point()

# Explore distribution of Y (price per square foot)
qplot(houses_new$pps, 
      geom = "density",
      main = "Histogram of PPS",
      xlab = "PPS")

# Build a linear model
lmod2 <- lm(pps ~ taxes + beds + baths + new, data = houses_new)
summary(lmod2)

# Build another linear model, removing baths and new (a smaller model)
lmod3 <- lm(pps ~ taxes + beds, data = houses_new)
summary(lmod3)

# Let lmod2 be the larger model compared to lmod3
# Conduct ANOVA
anova(lmod2, lmod3)
# Large p-value so fail to reject the null hypothesis that the smaller model is correct

# Conduct residual diagnostics 
plot(houses$taxes/houses$price)
plot(lmod2)

# Let's look for influential outliers using Cook's statistic
houses_new[cooks.distance(lmod2) > 0.1, ]

