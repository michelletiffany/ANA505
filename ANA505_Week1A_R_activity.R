# ANA 505 Week 1A Activity
# Michelle Tan - 07/04/2024

# Install and load caTools Library
install.packages('caTools')
library(caTools)

#Define datasets
years_experience <- c(1.1,1.3,1.5,2.0,2.2,2.9,3.0,3.2,3.2,3.7)
salary <- c(39343.00, 46205.00, 37731.00, 43525.00, 39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)

#Dataframe
df <- data.frame(Years_exp = years_experience, Salary = salary)

head(df)
attach(df)

set.seed(123)
split <- sample.split(df$Salary, SplitRatio = 0.8)

trainingset <- subset(df, split == TRUE)
trainingset_false <- subset(df, split == FALSE)
 
lm.r <- lm(formula = Salary~Years_exp, data = trainingset)

summary(lm.r)

# Call:
#   lm(formula = Salary ~ Years_exp, data = trainingset)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5986.8 -4326.6   440.6  3827.3  5864.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    29171       4855   6.008 0.000958 ***
#   Years_exp       9190       1933   4.755 0.003141 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5032 on 6 degrees of freedom
# Multiple R-squared:  0.7903,	Adjusted R-squared:  0.7554 
# F-statistic: 22.61 on 1 and 6 DF,  p-value: 0.003141


