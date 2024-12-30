# . ####
# Q1.1: Is there a correlation or association between two variables? - Observations assigned to categories ####
# 1. Chi-square test of association ####
stream<-matrix(c(10,2,8,6,2,10,7,5),nrow=2)
stream
chisq.test(stream)
# 2. Cramér coefficient of association ####
# Cramér coefficient (C) is a measure from 0 (no association) to 1 (perfect association)
 x<-matrix(c(10,2,8,6),nrow=2)
 x
 a<-x[1,1]
 b<-x[1,2]
 c<-x[2,1]
 d<-x[2,2]
 e<-x[1,1]+x[1,2]
 f<-x[2,1]+x[2,2]
 g<-x[1,1]+x[2,1]
 h<-x[1,2]+x[2,2]
 phi<-( (a*d)-(b*c) )/(sqrt(e*f*g*h) )
phi
# 3. Phi coefficient of association ####
# This is a special case of the Cramér coefficient for 2×2 tables
# . ####
# Q1.2: Is there a correlation or association between two variables? - Observations assigned a value ####
# 4. ‘Standard’ correlation (Pearson’s product-moment correlation) (cont, normal) ####
# aka bivariate normal distribution

data <- data.frame(
  Pair = 1:6,
  Female = c(17.1, 18.5, 19.7, 16.2, 21.3, 19.6),
  Male = c(16.5, 17.4, 17.3, 16.8, 19.5, 18.3)
)

# Calculate correlation coefficient
correlation <- cor(data$Female, data$Male)

# Calculate coefficient of determination (R-squared)
r_squared <- correlation^2

# Print results
cat("Correlation coefficient:", correlation, "\n")
cat("Coefficient of determination (R-squared):", r_squared * 100, "%\n")

cor(data)
cor.test(data$Female,data$Male)


# 5. Spearman’s rank-order correlation ####
# This is one of two commonly used non-parametric equivalents of the Pearson’s product-moment correlation.
# The statistic it gives is called rs and ranges from −1 through 0 to 1, indicating perfect
# negative correlation, no correlation and perfect positive correlation, respectively.


Female <- c(17.1, 18.5, 19.7, 16.2, 21.3, 19.6)
Male <- c(16.5, 17.4, 17.3, 16.8, 19.5, 18.3)

# Calculate Spearman's rank correlation
spearman_correlation <- cor(Female, Male, method = "spearman")
print(spearman_correlation) 

# Perform Spearman's rank correlation test
spearman_test <- cor.test(Female, Male, method = "spearman") 
print(spearman_test)

# Demonstrate equivalence to Pearson correlation on ranks
pearson_on_ranks <- cor(rank(Female), rank(Male))
print(pearson_on_ranks)
# 6. Kendall rank-order correlation ####

   cor(Female,Male,method="k")

 cor.test(Female,Male,method="kendall")
# => Regression (cause & effect) ####
# . ####
# Q2: Is there a cause-and-effect relationship between two variables? ####
# 7. ‘Standard’ linear regression ####
 
 pH <- c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6)
 Uptake <- c(
   11.32, 11.29, 11.37, 11.32, 11.32, 11.49,
   11.31, 11.22, 11.40, 11.31, 11.36, 11.52,
   11.22, 11.18, 11.38, 11.35, 11.40, 11.38,
   11.23, 11.21, 11.37, 11.32, 11.35, 11.49
 )
 
 # Create a data frame
 data <- data.frame(pH, Uptake)
 
 # Fit the linear regression model
 model <- lm(Uptake ~ pH, data = data)
 
 # Summarize the model
 summary(model)
 
 # Plot the data and regression line
 plot(Uptake ~ pH, data = data, main = "Uptake vs. pH")
 abline(model, col = "red") 
 
 # Diagnostic plots
 plot(model)

# + Kendall robust line-fit method ####
# simple non-parametric test
# ! 8. Logistic regression ####
 
 Shade <- c(1, 2, 3, 4, 5, 6, 7)
 Plants_with_virus <- c(2, 4, 4, 4, 6, 6, 8)
 Plants_without_virus <- c(8, 6, 6, 6, 4, 4, 2)
 
 # Create a data frame
 data <- data.frame(Shade, Plants_with_virus, Plants_without_virus)
 
 # Calculate the proportion of plants with virus
 data$Virus <- data$Plants_with_virus / (data$Plants_with_virus + data$Plants_without_virus)
 
 # Fit the logistic regression model
 model <- glm(Virus ~ Shade, family = binomial(link = "logit"), data = data)
 
 # Summarize the model
 summary(model)
 
 # Plot the fitted probabilities
 plot(Shade, fitted(model), type = "l", xlab = "Shade", ylab = "Probability of Virus")
 points(Shade, data$Virus)
 
 
# + Model II regression ####
# + Polynomial, cubic and quadratic regression ####
# . #### 
# Q3: Tests for more than two variables - Test of association ####
# 9. Analysis of covariance (ANCOVA) ####
# hybrid between ANOVA and linear regression
 
 Species <- factor(c(rep(1, 6), rep(2, 6)))
 Temp <- c(10.1, 12.2, 13.5, 11.2, 10.2, 9.8, 
           14.1, 12.3, 9.5, 11.6, 10.1, 9.4)
 BPM <- c(89.0, 94.8, 99.6, 93.8, 91.0, 89.2, 
          104.5, 103.6, 91.1, 99.6, 99.1, 88.7)
 
 # Create a data frame
 data <- data.frame(Species, Temp, BPM)
 
 # Perform ANOVA using aov()
 aov_model <- aov(BPM ~ Species + Temp, data = data) 
 summary(aov_model)
 
 # Perform ANOVA using lm()
 lm_model <- lm(BPM ~ Species + Temp, data = data)
 summary(lm_model)

# + Multiple regression, stepwise regression #### 
 
 
 