# . ####
# Q1: Do frequency distributions differ? ####
# 1. G Test ########

# Install and load DescTools if not already installed
if (!require(DescTools)) install.packages("DescTools")
library(DescTools)

# Observed frequencies
observed <- c(40, 30, 20, 10)

# Expected frequencies (Uniform Distribution)
expected_uniform <- c(25, 25, 25, 25)

# Expected frequencies (Custom Distribution)
expected_custom <- c(40, 30, 20, 10)

# G-test for Uniform Distribution
gtest_uniform <- GTest(x = observed, p = expected_uniform / sum(expected_uniform))
print("G-test for Uniform Distribution:")
print(gtest_uniform)

# G-test for Custom Distribution
gtest_custom <- GTest(x = observed, p = expected_custom / sum(expected_custom))
print("G-test for Custom Distribution:")
print(gtest_custom)



# 2. Chi Square Test Example 1 ####

# Observed frequencies
observed <- c(40, 30, 20, 10)

# Expected frequencies (Uniform Distribution)
expected_uniform <- c(25, 25, 25, 25)

# Chi-square test for Uniform Distribution
chisq_uniform <- chisq.test(x = observed, p = expected_uniform / sum(expected_uniform))
print("Chi-Square Test for Uniform Distribution:")
print(chisq_uniform)

# Expected frequencies (Custom Distribution)
expected_custom <- c(40, 30, 20, 10)

# Chi-square test for Custom Distribution
chisq_custom <- chisq.test(x = observed, p = expected_custom / sum(expected_custom))
print("Chi-Square Test for Custom Distribution:")
print(chisq_custom)







# Chi Square Test Example 2 ###

number <- c(0:8)
obs<-c(37,32,16,9,2,0,1,1,0)
meanlice=sum(obs*number)/sum(obs)
meanlice

expected_freq=sum(obs)*dpois(number,meanlice)
expected_freq
hist(expected_freq)

sum(obs)-sum(expected_freq[1:4] )

expected_freq[5]=sum(obs)-sum(expected_freq[1:4] )
expected_freq
plot(c(1:length(expected_freq)), expected_freq, type="o")
plot(density(expected_freq))

obs[5]=sum(obs)-sum(obs[1:4] )
obs
plot(c(1:length(obs)), obs, type = "o")

par(mfrow = c(2, 1)) 
plot(c(1:length(expected_freq)), expected_freq, type="o")
plot(c(1:length(obs)), obs, type = "o",
     xlab = "Index", ylab = "Observed Values", 
     main = "Observed Values")
par(mfrow = c(1, 1)) 

v=(obs-expected_freq)^2/expected_freq
chisquare=sum(v[1:5] )
chisquare

1-pchisq(chisquare,3)
# Here the P-value is well above 0.05 so we don’t reject the null hypothesis 
# that the frequencies of number of lice per fish follows a Poisson distribution.









# 3. Kolmogorov-Smirnov test ####

var1 <- c(12.5, 13.5, 13.2, 12.5, 12.1, 12.6, 12.1, 12.8, 
          14.2, 13.2, 13.8, 12.0, 12.5, 12.1, 12.8, 12.9, 
          12.6, 12.8, 12.5, 13.1, 12.4, 13.5, 13.4, 13.6, 
          13.0, 14.1, 12.6, 13.2, 13.8, 13.8, 13.9, 14.0, 
          14.1, 12.1, 12.9, 14.5, 13.2, 14.1, 12.5, 12.5, 
          15.0, 12.6, 13.0, 13.5, 14.0, 12.9, 12.4, 12.8) 
#plot (c(1:length(var1)), var1)

ks.test(var1, "pnorm", mean=mean(var1),
        sd=sqrt(var(var1) ) )
#One-sample Kolmogorov–Smirnov test
#data: var1



# 4. Anderson-Darling Test ####

# Define the data
var1 <- c(12.5, 13.5, 13.2, 12.5, 12.1, 12.6, 12.1, 12.8, 
          14.2, 13.2, 13.8, 12.0, 12.5, 12.1, 12.8, 12.9, 
          12.6, 12.8, 12.5, 13.1, 12.4, 13.5, 13.4, 13.6, 
          13.0, 14.1, 12.6, 13.2, 13.8, 13.8, 13.9, 14.0, 
          14.1, 12.1, 12.9, 14.5, 13.2, 14.1, 12.5, 12.5, 
          15.0, 12.6, 13.0, 13.5, 14.0, 12.9, 12.4, 12.8)

# Install and load the nortest package (if not already installed)
if(!require(nortest)) install.packages("nortest")
library(nortest)

# Perform the Anderson-Darling test
ad.test(var1)

# 5. Shapiro-Wilk Test ####
shapiro.test(var1)

# 6. Graphical test for normality - Quantile-Quantile QQ Plot, ECDF ####
var1
qqnorm(var1)
qqline(var1)
V1<-var1
x<-seq(11.5, 15.5, 0.1)
V1
plot(ecdf(V1)) #empirical cumulative distribution function (ECDF)
lines(x, pnorm(x, mean=mean(V1), sd=sqrt(var(V1))),
      lty=3)

plot(ecdf(x), do.points=FALSE, verticals=TRUE)
lines(x, pnorm(x, mean=mean(V1), sd=sqrt(var(V1))),
      lty=3)

# 6+ One sample t-test ####
# Sample data (replace with your actual data)
yield <- c(155, 148, 162, 158, 149, 160, 152, 157, 165, 145, 
           153, 159, 161, 147, 156, 163, 150, 154, 151, 164)

plot(density(yield), main = "Density Plot of Corn Yields", xlab = "Yield (bushels/acre)")

# Historical average yield
historical_mean <- 150

# Perform the one-sample t-test
t_test_result <- t.test(yield, mu = historical_mean)

# Print the results
print(t_test_result) 




# Sample data (replace with your actual data)
group1 <- c(12.5, 13.5, 13.2, 12.5, 12.1, 12.6, 12.1, 12.8)
group2 <- c(14.2, 13.2, 13.8, 12.0, 12.5, 12.1, 12.8, 12.9)

# T-test
t_test_result <- t.test(group1, group2)
print("T-test:")
print(t_test_result)



# . #####
# Q2.1: Do the observations from two groups differ? - Paired data ####
# 7. Paired t-test (before & after intervention) ####
before=c(34.6,38.2,37.6)
after=c(41.3,39.6,41.0)
t.test(before,after,paired=T)

# 8. Wilcoxon signed ranks test ####
# This test is the non-parametric equivalent of the paired t-test.
Day1 <- c(268, 260, 243, 290, 294, 270, 268)
Day2 <- c(236, 241, 239, 285, 282, 273, 258)
wilcox.test(Day1, Day2, paired=TRUE)
# V=27, p-value=0.03125
# Here the P-value is less than 0.05, so we reject the null hypothesis that the
# median difference between ‘Day1’ and ‘Day2’ is zero.

# 8+ Sign test ####

length(Day1)
#[1] 7
sum(Day1>Day2)
#[1] 6

binom.test(6,7)
#p=0.8571429
#Here the P-value is greater than 0.05, so we can’t exclude the null hypothesis
#that there is an equal probability of ‘Day1’ being greater than ’Day2’ and ‘Day1’
#being less than ‘Day2’.





# . ####
# Q2.2: Do the observations from two groups differ? - Unpaired data ####
# 9. T-test (comparing two set of values) ####
Premier<- c(24.5, 
            23.4 ,
            22.1 ,
            25.3 ,
            23.4 )
  Super <-  c(26.4, 27.0, 25.2, 25.8, 27.1)

  t.test(Premier,Super,paired=FALSE,var.equal=TRUE)
  t.test(Premier,Super,paired=FALSE)
  #t.test(Grain~Cultivar, var.equal=TRUE)
  
  Grain <- c(24.5, 23.4, 22.1, 25.3, 23.4, 26.4, 27.0, 25.2, 25.8, 27.1)
  Cultivar <- factor(c(rep("Premier", 5), rep("Super", 5))) 
  
  #Grain <- c(24.5, 23.4, 22.1, 25.3, 23.4, 26.4, 27.0, 25.2, 25.8, 27.1, 25.5, 25.7, 26.8, 27.3, 26.0)
  #Cultivar <- factor(c(rep("Premier", 5), rep("Super", 5), rep("Dupa", 5)))) 
  
  # Perform t-test with group information
  t.test(Grain ~ Cultivar, var.equal=TRUE) 
  
# 9+ One-way ANOVA test ####
  summary(aov(Grain ~ Cultivar))

# 10. Mann-Thitney U test (comparing two set of values) ####
# Mann-Whitney U test
mann_whitney_result <- wilcox.test(group1, group2)
print("\nMann-Whitney U test:")
print(mann_whitney_result)



#





# . ####
# Q3.1: Do the observations from more than two groups differ? - Repeated Measures ####
# 11. Friedman test ( for repeated measures) ####
pondcells <- matrix(
  c(130, 125, 350, 375, 225, 235, 
    115, 120, 375, 200, 250, 200, 
    145, 170, 235, 275, 225, 155, 
    200, 230, 140, 325, 275, 215), 
  nrow = 4, 
  byrow = TRUE, 
  dimnames = list(1:4, c("A", "B", "C", "D", "E", "F"))
)

friedman.test(pondcells)

# ! 12. Repeataed Measures ANOVA ( for repeated measures) ####
# Sample data (assuming 'Cells' is a vector and 'Pond' and 'Day' are factors)
Cells <- c(130, 125, 350, 375, 225, 235, 
           115, 120, 375, 200, 250, 200, 
           145, 170, 235, 275, 225, 155, 
           200, 230, 140, 325, 275, 215)

Pond <- factor(rep(c("A", "B"), each = 12)) # Assuming two ponds
Day <- factor(rep(1:6, 4)) # Assuming six days of measurement

# Create a data frame
df <- data.frame(Cells, Pond, Day)

# Perform repeated measures ANOVA
model <- aov(Cells ~ Pond*Day + Error(Cells/(Pond*Day)), data = df)
summary(model)
summary(aov(Cells∼Pond*Day + Error(Cells/(Pond*Day))))

# . ####
# Q3.2: Do the observations from more than two groups differ? - Independent Samples ####
# 13. One-way ANOVA test ####
oneway <- data.frame(Grain, Cultivar)
print(oneway) 
# Perform one-way ANOVA
model <- aov(Grain ~ Cultivar, data = oneway) 
# Summarize the ANOVA results
summary(model)

# 13+. Post hoc testing: after one-way ANOVA ####
Grain <- c(24.5, 23.4, 22.1, 25.3, 23.4, 26.4, 27.0, 25.2, 25.8, 27.1, 25.5, 25.7, 26.8, 27.3, 26.0)
Cultivar <- factor(c(rep("Premier", 5), rep("Super", 5), rep("Dupa", 5))) 

model <- aov(Grain_size ~ Cultivar_code, data = data)

# Basic plot of Grain vs. Cultivar
plot(Cultivar, Grain)

# Plot with custom labels
labs <- c("Dupa", "Premier", "Super") # Assuming "P" corresponds to "Dupa" 
plot(Cultivar, Grain, xlab = "Cultivar", ylab = "Grain size (mg)", names = labs)

# Perform one-way ANOVA
model <- aov(Grain ~ Cultivar, data = oneway) 

# Perform Tukey's HSD post-hoc test
tukey_results <- TukeyHSD(model)

# Print Tukey's HSD results
print(tukey_results)


# 14. Kruskal–Wallis test ####
 kruskal.test(Grain~Cultivar)

# 14+. Post hoc testing: after Kruskal–Wallis test ####

# . ####
# Q4.1: There are two independent ways of classifying the data ####
# 15. Friedman test ** ####
#The Friedman test is a non-parametric analogue of a two-way ANOVA
f <- matrix(c(
  1130, 1125, 1350, 1375, 1225, 1235,
  1115, 1120, 1375, 1200, 1250, 1200,
  1145, 1170, 1235, 1175, 1225, 1155,
  1200, 1230, 1140, 1325, 1275, 1215
), nrow = 4, byrow = TRUE)

# Perform Friedman test on the original matrix
friedman.test(f)

# Transpose the matrix
g <- t(f)

# Perform Friedman test on the transposed matrix
friedman.test(g)

# This time the P-value is well above 0.05, so we have no reason to reject the null
# hypothesis that data in the columns come from a distribution with the same median.

# ! 16. Two-Way ANOVA (without replication) ####
# input????
Yield <- c(1130, 1125, 1350, 1375, 1225, 1235,
1115, 1120, 1375, 1200, 1250, 1200,
1145, 1170, 1235, 1175, 1225, 1155,
1200, 1230, 1140, 1325, 1275, 1215)
  #c(10, 12, 15, 8, 11, 13, 14, 9, 16, 12, 18, 15,            17, 14, 19, 13, 16, 18, 20, 17, 21, 19, 22, 20) 

Blend <- factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4))) 
Farm <- factor(rep(1:4, each = 6)) 

# Create a data frame
two <- data.frame(Yield, Blend, Farm)

# Check column names
names(two)

# Perform two-way ANOVA without interaction
#model <- aov(Yield ~ Farm + Blend, data = two) 
model <- aov(Yield ~ Farm * Blend) 

# Summarize the ANOVA results
summary(model)

# . ####
# Q4.2: More than one observation for each factor combination (with replication) ####
# 17. Interaction ####
 interaction.plot(Farm,Blend,Yield)
# 18. Two-Way ANOVA (with replication) *** ####

# Sample data (hardcoded)
star <- data.frame(
  intake = c(78.1, 75.5, 76.3, 81.2, 69.5, 72.1, 73.2, 71.1, 82.4, 80.9, 83.0, 88.2, 72.3, 73.3, 70.0, 72.9),
  sex = factor(c(rep("female", 4), rep("male", 4), rep("female", 4), rep("male", 4))), 
  day = factor(c(rep(16, 8), rep(8, 8))) 
) 
intake <- star$intake
sex <- star$sex
day <-star$day

# Perform two-way ANOVA with interaction
model <- aov(intake ~ sex * day, data = star)

# Summarize the ANOVA results
summary(model)

# Create an interaction plot
interaction.plot(star$sex, star$day, star$intake, 
                 xlab = "Sex", ylab = "Intake", 
                 main = "Interaction Plot of Sex and Day on Intake")
# 19. Scheirer–Ray–Hare test ####
# ~ ANOVA using ranks in an extension of the Kruskal–Wallis test?




# Sample data (replace with your actual data)
star <- data.frame(
  intake = c(78.1, 75.5, 76.3, 81.2, 69.5, 72.1, 73.2, 71.1, 82.4, 80.9, 83.0, 88.2, 72.3, 73.3, 70.0, 72.9),
  sex = factor(c(rep("female", 4), rep("male", 4), rep("female", 4), rep("male", 4))),
  day = factor(c(rep(16, 8), rep(8, 8)))
)

# Create a new variable 'myrank' 
star$myrank <- rank(star$intake)

# Perform two-way ANOVA using the ranks
model <- aov(myrank ~ sex * day, data = star)

# Summarize the ANOVA results
anova_table <- summary(model)

# Calculate MStotal
ms_tot <- sum(anova_table[[1]][, "Sum Sq"]) / sum(anova_table[[1]][, "Df"])

# Calculate Scheirer-Ray-Hare test statistics
sex_statistic <- anova_table[[1]][1, "Sum Sq"] / ms_tot
day_statistic <- anova_table[[1]][2, "Sum Sq"] / ms_tot
int_statistic <- anova_table[[1]][3, "Sum Sq"] / ms_tot

# Calculate p-values
sex_pvalue <- 1 - pchisq(sex_statistic, 1)
day_pvalue <- 1 - pchisq(day_statistic, 1)
int_pvalue <- 1 - pchisq(int_statistic, 1)

# Print the results
cat("Scheirer-Ray-Hare Test Results:\n")
cat("Factor", "Statistic", "p-value\n")
cat("sex", sex_statistic, sex_pvalue, "\n")
cat("day", day_statistic, day_pvalue, "\n")
cat("sex:day", int_statistic, int_pvalue, "\n")




# . ####
# Q5: There are more than two independent ways to classify the data ####
# Multifactorial testing ####
# 20. Three-Way ANOVA (without replication) ####
region <- factor(c(rep("North", 4), rep("South", 4), rep("North", 4), rep("South", 4))) 
 summary(aov(intake~sex*day*region))
 summary(aov(intake~sex*day*region-sex:day:region))
# 21. Three-Way ANOVA (with replication) ####
 
 

 
 # Data
 distance <- factor(rep(c(1, 2), each = 12)) # Corrected: 24 elements
 exclosure <- factor(rep(c(0, 1, 2), 4)) 
 site <- factor(rep(c(1, 1, 1, 1, 2, 2, 2, 2), 3)) # Corrected: 24 elements
 grass <- c(112, 115, 187, 141, 121, 189, 116, 102, 175, 101, 157, 186, 
            121, 145, 198, 135, 141, 208, 138, 124, 168, 129, 133, 206)
 
 # Create a data frame
 data <- data.frame(grass, distance, exclosure, site)
 
 # Fit the three-way ANOVA model (with replication)
 model <- aov(grass ~ exclosure * distance * site, data = data)
 
 # Summarize the ANOVA results
 summary(model)
 
 #?????
 TukeyHSD(model, "exclosure")
 
 
 # + Multiway ANOVA ####

# . ####
# Q6: Not all classifications are independent - ####
# Non independent factors, Nested factors, random or fixed factors
# Nested or hierarchical designs
# ? 21. Two-level nested-design ANOVA ####
#  summary(aov(cholest~intake/cage))
 
 # Sample data (replace with your actual data)
 cholest <- c(180, 175, 190, 178, 185, 192, 
              160, 155, 168, 152, 165, 170, 
              140, 135, 145, 142, 138, 148) 
 intake <- factor(rep(c("low", "medium", "high"), each = 6))
 cage <- factor(rep(1:6, 3)) 
 
 # Create a data frame
 data <- data.frame(cholest, intake, cage)
 data
 # Perform nested ANOVA
 model <- aov(cholest ~ intake/cage, data = data)
 
 # Summarize the ANOVA results
 summary(model)



