# Observation, inspection and plotting ####
# ! 1. Principal component analysis (PCA) and factor analysis ####
# Sample data (replace with your actual data)
data <- data.frame(
  Sex = factor(c(rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4))), 
  levels = c(1, 2), 
  labels = c("Male", "Female"),
Species = factor(rep(c(1, 2), 8)),
Thorax = c(1.01, 0.98, 1.02, 1.05, 0.98, 0.89, 0.89, 0.95, 
           1.20, 1.15, 1.18, 1.21, 0.95, 0.94, 0.96, 0.91),
Wing = c(2.51, 2.45, 2.57, 2.61, 2.40, 2.35, 2.38, 2.41, 
         3.10, 3.12, 3.21, 3.20, 2.51, 2.50, 2.62, 2.45),
Femur = c(0.06, 0.05, 0.08, 0.07, 0.04, 0.04, 0.05, 0.05, 
          0.09, 0.10, 0.09, 0.10, 0.08, 0.07, 0.08, 0.07),
Eye = c(0.52, 0.53, 0.55, 0.52, 0.54, 0.50, 0.50, 0.49, 
        0.48, 0.52, 0.52, 0.55, 0.56, 0.49, 0.51, 0.52),
Ant = c(0.11, 0.12, 0.11, 0.10, 0.13, 0.14, 0.12, 0.12, 
        0.09, 0.10, 0.11, 0.09, 0.11, 0.13, 0.14, 0.13)
)

# Perform PCA 
pca <- prcomp(~ Thorax + Wing + Femur + Eye + Ant, data = data, scale = TRUE)

# Summary of PCA
summary(pca)

# Print the results
print(pca)

# Create a biplot
biplot(pca)

# + Canonical variate analysis ####
# ! 2. Discriminant function analysis ####

# Load the MASS library
library(MASS)

# Perform LDA
fit <- lda(Species ~ Thorax + Wing + Femur + Eye + Ant, data = data, CV = TRUE)

# Print the results
print(fit)

# Extract predicted classes
predicted_classes <- fit$class

# Create a confusion matrix
confusion_matrix <- table(predicted_classes, data$Species)

# Calculate the proportion of correctly classified individuals for each species
#prop_table(confusion_matrix, margin = 1) #renamed in new R
proportions(confusion_matrix, margin = 1) 

# Calculate the overall proportion of correct classifications
diag(prop.table(confusion_matrix))


ct <- table(data$Species, fit$class) 

# Calculate the proportion of correctly classified individuals for each species
prop_correct_per_species <- diag(proportions(ct, 1)) 

# Calculate the overall proportion of correct classifications
overall_accuracy <- sum(diag(proportions(ct)))

# Print the results
print(prop_correct_per_species)
print(overall_accuracy) 


# ! 3. Multivariate analysis of variance (MANOVA) ####
# Assuming your data is in a data frame named 'data' as in the previous example

# Combine 'thorax' and 'wing' into a matrix
Y <- cbind(data$Thorax, data$Wing)

# Perform MANOVA
fit <- manova(Y ~ Species, data = data)

# Univariate ANOVA for each response variable
summary.aov(fit)

# MANOVA with Wilks' lambda test
summary(fit, test = "Wilks")

# + Multivariate analysis of covariance (MANCOVA) ####
# + Cluster analysis ####
# Assuming your data is in a data frame named 'data' as in the previous examples

# Select the variables for clustering
Y <- cbind(data$Thorax, data$Wing, data$Femur, data$Eye, data$Ant)

# Calculate Euclidean distances between individuals
d <- dist(Y, method = "euclidean")

# Perform hierarchical clustering using Ward's method
fit <- hclust(d, method = "ward.D") 

# Plot the dendrogram
plot(fit)

# Determine cluster membership for k = 4 clusters
k <- 4
groups <- cutree(fit, k = k)

# Add rectangles to the dendrogram
rect.hclust(fit, k = k, border = "red")

# + DECORANA and TWINSPAN ####
# DECORANA (detrended correspondence analysis) and TWINSPAN (two-way indicator species analysis)