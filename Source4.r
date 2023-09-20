# Set seed for reproducibility
set.seed(123)

# Generate synthetic data for age, weight, and height
n <- 1000
age <- sample(18:65, n, replace = TRUE)
weight <- rgamma(n, 1, 1)
height <- rnorm(n, mean = 170, sd = 10)

# Combine variables into data frame
synthetic_data <- data.frame(age, weight, height)

# Add disease variable with different prevalences by gender
synthetic_data$gender <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.4, 0.6))
synthetic_data$disease <- ifelse(synthetic_data$gender == "Male", 
                                 rbinom(n, 1, 0.2), rbinom(n, 1, 0.3))

# Print summary statistics
summary(synthetic_data)

# Split data into groups with and without disease
disease_group <- subset(synthetic_data, disease == 1)
no_disease_group <- subset(synthetic_data, disease == 0)

# Perform Wilcoxon rank-sum test
wilcox_result <- wilcox.test(disease_group$weight, no_disease_group$weight)

# Print the test result
print(wilcox_result)

