# Define population size
n <- 1000

# Generate age data
age <- rnorm(n, mean = 35, sd = 10)

# Generate gender data (0 = male, 1 = female)
gender <- rbinom(n, 1, prob = 0.5)

# Generate weight data
ifelse(gender == 0, weight <- rnorm(n, mean = 80, sd = 10), weight <- rnorm(n, mean = 65, sd = 10))

# Generate height data
ifelse(gender == 0, height <- rnorm(n, mean = 180, sd = 10), height <- rnorm(n, mean = 165, sd = 10))

# Generate salary data with higher frequency for lower values and lower frequency for higher values
salary <- round(runif(n, min = 800, max = 10000)^2/1000)

# Introduce missing values
set.seed(123)
missing_rate <- 0.1 # 10% missing rate
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing
age[missing_index] <- NA
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing
gender[missing_index] <- NA
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing
weight[missing_index] <- NA
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing
height[missing_index] <- NA
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing
salary[missing_index] <- NA
missing_index <- sample(n, floor(n*missing_rate)) # random sample of indices to set as missing

# Combine variables into data frame
population_data <- data.frame(age, gender, weight, height, salary)

# View the first few rows of the data frame
head(population_data)

# Mean imputation for age variable
population_data$age_imputed <- ifelse(is.na(population_data$age), mean(population_data$age, na.rm = TRUE), population_data$age)

#Regression imputation: use regression models to predict the missing values 
#based on the other variables in the data set.
# Regression imputation for weight variable based on age and gender
reg_model <- lm(weight ~ age + gender, data = population_data)
population_data$weight_imputed <- ifelse(is.na(population_data$weight), predict(reg_model, newdata = population_data), population_data$weight)

#Multiple imputation: create multiple imputed data sets using a model-based 
#approach, where the missing values are replaced with plausible values based 
#on the observed data.
# Multiple imputation using the mice package
library(mice)
imp <- mice(population_data, m = 5) # generate 5 imputed data sets
population_data_imputed <- complete(imp) # combine the imputed data sets
