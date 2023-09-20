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
gender[missing_index] <- NA
weight[missing_index] <- NA
height[missing_index] <- NA
salary[missing_index] <- NA

# Combine variables into data frame
population_data <- data.frame(age, gender, weight, height, salary)

# View the first few rows of the data frame
head(population_data)
