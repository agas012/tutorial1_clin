# Define population size
n <- 1000

# Generate age data
age <- rnorm(n, mean = 35, sd = 10)

# Generate gender data (0 = male, 1 = female)
gender <- rbinom(n, 1, prob = 0.5)

# Generate weight data
set.seed(123)
weight <- sapply(gender, function(x) {
  if(x == 0) {
    return(rnorm(1, mean=250, sd=40))
  } else {
    return(rnorm(1, mean=200, sd=35))
  }
})

# Generate height data
height <- sapply(gender, function(x) {
  if(x == 0) {
    return(rnorm(1, mean=180, sd=30))
  } else {
    return(rnorm(1, mean=160, sd=30))
  }
})

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

#different row random
# Introduce missing values
set.seed(123)
missing_rate <- 0.1 # 10% missing rate
age[sample(n, floor(n*missing_rate))] <- NA
gender[sample(n, floor(n*missing_rate))] <- NA
weight[sample(n, floor(n*missing_rate))] <- NA
height[sample(n, floor(n*missing_rate))] <- NA
salary[sample(n, floor(n*missing_rate))] <- NA
# Combine variables into data frame
population_data <- data.frame(age, gender, weight, height, salary)


