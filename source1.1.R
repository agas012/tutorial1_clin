#Load the plotly package
library(plotly)

# Define population size
n <- 1000

nis <- is.numeric(n)
nclass <- class(n)

# Generate age data
age <- rnorm(n, mean = 35, sd = 10)

# Generate gender data (0 = male, 1 = female)
#Binomial distribution is a probability distribution used in statistics that 
#summarizes the likelihood that a value will take one of two independent 
#values under a given set of parameters or assumptions.
#The underlying assumptions of binomial distribution are that there is only one 
#outcome for each trial, that each trial has the same probability of success, 
#and that each trial is mutually exclusive, or independent of one another.
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


# Define prevalence rate of disease for males and females
disease_prevalence_male <- 0.2
disease_prevalence_female <- 0.1

disease <- sapply(gender, function(x) {
  if(x == 0) {
    return(rbinom(1, 1, prob = disease_prevalence_male))
  } else {
    return(rbinom(1, 1, prob = disease_prevalence_female))
  }
})

# Generate salary data with higher frequency for lower values and lower frequency for higher values
#In this example, we use the runif function to generate a vector of uniformly 
#distributed random numbers between 800 and 10000. We then apply a non-linear 
#transformation by squaring the values and dividing them by 1000, which causes 
#the lower values to be more frequent and the higher values to be less frequent. 
#We round the transformed values to the nearest integer to get the 
#runif(n, a, b) generates n uniform random numbers between a and b.
#final salary values.
a = runif(n, min = 800, max = 10000)
b = a^2
c = b/1000
hist(a)
hist(b)
hist(c)
salary <- round(runif(n, min = 800, max = 10000)^2/1000)
hist(salary)

# Combine variables into data frame
population_data <- data.frame(age, gender, weight, height, disease, salary)
hist(population_data[['weight']])

# View the first few rows of the data frame
head(population_data)

# Create a scatter plot of height vs weight with color-coded markers by gender
plot <- plot_ly(data = population_data, x = ~weight, y = ~height, color = ~factor(gender),
                colors = c("#1F77B4", "#FF7F0E"), type = "scatter", mode = "markers",
                marker = list(size = 6, opacity = 0.7),
                hovertemplate = paste("Weight: %{x}<br>Height: %{y}<br>Gender: %{marker.color}<extra></extra>")
)
# Add axis titles and plot title
plot <- plot %>% layout(title = "Height vs Weight by Gender",
                        xaxis = list(title = "Weight"),
                        yaxis = list(title = "Height"))

# Display the plot
plot

ploth <- plot_ly(data = population_data, x = ~weight, color = ~factor(gender),
                 colors = c("#1F77B4", "#FF7F0E"), type = "histogram")
ploth


# Split data into groups with and without disease
disease_group <- subset(population_data, disease == 1)
no_disease_group <- subset(population_data, disease == 0)

# Perform t-test to compare height between groups
ttest_result <- t.test(disease_group$height, no_disease_group$height)

# Print the t-test result
print(ttest_result)

# Define age categories
age_breaks <- c(-Inf, 18, 30, 40, 50, Inf)
age_labels <- c("Under 18", "18-29", "30-39", "40-49", "Over 50")
# Categorize age into groups
population_data$age_group <- cut(population_data$age, breaks = age_breaks, labels = age_labels)

# Create contingency table of age groups by disease status
age_table <- table(disease = 
                   factor(population_data$disease, levels = c(0, 1),labels = c("No", "Yes")), 
                   age_group = factor(population_data$age_group, levels = age_labels),
                   dnn = c("Disease", "Age group")
                   )

# Perform chi-square test of independence
chi2_result <- chisq.test(age_table)

# Print the chi-square test result
print(chi2_result)