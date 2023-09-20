# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n <- 100
x <- rnorm(n, mean = 0, sd = 1)
y <- rnorm(n, mean = 0.5, sd = 1.2)
z <- rnorm(n, mean = 1, sd = 0.8)
categorical <- sample(c("A", "B", "C"), n, replace = TRUE)
disease <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))

# Combine variables into data frame
synthetic_data <- data.frame(x, y, z, categorical, disease)

# Box plots of continuous variables by categorical variable
boxplot(x ~ categorical, data = synthetic_data, main = "Box Plot of X by Categorical Variable")
boxplot(y ~ categorical, data = synthetic_data, main = "Box Plot of Y by Categorical Variable")
boxplot(z ~ categorical, data = synthetic_data, main = "Box Plot of Z by Categorical Variable")

# Correlation matrix plot of all continuous variables
corr_matrix <- cor(synthetic_data[,1:3])
corrplot::corrplot(corr_matrix, type = "upper", order = "hclust", tl.cex = 1)

# Perform parametric tests
t_result <- t.test(x, y)
anova_result <- aov(z ~ categorical, data = synthetic_data)
linear_result <- lm(y ~ x, data = synthetic_data)
logistic_result <- glm(disease ~ x + y + z, data = synthetic_data, family = binomial())
cor_result <- cor(x, y)

# Perform non-parametric tests
wilcox_result <- wilcox.test(x, y)
kruskal_result <- kruskal.test(z ~ categorical, data = synthetic_data)
spearman_result <- cor(x, y, method = "spearman")
mannwhitney_result <- wilcox.test(x, y)

# Print the test results
print(t_result)
print(anova_result)
print(linear_result)
print(logistic_result)
print(cor_result)
print(wilcox_result)
print(kruskal_result)
print(spearman_result)
print(mannwhitney_result)

# Scatter plot of two continuous variables with regression line
plot(x, y, main = "Scatter Plot of X and Y with Regression Line")
abline(linear_result, col = "red")

# Logistic regression plot with disease and continuous variable
plot(x, disease, pch = 19, xlab = "X", ylab = "Disease", main = "Logistic Regression Plot with Disease and X")
curve(predict(logistic_result, data.frame(x = x, y = mean(y), z = mean(z)), type = "response"), add = TRUE, col = "red")

# Bar plot of test results
p_values <- c(t_result$p.value, summary(anova_result)[[1]][["Pr(>F)"]][[1]], summary(linear_result)$coefficients[2, 4], summary(logistic_result)$coefficients[2, 4], cor.test(x, y)$p.value, wilcox_result$p.value, kruskal_result$p.value, spearman_result, mannwhitney_result$p.value)
names(p_values) <- c("One-sample t-test", "One-way ANOVA", "Linear regression", "Logistic regression", "Pearson correlation", "Wilcoxon rank-sum test", "Kruskal-Wallis test", "Spearman correlation", "Mann-Whitney U test")
barplot(p_values, main = "Bar Plot of Test Results", ylab = "P-value")
