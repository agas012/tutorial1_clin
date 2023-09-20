# Create a vector of random data
x <- rnorm(1000)
# Create a Q-Q plot with a 45-degree reference line
qqnorm(x)
qqline(x, col = "red")


y <- rgamma(10000, 1)
# Create a Q-Q plot with a 45-degree reference line
qqnorm(y)
qqline(y, col = "red")