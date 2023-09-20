#https://www.r-bloggers.com/2014/11/simulating-the-rule-of-five/
set.seed(1)  # make it repeatable
pop <- rnorm(1000)  # generate the "population"
med <- median(pop) # calculate the median

ssize <- 100000 # how many trials to run
matched <- sapply(seq(ssize), function(i) {
  rg <- range(sample(pop, 5)) # get the range for 5 random samples
  ifelse(med>=rg[1] & med<=rg[2], TRUE, FALSE) # test if median in range
})
sum(matched)/ssize # proportion matched


# first set up a function to do the sampling
pickfive <- function(popsize, ssize) {
  pop <- rnorm(popsize)
  med <- median(pop)
  matched <- sapply(seq(ssize), function(i) {
    rg <- range(sample(pop, 5))
    ifelse(med>=rg[1] & med<=rg[2], TRUE, FALSE)
  })
  sum(matched)/ssize
}
# test it
pickfive(popsize=1000, ssize=100000)

# 1,000 to 1 million by a thousand
set.seed(1)
possible <- seq(1000, 1000000, by=1000)
output <- sapply(possible, pickfive, 5000) # takes a while
print(mean(output))

plot(x = 1:length(output), y=output, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(h=mean(output), col = "red")