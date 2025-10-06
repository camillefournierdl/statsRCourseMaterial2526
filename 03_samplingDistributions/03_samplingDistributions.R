library(tidyverse)

# Sampling distributions and z-scores

set.seed(123) # reproducibility

#### how to simulate a test distribution, and compare it with an observed sample ####
# example 4.11 from the book

# Set parameters
n <- 1824       # number of voters
n_sim <- 10000  # number of simulated elections

# Simulate each election: number of votes for Brown
votes_Brown <- rbinom(n = n_sim, size = n, prob = 0.5)

# Compute proportion of votes for Brown
prop_Brown <- votes_Brown / n

# Basic summary
mean(prop_Brown)        # average vote share for Brown (should be close to 0.5)
sd(prop_Brown)          # standard deviation across simulations : sqrt(p*(1-p)/n) is an estimation (in practice we usually do not know the underlying pop's sd)

data <- data.frame(prop_Brown)

# Plot with ggplot2
ggplot(data, aes(x = prop_Brown)) +
  geom_histogram(binwidth = 0.005, fill = "skyblue", color = "white") +
  geom_vline(xintercept = 0.605, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 0.605, y = 800, label = "p = 0.605", color = "red", hjust = 1.5) +
  labs(
    title = "Distribution of Vote Share for Brown",
    x = "Proportion voting for Brown",
    y = "Count"
  ) +
  theme_minimal()

# calculation of a z-score:
p_hat <- 0.605 # observed probability in sample
p0 <- 0.5 # theoretical probability from population if votes are spread 50-50
n <- 1824 # sample size

# the formula is essentially an estimation of how big the deviation is:
# how many standard errors is the value observed from the mean
# this is using the formulas from the "Data Analysis for Public Policy Formulas.pdf" file
# (z score for one sample proportion), using se_0
z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n) # formula for proportion z-score

z

# probability to observe a value larger than z in a normal distribution 
1 - pnorm(z) # pnorm calculates the probability of a sample mean being smaller than the z-score, in a normal distribution
# when we start talking about tests, that would be the equivalent of a one-sided test

# >> this tells us that the population from which we sampled might be different from the one we simulated (mean = 0.5)

#### example (try to think how to do it yourself) ####
# ?> how would you visualize the likelihood of the population's sample mean to be 0.6 (if the observed sample is 0.6, with n = 1824)

p_hat <- 0.605 # observed probability in sample
p0 <- 0.6 # theoretical probability from population if votes are spread 60-40
n <- 1824 # pop size

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n) # formula for proportion z-score

z

# probability to observe a value larger than z in a normal distribution 
1 - pnorm(z) # pnorm calculates the probability of a sample being smaller than the z-score, in a normal distribution

### here we visualize the example:

# approximate / simulate sampling distribution under H0
mean0 <- p0
sd0 <- sqrt(p0 * (1 - p0) / n) # could also use the sd of the pop, from the book, but it's usually not known

x <- seq(mean0 - 4*sd0, mean0 + 4*sd0, length.out = 400)
y <- dnorm(x, mean = mean0, sd = sd0)*sd0 # standardize output to have this and next plot on the same scale
df1 <- data.frame(x = x, y = y)

ggplot(df1, aes(x, y)) +
  geom_line(color = "steelblue", size = 1) +
  geom_vline(xintercept = p_hat, color = "red", linetype = "dashed") +
  annotate("text", x = p_hat, y = max(y)*0.9, label = "Observed p̂", color = "red", hjust = -0.1) +
  labs(
    title = "Sampling distribution of sample proportion under H0",
    x = "Sample proportion",
    y = "Density"
  ) +
  theme_minimal()

# >> it's likely the sample is taken from a population that has a proportion of voters around 0.6 for Brown

# how to represent z on that plot?

x2 <- seq(-4, 4, length.out = 400)
y2 <- dnorm(x2) # 
df2 <- data.frame(x2, y2)

ggplot(df2, aes(x2, y2)) +
  geom_area(data = subset(df2, x2 >= z), aes(x2, y2), fill = "red", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1) +
  geom_vline(xintercept = z, color = "red", linetype = "dashed") +
  annotate("text", x = z, y = 0.05, label = paste0("z = ", round(z, 2)), color = "red", hjust = -0.1) +
  annotate("text", x = z, y = 0.10, label = paste0("AUC = ", round(1 - pnorm(z), 2)), color = "red", hjust = -0.05) +
  labs(
    title = "Standard normal curve showing z-score and area under curve (AUC)",
    x = "z-score",
    y = "Density"
  ) +
  theme_minimal()

