library(tidyverse)

# Sampling distributions and z-scores

set.seed(123) # reproducibility

# a tutorial on how to use ggplot:
# > https://r4ds.hadley.nz/data-visualize.html

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

# simpler version (how I would plot it quickly)
# ggplot(data, aes(x = prop_Brown)) +
#   geom_histogram(binwidth = 0.005, fill = "skyblue", color = "white") +
#   geom_vline(xintercept = 0.605, color = "red", linetype = "dashed", size = 1)


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

# simpler version (how I would plot it quickly)
# ggplot(df2, aes(x = x2, y = y2)) +
#   geom_area(data = subset(df2, x2 >= z), aes(x = x2, y = y2), fill = "red", alpha = 0.3) +
#   geom_line(color = "steelblue", size = 1) + # geom_point could also do the job
#   geom_vline(xintercept = z, color = "red", linetype = "dashed")

#### now an example with a continuous variable (not a proportion) ####

#### Normal distribution + z-score for a continuous variable (heights) ####
# Suppose adult heights (in cm) are approximately Normal with:
mu <- 170   # population mean height
sigma <- 8  # population sd

# Simulate a population-like sample
N <- 5000
heights <- tibble(height = rnorm(N, mean = mu, sd = sigma))

# Choose an observed height and compute its z
x_obs <- 185
z_obs <- (x_obs - mu) / sigma
p_right <- 1 - pnorm(z_obs)   # one-sided "taller than x_obs"
p_two   <- 2 * (1 - pnorm(abs(z_obs)))  # two-sided tail area

z_obs
p_right
p_two

# Visualize the population Normal model with the observed value
x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 400)
df_norm <- tibble(x = x, y = dnorm(x, mean = mu, sd = sigma))

ggplot(df_norm, aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_area(data = subset(df_norm, x >= x_obs), aes(x, y), fill = "red", alpha = 0.25) +
  geom_vline(xintercept = x_obs, color = "red", linetype = "dashed") +
  annotate("text", x = x_obs, y = max(df_norm$y)*0.9,
           label = paste0("Observed x = ", x_obs, " cm"), color = "red", hjust = -0.05) +
  annotate("text", x = mu + 2.3*sigma, y = max(df_norm$y)*0.5,
           label = paste0("z = ", round(z_obs, 2),
                          "\nP(X ≥ x) = ", signif(p_right, 3)),
           color = "black", hjust = 0) +
  labs(
    title = "Normal model for height with one-sided tail beyond observed x",
    x = "Height (cm)",
    y = "Density"
  ) +
  theme_minimal()

# Show the same z on the Standard Normal with shaded tail
x_z <- seq(-4, 4, length.out = 400)
df_z <- tibble(z = x_z, dens = dnorm(x_z))

ggplot(df_z, aes(z, dens)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_area(data = subset(df_z, z >= z_obs), aes(z, dens), fill = "red", alpha = 0.25) +
  geom_vline(xintercept = z_obs, color = "red", linetype = "dashed") +
  annotate("text", x = z_obs, y = 0.05,
           label = paste0("z = ", round(z_obs, 2)), color = "red", hjust = -0.1) +
  annotate("text", x = z_obs, y = 0.10,
           label = paste0("AUC (right) = ", signif(p_right, 3)), color = "red", hjust = -0.05) +
  labs(
    title = "Standard normal curve: z-score and right-tail area",
    x = "z-score",
    y = "Density"
  ) +
  theme_minimal()

#### Z-score for a sample mean (CLT / sampling distribution) ####
# Draw a sample of n individuals and test whether its mean differs from mu

n <- 36
samp <- sample(heights$height, size = n, replace = TRUE)
xbar <- mean(samp)

# Under H0: mean = mu with known sigma, the sampling distribution is Normal(mu, sigma/sqrt(n))
se <- sigma / sqrt(n)
z_xbar <- (xbar - mu) / se
p_two_mean <- 2 * (1 - pnorm(abs(z_xbar)))

xbar
z_xbar
p_two_mean
diffMu <- abs(xbar - mu)

# Visualize the sampling distribution and the observed sample mean
x_samp <- seq(mu - 4*se, mu + 4*se, length.out = 400)
df_samp <- tibble(x = x_samp, y = dnorm(x_samp, mean = mu, sd = se))

ggplot(df_samp, aes(x, y)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_area(data = subset(df_samp, x >= mu + diffMu), aes(x, y), fill = "red", alpha = 0.25) +
  geom_area(data = subset(df_samp, x <= mu - diffMu), aes(x, y), fill = "red", alpha = 0.25) +
  geom_vline(xintercept = xbar, color = "red", linetype = "dashed") +
  annotate("text", x = xbar, y = max(df_samp$y)*0.9,
           label = paste0("Observed x̄ = ", round(xbar, 2), " cm"),
           color = "red", hjust = -0.05) +
  annotate("text", x = mu + 2.5*se, y = max(df_samp$y)*0.55,
           label = paste0("z = ", round(z_xbar, 2),
                          "\nTwo-sided p = ", signif(p_two_mean, 3)),
           color = "black", hjust = 0) +
  labs(
    title = "Sampling distribution of the sample mean under H0",
    x = "Sample mean (cm)",
    y = "Density"
  ) +
  theme_minimal()

### here let's visualise what happens if we run this 20 times

# install.packages('patchwork')
library(patchwork)

creatingPlot <- function(heights_vec, mu, sigma, n = 36) {
  samp <- sample(heights_vec, size = n, replace = TRUE)
  xbar <- mean(samp)
  
  se <- sigma / sqrt(n)
  z_xbar <- (xbar - mu) / se
  p_two_mean <- 2 * (1 - pnorm(abs(z_xbar)))
  diffMu <- abs(xbar - mu)
  
  x_samp <- seq(mu - 4 * se, mu + 4 * se, length.out = 400)
  df_samp <- tibble(x = x_samp, y = dnorm(x_samp, mean = mu, sd = se))
  ymax <- max(df_samp$y)
  
  # return a ggplot object
  ggplot(df_samp, aes(x, y)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_area(data = subset(df_samp, x >= mu + diffMu), aes(x, y), fill = "red", alpha = 0.25) +
    geom_area(data = subset(df_samp, x <= mu - diffMu), aes(x, y), fill = "red", alpha = 0.25) +
    geom_vline(xintercept = xbar, color = "red", linetype = "dashed") +
    annotate("text", x = xbar - 0.05*xbar, y = ymax * 0.9,
             label = paste0("Observed x̄ = ", round(xbar, 2), " cm"),
             color = "red", hjust = -0.05) +
    annotate("text", x = mu + 0.5 * se, y = ymax * 0.55,
             label = paste0("z = ", round(z_xbar, 2),
                            "\nTwo-sided p = ", signif(p_two_mean, 3)))+
    theme_minimal()
}

# - build N plots as a list
set.seed(1)
plots <- replicate(
  12,                                                  # how many plots you want
  creatingPlot(heights_vec = heights$height, mu = mu, sigma = sigma, n = 36),
  simplify = FALSE
)

# ---- A) 5 x 4 grid on one panel ----
# using patchwork
wrap_plots(plots, nrow = 3, ncol = 4)

# > remember that we are using mu and sigma from a population (we assume we know these), normally you'd have to use an estimate of that, based on your sample(s)
# ?> question: what happens if I change sigma, or n? Are z, and p, going to be larger, smaller, or the same?
