library(tidyverse)

# Sampling distributions and z-scores

set.seed(123) # reproducibility

# In this class, we're still using pretty basic functions. 
# The most important knowledge might not be to know how these functions work, you can always google it, check the help?, ask chatgpt.
# Rather, we focus on visualizing what we're doing, and creating links between the empirical part (how would you do it by hand), and the computations.
# As such, the code contains simulations of distributions, and ggplots, which are meant for illustrating more than for you to learn how to use.

# a tutorial on how to use ggplot:
# > https://r4ds.hadley.nz/data-visualize.html

#### Toolbox with main functions ####

# pnorm(q, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)

# Calculate the probability (or area under the curve / AUC) to the left of a normal distribution: 
# default is mean = 0 and sd  = 1 -> z-distribution
# Returns P(X <= q) for X ~ N(mean, sd)

pnorm(1, lower.tail = FALSE)       # right-tail area at z = 1
pnorm(-1) # this is equivalent to using the table you'll use 'by hand' -> this function works with symmetrically from the course's table

pnorm(-1, mean = 0, sd = 3) # could edit the mean and sd argumments

pnorm(1, lower.tail = F) # using the lower.tail = F argument, this is more similar to using the table (probability, or area under the curve, of a positive z-value)

pnorm(2) - pnorm(-1)               # P(-1 < Z < 2)
pnorm(70, mean=60, sd=10)          # no need to standardize by hand

# dnorm(x, mean=0, sd=1, log=FALSE)
# Returns the density (height) at x — not a probability
dnorm(0) # can be used to plot the distribution of a normal distribution (can always manually set mean and sds) -> we use it with value ranging from -4 to 4, see:

z <- seq(-4, 4, length.out = 100)
pz <- dnorm(z) 

plot(pz~z) # plot of N(0,1)

# qnorm(p, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
# Quantile: z such that P(Z <= z) = p // Calculate a z-value for a probability
# P(output < -1) = input 
qnorm(0.5) # will give you the z-value corresponding to a certain probability (still considering the area under the curve of the left of that value, without using lower.tail = F)

qnorm(0.95)
qnorm(0.05) # is equal to the symetric z-value for a centered normal

qnorm(c(0.025, 0.975))             # ≈ -1.96, 1.96 -> could use more than 1 argument

qnorm(0.90, mean=100, sd=15)       # 90th percentile of N(100,15)

# rnorm(n, mean=0, sd=1)
# can be used to generate a random sample for a population that would follow a normal distribution of given mean and sd 
# Random draws; set.seed() for reproducibility
set.seed(123)

rnorm(50) 

hist(rnorm(100))

hist(rnorm(10000))

hist(rnorm(10000, mean = 20, sd = 10))

#### 4.9 ####
# For a normal distribution, verify that the probability between
# (a) μ − σ and μ + σ equals 0.68.
# (b) μ − 1.96σ and μ + 1.96σ equals 0.95.
# (c) μ − 3σ and μ + 3σ equals 0.997.
# (d) μ − 0.67σ and μ + 0.67σ equals 0.50.

# P(μ−kσ<X<μ+kσ)=P(−k<Z<k) # >> remember this!  

## - Visual + computation for P(μ-σ < X < μ+σ) -

## 1) Plot the standard normal and shade area between -1 and 1
z <- seq(-4, 4, length.out = 1000)
df <- data.frame(z = z, dens = dnorm(z))

z_left  <- -1
z_right <-  1

ggplot(df, aes(z, dens)) +
  geom_line(size = 0.7) +
  geom_ribbon(
    data = subset(df, z >= z_left & z <= z_right),
    aes(ymin = 0, ymax = dens),
    alpha = 0.4
  ) +
  geom_vline(xintercept = c(z_left, z_right), linetype = "dashed") +
  labs(
    x = "z",
    y = "Density",
    title = "Standard Normal: shaded area = P(-1 < Z < 1)"
  ) +
  annotate("text", x = 0, y = dnorm(0)*0.6, label = "P(-1 < Z < 1)")

### optional (will be done on whiteboard in class) ###
# probabilities
p_left  <- pnorm(z_left)            # P(Z < -1) -> that's the behavior of the function, it calculates the Area Under the Curve (AUC) on the left of the value
# you could calculate the AUC on the right side by using pnorm(z_left, lower.tail = F) -> check what the default behavior is in the help?
p_right <- pnorm(z_right)           # P(Z <  1)
p_mid   <- p_right - p_left         # P(-1 < Z < 1)

# build a long data frame with which area to shade in each panel
df_long <- bind_rows(
  df %>% mutate(panel = "P(Z < 1)",     shade = ifelse(z <= z_right, dens, NA_real_)),
  df %>% mutate(panel = "P(Z < -1)",    shade = ifelse(z <= z_left,  dens, NA_real_)),
  df %>% mutate(panel = "P(-1 < Z < 1)",shade = ifelse(z >= z_left & z <= z_right, dens, NA_real_))
)

# labels to annotate each facet with its numeric value
ann <- data.frame(
  panel = c("P(Z < 1)", "P(Z < -1)", "P(-1 < Z < 1)"),
  x     = c(-3.7, -3.7, -3.7),
  y     = c(dnorm(0)*0.9, dnorm(0)*0.9, dnorm(0)*0.9),
  lab   = c(
    sprintf("= %.4f", p_right),
    sprintf("= %.4f", p_left),
    sprintf("= %.4f", p_mid)
  )
)

ggplot(df_long, aes(z, dens)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = 0, ymax = shade), alpha = 0.35, na.rm = TRUE) +
  geom_vline(xintercept = c(z_left, z_right), linetype = "dashed") +
  facet_wrap(~ panel, nrow = 3) +
  labs(
    x = "z",
    y = "Density",
    title = "Areas under the Standard Normal: P(Z<1), P(Z<-1), and their difference P(-1<Z<1)"
  ) +
  geom_text(data = ann, aes(x = x, y = y, label = lab), inherit.aes = FALSE)


## 2) Compute the probability using pnorm()

# (i) Standard normal version: P(-1 < Z < 1)
p_std <- pnorm(1) - pnorm(-1)        # same as 2*pnorm(1) - 1 (mathematical property of the symmetry, you can draw it by hand)
p_std

# (ii) Equivalent computation on any N(mu, sigma^2): P(mu - sigma < X < mu + sigma) 
# >> just to show you that you can use these functions directly without calculating a z-score in case you're working with "real data"
mu    <- 10
sigma <- 3
p_x <- pnorm(mu + sigma, mean = mu, sd = sigma) -
  pnorm(mu - sigma, mean = mu, sd = sigma)
p_x

## b, (or by extension, c & d) can be calculated by replacing the 'sigma' value here:
p_std <- pnorm(1.96) - pnorm(-1.96)        # same as 2*pnorm(1.96) - 1
p_std

p_std <- pnorm(3) - pnorm(-3)       

p_std <- pnorm(0.67) - pnorm(-0.67)
p_std

# >> in the next example we go from the probability to the z-score

#### 4.10 ####

# Find the z-value for which the probability that a normal variable exceeds μ + zσ equals 
# (a) 0.01, (b) 0.025, (c) 0.05, (d) 0.10, (e) 0.25, (f) 0.50

qnorm(1 - 0.01) # this is the simple way of doing it 
# note that the behavior is the area under the curve on the left of the computed quantile

# next I propose a visualisation + a way to do it for multiple values

# Target tail probabilities
alpha <- c(0.01, 0.025, 0.05, 0.10, 0.25, 0.50)

# z such that P(Z > z) = alpha
z <- qnorm(1 - alpha)

# Quick table (z rounded) + check via pnorm()
res <- data.frame(alpha = alpha, z = round(z, 3),
                  check = round(pnorm(z, lower.tail = FALSE), 3))
print(res)

# - Plot: shade right-tail area α in each panel 
zgrid <- seq(-4, 4, length.out = 2000)
base  <- data.frame(z = zgrid, dens = dnorm(zgrid))

# Build a small dataframe for facets (one per alpha)
labels <- paste0("α = ", alpha, "  (z = ", sprintf("%.2f", z), ")")
plot_df <- do.call(rbind, lapply(seq_along(alpha), function(i) {
  df <- base
  df$panel <- labels[i]
  df$shade <- ifelse(df$z >= z[i], df$dens, NA_real_)
  df
}))
vlines <- data.frame(panel = labels, x = z)

ggplot(plot_df, aes(z, dens)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = shade), alpha = 0.35, na.rm = TRUE) +
  geom_vline(data = vlines, aes(xintercept = x), linetype = "dashed") +
  facet_wrap(~ panel, nrow = 2) +
  labs(title = "Standard Normal: P(Z > z) = α",
       x = "z", y = "Density") +
  theme_minimal()

#### Normal distribution + z-score for a proportion variable (votes for Brown) ####

# example 4.11 from the book (in the course section, not the exercices)

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

##### example (try to think how to do it yourself) #####
# ?> how would you visualize the likelihood of the population's sample mean to be 0.6 (if the observed sample is 0.6, with n = 1824)

p_hat <- 0.605 # observed probability in sample
p0 <- 0.6 # theoretical probability from population if votes are spread 60-40
n <- 1824 # pop size

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n) # formula for proportion z-score, no direct way of doing this for a proportion in R (we'll cover it in the Chi2 chapters)

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

#### Normal distribution + z-score for a continuous variable (heights) ####
# made up example
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
# using the same population from the previous example
# Draw a sample of n individuals and visualize whether its mean differs from mu

n <- 36
samp <- sample(heights$height, size = n, replace = TRUE) # that's the sample, you can print(samp)
xbar <- mean(samp) # observed sample mean

## >> Big underlying question: what is the likelihood of observing a mean that's THAT different, when sampling from that population randomly? 

# if the sample comes from the underlying distribution: mean = mu with known sigma, the sampling distribution is Normal(mu, sigma/sqrt(n))
se <- sigma / sqrt(n)
z_xbar <- (xbar - mu) / se # calculate the z-score for that sample
p_two_mean <- 2 * (1 - pnorm(abs(z_xbar))) # and the two-sided area under the curve for a given z-score 
# (we don't care whether it's larger or smaller, in this example) 

xbar
z_xbar
p_two_mean
diffMu <- abs(xbar - mu) # absolute difference in means

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

### let's visualise what happens if we run this 20 times

# install.packages('patchwork')
library(patchwork)

# this is a function that allows me to sample the population with a given mu, sigma, and n, so I can do that multiple times

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
    annotate("text", x = xbar - 0.01*xbar, y = ymax * 0.9,
             label = paste0("Observed x̄ = ", round(xbar, 2), " cm"),
             color = "red", hjust = 0) +
    annotate("text", x = mu - 0.5 * se, y = ymax * 0.55,
             label = paste0("z = ", round(z_xbar, 2),
                            "\nTwo-sided p = ", signif(p_two_mean, 3)))+
    # xlim(150,180)+ # could help see the difference when playing with different sigma & n -> (Ctrl + Shift + C) to comment/uncomment
    theme_minimal()
}

# - build N plots as a list, calling the function inside replicate (here, I do it 12 times, it is the first argument of the following code)
plots <- replicate(
  12,                                                  # how many plots you want
  creatingPlot(heights_vec = heights$height, mu = mu, sigma = sigma, n = 2),
  simplify = FALSE
)

# - A) 5 x 4 grid on one panel -
# using patchwork
wrap_plots(plots, nrow = 3, ncol = 4) # plotting all together

## >> Reminder -- Big underlying question: what is the likelihood of observing a mean that's THAT different, when sampling from that population randomly? 

# >> If you would run this simulation enough times, you'd have some instances where the sampled mean was quite different from the population. It is less likely, but still possible. 

# >> remember that we are using mu and sigma from a population (we assume we know these), normally you'd have to use an estimate of that, based on your sample(s)
# ?> question: what happens if I change sigma, or n? Are z, and p, going to be larger, smaller, or the same?

# >>! important note: when estimating the mean from our sample when doing hypothesis testing, we don't consider the distribution of mean to be a perfect gaussian. 
# >> Rather, we use a more conservative distribution that 



