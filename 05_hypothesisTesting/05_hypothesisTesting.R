library(tidyverse)
library(patchwork)

set.seed(123)

# Confidence Intervals & Hypothesis Testing (Student’s t)

#### Toolbox with main functions ####

##### Confidence Intervals #####

### One sample distribution: CIs of a mean
# Assumes independent and identically distributed (i.d.d) draws; uses t with length(x)-1 df and sd(x)/sqrt(n).
# t.test(x, conf.level = 0.95)

ci <- t.test(iris$Sepal.Width, conf.level = 0.95)$conf.int
ci                        # [lower, upper]
ci[1]; ci[2]              # extract bounds

# equivalent to:
x  <- iris$Sepal.Width
n  <- length(x)
m  <- mean(x)
se <- sd(x) / sqrt(n)
tcrit <- qt(0.975, df = n - 1) # similar to qnorm we used last week, this time we use the student distribution instead of a normal distribution
c(m - tcrit*se, m + tcrit*se)

# >> already relevant: the ci is dependent on the distribution we want to draw the z-scores from (and the se calculation) -> assumptions!!

### For a one-sample proportion
# x = number of successes, n = trials
# prop.test(x, n, conf.level = 0.95)$conf.int   # CI with continuity correction
prop.test(1104, 1824, conf.level = 0.95)$conf.int   # >> look at the help function!

### For a two-sample difference in mean
# t.test(x1, x2, var.equal = FALSE)$conf.int
x1 <- c(2, 7, 2, 8, 10, 5)
x2 <- c(5, 8, 7, 9, 10, 10)

# Welch CI (unequal variances, default)
t.test(x1, x2, var.equal = FALSE)$conf.int

# Student (pooled) CI
t.test(x1, x2, var.equal = TRUE)$conf.int

# Welch = no assumption of equal variance;
# Student = assumes equal variance, slightly narrower CI if true.

# paired t.test

# Difference of paired observations, e.g. before vs after
t.test(x1, x2, paired = TRUE)$conf.int
# or
t.test(x1 - x2)$conf.int   # equivalent
# Assumes differences are approximately normal.

##### Hypothesis Testing: Student’s t #####

### One-sample t-test ###
# Tests whether the sample mean differs from a hypothesized population mean (μtest)
# Assumes i.i.d. draws and approximate normality of the sampling distribution.

# t.test(x, mu = μtest)
t.test(iris$Sepal.Width)   # H0: mean = 0, mean = 0, two-sided alternative by default
t.test(iris$Sepal.Width, mu = 3)   # H0: mean = 3, two-sided alternative by default

### Reading the output ###
# Each t.test() call reports:
#   t = test statistic
#   df = degrees of freedom
#   p-value = probability under H₀
#   confidence interval = range of plausible values for the true difference
#   sample estimates = sample means (and differences)
#
# Note: the CI and the p-value are two views of the same inference.
# If the CI excludes μtest (or 0 for a difference), the p-value < α.

# >> output includes:
#   statistic = t_obs = (mean(x) - μtest) / (sd(x)/√n)
#   parameter = degrees of freedom (n-1)
#   p.value = P(|T| ≥ |t_obs|) under H0
#   conf.int = the 95% CI for the mean (same as in previous section!)

# Check a one-sided alternative:
t.test(iris$Sepal.Width, mu = 3, alternative = "greater")  # H1: mean > 3
t.test(iris$Sepal.Width, mu = 3, alternative = "less")     # H1: mean < 3

# equivalent manual calculation
x  <- iris$Sepal.Width
n  <- length(x)
m  <- mean(x)
se <- sd(x)/sqrt(n)
t_obs <- (m - 3)/se
pval  <- 2*pt(-abs(t_obs), df = n-1)
t_obs; pval

### Two-sample t-tests ###
# Compare means from two independent groups.
# t.test(x1, x2, var.equal = FALSE)  -> Welch test (default, unequal variances)
# t.test(x1, x2, var.equal = TRUE)   -> Student test (equal variances assumed)

x1 <- c(2, 7, 2, 8, 10, 5)
x2 <- c(5, 8, 7, 9, 10, 10)

t.test(x1, x2)                           # Welch two-sided test
t.test(x1, x2, var.equal = TRUE)         # Student (pooled) version

# One-sided alternatives:
t.test(x1, x2, alternative = "greater")  # H1: mean(x1) > mean(x2)
t.test(x1, x2, alternative = "less")     # H1: mean(x1) < mean(x2)

# Equivalent manual computation for Welch’s t
# n1 <- length(x1); n2 <- length(x2)
# s1 <- sd(x1); s2 <- sd(x2)
# m1 <- mean(x1); m2 <- mean(x2)
# SE_diff <- sqrt(s1^2/n1 + s2^2/n2)
# t_obs <- (m1 - m2)/SE_diff
# df_welch <- (s1^2/n1 + s2^2/n2)^2 /
#   ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
# pval <- 2*pt(-abs(t_obs), df = df_welch)
# t_obs; df_welch; pval

### Paired t-test ###
# Used when observations are matched (before–after, twin studies, etc.)
# Tests whether the mean difference (x1 - x2) is zero.

before <- c(3.2, 4.1, 3.8, 3.5, 4.0)
after  <- c(3.6, 4.4, 4.1, 3.7, 4.3)

t.test(before, after, paired = TRUE)     # two-sided by default
t.test(before, after, paired = TRUE, alternative = "greater")  # one-sided

# >> what are the assumptions of all these different tests? t.test with var.equal = T, t.test with var.equal = F?
# -> alternatives can be bootstraps, permutations, or non-parametric tests

# bonus example of a bootstrap ci
B <- 2000
boot_means <- replicate(B, mean(sample(iris$Sepal.Width, replace = TRUE)))
quantile(boot_means, c(0.025, 0.975))         # Percentile CI
# here is a great illustration of a permutation test for 2 samples: https://www.jwilber.me/permutationtest/


#### Comparing samples ####

irisData <- iris

plot(irisData)

# compare populations

irisData %>% 
  ggplot(aes(y = Sepal.Width, fill = Species))+
  geom_boxplot()

irisData %>% 
  ggplot(aes(x = Sepal.Width, fill = Species))+
  geom_histogram(position = "identity")+
  facet_wrap(~Species, nrow = 3)

means <- irisData %>% 
  group_by(Species) %>% 
  summarise(m = mean(Sepal.Width, na.rm = TRUE))

irisData %>% 
  ggplot(aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.9) +
  facet_wrap(~Species, nrow = 3, scales = "free_y") +
  geom_vline(data = means, aes(xintercept = m, colour = Species),
             linetype = "dashed")

# how do we test whether two species have a different mean for that variable in the example?
# essentially we do an inference on the distribution of the mean of the original population from which these samples were drawn from:

# use the help to look at what t.test() does (e.g. what is the default behavior & assumptions? paired samples? equal variance?)
# >> t.test isn't actually a simple student t-test by default...

t.test(irisData %>% #sample1
         filter(Species == "versicolor") %>% 
         pull(Sepal.Width),
       irisData %>% #sample2
         filter(Species == "virginica") %>% 
         pull(Sepal.Width)
       )

t.test(irisData %>% #sample1
         filter(Species == "virginica") %>% 
         pull(Sepal.Width),
       irisData %>% #sample2
         filter(Species != "virginica") %>% 
         pull(Sepal.Width)
)

t.test(irisData %>% #sample1
         filter(Species == "virginica") %>% 
         pull(Sepal.Width),
       irisData %>% #sample2
         filter(Species != "virginica") %>% 
         pull(Sepal.Width),
       alternative = "greater"
)

t.test(irisData %>% #sample1
         filter(Species == "virginica") %>% 
         pull(Sepal.Width),
       irisData %>% #sample2
         filter(Species != "virginica") %>% 
         pull(Sepal.Width),
       alternative = "less"
)

# >> it is fairly easy to run a t.test (welch, proportion, or other 1 or 2 sample, one or two sided tests)
# >> what might be more complicated, is to understand what a test actually does. 
# >> the following code, and resulting plots are meant to explore the concepts that range from
# >> the description of 2 plant samples (and the width of their sepal)
# >> to the test statistic that evaluates how likely the two samples are coming from different populations.

# >> I would like to spend some time discussing the final plot. If one concept is not clear to you, maybe the code can help clarify it. 
# >> if the code doesn't help, you can talk to us, go back to Agresti, or use a ChatBot to explain it. With, or without the code.

#### Visualizing how a t.test works #### 
# pick the two species to compare
pair <- c("virginica","versicolor")

dat2 <- irisData %>% filter(Species %in% pair)

# Welch t-test on Sepal.Width
t.test(Sepal.Width ~ Species, data = dat2)

# Per-species summaries
summ <- dat2 %>% 
  group_by(Species) %>% 
  summarise(n = dplyr::n(),
            mean = mean(Sepal.Width),
            sd = sd(Sepal.Width),
            se = sd/sqrt(n),
            df = n - 1)

p0 <- dat2 %>% 
  ggplot(aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 15) +
  geom_vline(data = summ, aes(xintercept = mean, colour = Species),
             linetype = "dashed", linewidth = 1)+
  labs(title= "Descriptives: Actual Data from the sample for 2 species",
       y = "Count")

p0

# 1) Per-species sampling distribution of the mean:
#    X = mean + t * se,   f_X(x) = dt(t, df)/se
tgrid <- tibble(t = seq(-4, 4, length.out = 501))
mean_dens <- summ %>% 
  crossing(tgrid) %>%
  mutate(x = mean + t*se,
         density = dt(t, df)/se)

p1 <- ggplot(mean_dens, aes(x = x, y = density, color = Species)) +
  geom_line(linewidth = 1) +
  geom_vline(data = summ, aes(xintercept = mean, color = Species), linetype = "dashed") +
  labs(title = "Inference: Sampling distribution of the mean (per species)\n-> using the t distribution (could show CIs)",
       x = "Sepal.Width (mean scale)", y = "Density") +
  theme_minimal()

p1

# 2) Sampling distribution of the mean difference (Welch):
#    D = (m1 - m2) + t * SE_diff,  df = Welch-Satterthwaite
s1 <- summ %>% filter(Species == pair[1])
s2 <- summ %>% filter(Species == pair[2])

SE_diff <- sqrt(s1$sd^2/s1$n + s2$sd^2/s2$n)
diff_hat <- s1$mean - s2$mean

# Welch df
df_welch <- (s1$sd^2/s1$n + s2$sd^2/s2$n)^2 /
  ((s1$sd^2/s1$n)^2/(s1$n-1) + (s2$sd^2/s2$n)^2/(s2$n-1))

tgrid2 <- tibble(t = seq(-4, 4, length.out = 1000))
diff_dens <- tgrid2 %>%
  transmute(x = diff_hat + t*SE_diff,
            density = dt(t, df = df_welch)/SE_diff)

# 95% CI via Welch t
tcrit   <- qt(0.975, df = df_welch)
ci_low  <- diff_hat - tcrit * SE_diff
ci_high <- diff_hat + tcrit * SE_diff

p2 <- ggplot(diff_dens, aes(x = x, y = density)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dotted") +      # null difference
  geom_vline(xintercept = diff_hat, linetype = "dashed") +# observed diff
  geom_vline(xintercept = c(ci_low, ci_high), linetype = "longdash") +  # 95% CI
  annotate("text",
           x = diff_hat, y = max(diff_dens$density)*0.6,
           vjust = -0.6, label = sprintf("obs diff = %.3f", diff_hat)) +
  annotate("text",
           x = (ci_low + ci_high)/2, y = max(diff_dens$density)*0.6,
           vjust = 1.8, label = sprintf("95%% CI = [%.3f, %.3f]", ci_low, ci_high)) +
  labs(
    title = "Inference: confidence interval of the difference between the 2 species",
    subtitle = "Estimator-centered sampling distribution (Delta-hat + t * SE)",
    x = "Difference in means (Sepal.Width)", y = "Density"
  ) +
  theme_minimal()

p2

t_obs <- diff_hat / SE_diff
pval  <- 2 * pt(-abs(t_obs), df = df_welch)

tgrid3 <- tibble(t = seq(-5, 5, length.out = 2001))
null_dens <- tgrid3 %>%
  transmute(
    t = t,
    density = dt(t, df = df_welch) / SE_diff  # change-of-variables
  )

p3 <- ggplot(null_dens, aes(x = t, y = density)) +
  geom_line(linewidth = 1) +
  geom_area(data = subset(null_dens, t >= t_obs), fill = "red", alpha = 0.25) +
  geom_area(data = subset(null_dens, t <= -t_obs), fill = "red", alpha = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted") +        # H0 center
  geom_vline(xintercept = t_obs, linetype = "dashed") + # observed diff
  labs(
    title = "Inference + Test: Null distribution of the t-statistic (Welch)",
    subtitle = sprintf("t_obs = %.2f, df = %.1f, two-sided p = %.3g",
                       t_obs, df_welch, pval),
    y = "Density"
  ) +
  theme_minimal()

wrap_plots(p0, p1, p2, p3, ncol = 2)

# >> once we have understood the different steps going through this visualization, we can be more comfortable to just type in a t.test(sample1, sample2) line ;)....
t.test(irisData %>% #sample1
         filter(Species == "versicolor") %>% 
         pull(Sepal.Width),
       irisData %>% #sample2
         filter(Species == "virginica") %>% 
         pull(Sepal.Width)
)



