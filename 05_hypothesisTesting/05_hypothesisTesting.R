library(tidyverse)
library(patchwork)

set.seed(123)

# Confidence intervals & hypothesis testing: Student's t

#### Confidence Intervals ####


#### Comparing samples ####

irisData <- iris

plot(irisData)

# visualise 2 populations (maybe use the iris dataset)

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

# welch test

#### Visualizing how a t.test works #### 
# pick the two species to compare
pair <- c("virginica","versicolor")

dat2 <- irisData %>% filter(Species %in% pair)

# Welch t-test on Sepal.Width
tt <- t.test(Sepal.Width ~ Species, data = dat2, var.equal = FALSE)

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


