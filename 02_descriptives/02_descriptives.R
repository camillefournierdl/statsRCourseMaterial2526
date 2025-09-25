##### General comment:
### I won't go over the basics of ggplot in this class, but we use it for visualisation.
### If you already understand the basics of the current code, # but not the ggplot parts,
### here is an excellent introduction on how to use ggplot for data visualisation: https://r4ds.hadley.nz/data-visualize.html
### I will quickly go over this ggplot material in a future course

# solution from the quiz (in-class exercice)

dataset <- c(20, 40, 5, 15, 10, 25, 20, 35, 30)

mean(dataset)
median(dataset)
mode(dataset) # doesn't do that... check the help() function!
# try googling it! how to calculate a mode in R?
# we use a "estimate_mode" function later, that I got from ChatGPT

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(dataset)

sd(dataset) # is this using n or n-1? what's the difference?

# what if I add an outlier? see last optional example below
# an exercice could be to edit the dataset including the outlier (75) and calculate the descriptives again

# Now, let's take the examples from the book, 3.22, 3.23 & 3.74

library(tidyverse) # for data handling (dplyr), the pipe operator, and plotting (ggplot2)

# to make sure you're using function from a given library, use dplyr::filter()

# ------------ 3.22 ------------
# A report published by a survey agency on the number of female workers (in millions) 
# in each country revealed the following data. 
#
# For Western Europe, the values were as follows: 
#   - Germany: 86 
#   - Norway: 89 
#   - France: 72 
#   - Ireland: 77 
#   - Finland: 81 
#   - Greece: 85 
#   - United Kingdom: 89 
#   - Belgium: 73 
#   - Italy: 79 
#   - Denmark: 83 
#
# For Africa, the values reported were as follows: 
#   - Congo: 32 
#   - Sudan: 12 
#   - Botswana: 11 
#   - Kenya: 36 
#   - Ghana: 22 
#   - Zimbabwe: 27 
#   - Madagascar: 12 
#   - Zambia: 38

# a) Calculate the mean for the two sets of nations. Which
# set of nations has a higher number of female workers?

# b) Find the standard deviation for the two sets of nations.
# Which set has a larger spread of scores and why?


# create a vector for both continents
# Western Europe data
europe <- c(Germany = 86, Norway = 89, France = 72, Ireland = 77, 
            Finland = 81, Greece = 85, UK = 89, Belgium = 73, 
            Italy = 79, Denmark = 83)

# Africa data
africa <- c(Congo = 32, Sudan = 12, Botswana = 11, Kenya = 36, 
            Ghana = 22, Zimbabwe = 27, Madagascar = 12, Zambia = 38)


# (a) Calculate the mean for each region
mean_europe <- mean(europe)
mean_africa <- mean(africa)


# equivalent to sum(europe)/length(europe)
mean_europe
mean_africa

# (b) Calculate the standard deviation for each region, if this is a sample
sd_europe <- sd(europe)
sd_africa <- sd(africa)

# equivalent to doing it 'by hand':
dev <- europe - mean(europe) # this would be the deviations from the mean (see how we have one value per country) -> print(dev)
ssd <- sum(dev^2) # sum of squares dev
variance <- ssd / (length(europe)-1) #n-1 if this is a sample
sqrt(variance)

sd_europe
sd_africa

# putting everything in a dataframe for visualization
df <- data.frame(
  region = c(rep("Europe", length(europe)), rep("Africa", length(africa))),
  country = c(names(europe), names(africa)),
  workers = c(europe, africa)
)

# Boxplot to compare distributions
ggplot(df, aes(x = region, y = workers, fill = region)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.7) +   # add points for each country
  theme_minimal()

ggplot(df, aes(x = region, y = workers, fill = region)) +
  geom_boxplot(alpha = 0.6, notch = T) +
  geom_jitter(width = 0.1, alpha = 0.7) +   # add points for each country
  labs(title = "Number of Female Workers (in millions)",
       y = "Female workers (millions)",
       x = "Region") +
  theme_minimal()

# from the ?help of geom_boxplot:
# In a notched box plot, the notches extend 1.58 * IQR / sqrt(n). 
# This gives a roughly 95% confidence interval for comparing medians. See McGill et al. (1978) for more details. 

# it is interesting to note that this is different from using SD.

# ------------ 3.23 ------------
# A report indicates that teacher’s total annual pay
# (including bonuses) in Toronto, Ontario, has a mean of
# $61,000 and standard deviation of $10,000 (Canadian dollars).
# Suppose the distribution has approximately a bell shape.

# (a) Give an interval of values that contains about (i) 68%,
# (ii) 95%, (iii) or nearly all salaries

# (b) Would a salary of $100,000 be unusual? Why?

# we do not know what the total population is, so we will use mean and sd only to calculate intervals with the Empirical Rule.

# Parameters
mean_salary <- 61000   # mean annual pay
sd_salary   <- 10000   # standard deviation

# About 68% of the data fall within 1 standard deviation of the mean
interval_68 <- c(mean_salary - 1*sd_salary,
                 mean_salary + 1*sd_salary)

# About 95% of the data fall within 2 standard deviations of the mean
interval_95 <- c(mean_salary - 2*sd_salary,
                 mean_salary + 2*sd_salary)

# About 99.7% (all or nearly all) fall within 3 standard deviations of the mean
interval_997 <- c(mean_salary - 3*sd_salary,
                  mean_salary + 3*sd_salary)

interval_68
interval_95
interval_997

# we could already answer the question based on our confidence intervals
salary_check <- 100000

# Calculate how many SDs away from the mean (the z-score)
z_score <- (salary_check - mean_salary) / sd_salary
z_score

# what do think? how unusual?

# ------------------------------------------
# Optional visualization
# ------------------------------------------

set.seed(123)  # for reproducibility

# Create a grid of salary values to plot the normal curve
salaries <- seq(30000, 110000, by = 100)
density_vals <- dnorm(salaries, mean = mean_salary, sd = sd_salary)

df <- data.frame(salary = salaries, density = density_vals)

ggplot(df, aes(x = salary, y = density)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = mean_salary, color = "black", linetype = "dashed") +
  geom_vline(xintercept = salary_check, color = "red", linetype = "dotted", size = 1) +
  labs(title = "Normal Curve for Teacher Salaries in Toronto",
       x = "Salary (CAD)",
       y = "Density") +
  theme_minimal()


# ------------ 3.74 ------------

set.seed(123)  # for reproducibility

# Parameters
n <- 5000  # sample size for simulation
us_mean <- 529
us_sd   <- 127
nonus_mean <- 649
nonus_sd   <- 129

# Create a data frame of simulated GRE scores for U.S. and Non-U.S. citizens
sim_data <- data.frame(
  # Combine simulated scores for both groups into one long vector
  score = c(rnorm(n, mean = us_mean, sd = us_sd),        # simulate U.S. scores
            rnorm(n, mean = nonus_mean, sd = nonus_sd)), # simulate Non-U.S. scores
  
  # Create a group label (repeats "U.S. citizens" n times, then "Non-U.S. citizens" n times)
  group = rep(c("U.S. citizens", "Non-U.S. citizens"), each = n)
)

# Look at the raw simulated score values
sim_data$score

# Plot a histogram of ALL scores together (ignores grouping)
hist(sim_data$score)

# Extract only the rows where group == "U.S. citizens"
sim_data[sim_data$group == "U.S. citizens",]

# Equivalent way to do the same thing using the subset() function
subset(sim_data, group == "U.S. citizens")

# Extract just the GRE score column for U.S. citizens
sim_data[sim_data$group == "U.S. citizens",]$score

# Equivalent using subset()
subset(sim_data, group == "U.S. citizens")$score

# Histogram of GRE scores only for U.S. citizens
hist(sim_data[sim_data$group == "U.S. citizens",]$score)

# Equivalent histogram using subset()
hist(subset(sim_data, group == "U.S. citizens")$score)

# Histogram of GRE scores only for Non-U.S. citizens
hist(sim_data[sim_data$group == "Non-U.S. citizens",]$score)

# --- Using ggplot2 for cleaner visualization ---

# Overlay histograms of the two groups (transparent so they overlap)
ggplot(sim_data, aes(x = score, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity")

# Place the histograms side by side instead of overlapping, what do you think?
ggplot(sim_data, aes(x = score, fill = group)) +
  geom_histogram(alpha = 0.5, position = "dodge")

library(viridis) # color scale
ggplot(sim_data, aes(x = score, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  # Add mean line for U.S. citizens
  geom_vline(xintercept = us_mean, linetype = "dashed", size = 1, color = viridis(2)[2]) + # could also use color = "red" for example
  # Add mean line for Non-U.S. citizens
  geom_vline(xintercept = nonus_mean, linetype = "dashed", size = 1, color = viridis(2)[1])

# density plot with labels, theme and color scale
ggplot(sim_data, aes(x = score, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis(discrete = T)+
  labs(title = "Simulated GRE Quantitative Distributions",
       x = "GRE Quantitative Score",
       y = "Density",
       fill = "Group") +
  theme_minimal()

# ------------ (optional) Mean vs Median vs Mode ------------
# This section was initially written by ChatGPT and reviewed by Camille F.

# Helper: estimate the mode for *continuous* data using the peak of a kernel density -> there are other ways of calculating the mode of a distribution, especially for bimodal examples
estimate_mode <- function(x) {
  d <- density(x, na.rm = TRUE)
  d$x[which.max(d$y)]
}

# 1) Build the three datasets
n <- 1000 # nb of rows (observations)

# also try with smaller n
# n <- 100

# Symmetric normal
x_sym <- rnorm(n, mean = 50, sd = 10)

# Right-skewed: log-normal-ish (exponentiate a normal)
x_skew <- rlnorm(n, meanlog = log(40), sdlog = 0.4)

# Bimodal: mixture of two normals
x_bi <- c(rnorm(n/2, mean = 40, sd = 6),
          rnorm(n/2, mean = 70, sd = 6))

# Put them in one long tibble, with a label for each scenario
# note: here tibble is equivalent to data.frame()
df <- bind_rows(
  tibble(value = x_sym,  scenario = "Symmetric (Normal)"),
  tibble(value = x_skew, scenario = "Right-skewed"),
  tibble(value = x_bi,   scenario = "Bimodal")
)

# equivalent to a more common form: 
# df1 <- data.frame(value = x_sym,  scenario = "Symmetric (Normal)")
# df2 <- data.frame(value = x_skew,  scenario = "Right-skewed")
# df3 <- data.frame(value = x_bi,  scenario = "Bimodal")
# df <- rbind(df1, df2, df3)

# 2) Compute mean, median, mode per scenario
# this code here is using dplyr, we can spend some time explaining how it works if we have the time
stats <- df %>%
  group_by(scenario) %>%
  summarize(
    mean   = mean(value),
    median = median(value),
    mode   = estimate_mode(value),     # density-based mode estimate
  ) %>%
  pivot_longer(cols = c(mean, median, mode),
               names_to = "stat", values_to = "value")

# We'll order the legend nicely
stats$stat <- factor(stats$stat, levels = c("mean", "median", "mode")) # factors can be useful to treat categorical data

# 3) Plot: histogram + density + vertical lines for mean/median/mode
# explain different steps of the graph if we have time, otherwise ggplot explanation next week?
ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.35) +
  geom_density(linewidth = 1) +
  # Draw the three reference lines from the stats table (enables a clean legend)
  geom_vline(data = stats,
             aes(xintercept = value, color = stat),
             linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c(mean = "#1f77b4", median = "#2ca02c", mode = "#d62728"),
                     name = "Statistic",
                     labels = c("Mean", "Median", "Mode (estimated)")) +
  facet_wrap(~ scenario, scales = "free", ncol = 1) +
  labs(title = "Mean vs Median vs Mode across different shapes",
       x = "Value", y = "Density") +
  theme_minimal(base_size = 12)

# 4) (Optional) Print the numeric summaries for discussion
stats %>%
  pivot_wider(names_from = stat, values_from = value) %>% # opposite of pivot_longer
  arrange(scenario) %>%
  print(n = Inf)


# ------------ (optional) Mean vs Median vs Mode, impact of outliers ------------

# 1) Build the three datasets
# n <- 1000 # nb of rows (observations)

# also try with smaller n
n <- 100

# Symmetric normal
x_sym <- rnorm(n, mean = 50, sd = 10)

# Right-skewed: log-normal-ish (exponentiate a normal)
x_skew <- rlnorm(n, meanlog = log(40), sdlog = 0.4)

# Bimodal: mixture of two normals
x_bi <- c(rnorm(n/2, mean = 40, sd = 6),
          rnorm(n/2, mean = 70, sd = 6))

# Put them in one long tibble, with a label for each scenario
# note: here tibble is equivalent to data.frame()

#### could run this variation here to look at the impact of outliers:
df <- bind_rows(
  tibble(value = x_sym,  scenario = "1. Symmetric (Normal)"),
  tibble(value = c(x_sym, 200), scenario = "2. One outlier"),
  tibble(value = c(x_sym, 150, 200, 500),   scenario = "3. Three larger outliers")
)

# 2) Compute mean, median, mode per scenario
# this code here is using dplyr, we can spend some time explaining how it works if we have the time
stats <- df %>%
  group_by(scenario) %>%
  summarize(
    mean   = mean(value),
    median = median(value),
    mode   = estimate_mode(value),     # density-based mode estimate
  ) %>%
  pivot_longer(cols = c(mean, median, mode),
               names_to = "stat", values_to = "value")

# We'll order the legend nicely
stats$stat <- factor(stats$stat, levels = c("mean", "median", "mode")) # factors can be useful to treat categorical data

# 3) Plot: histogram + density + vertical lines for mean/median/mode
# explain different steps of the graph if we have time, otherwise ggplot explanation next week?
ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.35) +
  geom_density(linewidth = 1) +
  # Draw the three reference lines from the stats table (enables a clean legend)
  geom_vline(data = stats,
             aes(xintercept = value, color = stat),
             linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c(mean = "#1f77b4", median = "#2ca02c", mode = "#d62728"),
                     name = "Statistic",
                     labels = c("Mean", "Median", "Mode (estimated)")) +
  facet_wrap(~ scenario, scales = "free", ncol = 1) +
  labs(title = "Mean vs Median vs Mode across different shapes",
       x = "Value", y = "Density") +
  xlim(10, 90)+
  theme_minimal(base_size = 12)

# 4) (Optional) Print the numeric summaries for discussion
stats %>%
  pivot_wider(names_from = stat, values_from = value) %>% # opposite of pivot_longer
  arrange(scenario) %>%
  print(n = Inf)

