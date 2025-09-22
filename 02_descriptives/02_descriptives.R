# Let's take the examples from the book, 3.22, 3.23 & 3.74

library(tidyverse) # for data handling (dplyr), the pipe operator, and plotting (ggplot2)

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

# (b) Calculate the standard deviation for each region
sd_europe <- sd(europe)
sd_africa <- sd(africa)

# equivalent to doing it 'by hand':
dev <- europe - mean(europe) # this would be the deviations from the mean (see how we have one value per country) -> print(dev)
ssd <- sum(dev^2) # sum of squares dev
variance <- ssd / (length(europe)-1)
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
# (ii) 95%, (iii) all or nearly all salaries

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

ggplot(sim_data, aes(x = score, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  # Add mean line for U.S. citizens
  geom_vline(xintercept = us_mean, linetype = "dashed", size = 1, color = viridis(2)[2]) + # could also use color = "red" for example
  # Add mean line for Non-U.S. citizens
  geom_vline(xintercept = nonus_mean, linetype = "dashed", size = 1, color = viridis(2)[1])


# density plot with labels, theme and color scale
library(viridis)
ggplot(sim_data, aes(x = score, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_viridis(discrete = T)+
  labs(title = "Simulated GRE Quantitative Distributions",
       x = "GRE Quantitative Score",
       y = "Density",
       fill = "Group") +
  theme_minimal()

