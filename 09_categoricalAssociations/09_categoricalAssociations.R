library(tidyverse)

theme_set(theme_minimal()) # set default theme for the session

### Chi-squared test
## Can be used to test whether 2 distributions match: in this example, whether the observed number of students that show up in class is uniformly distributed for a week

# create the objects that describe observed / theoretical distribution
observedCounts <- c(23, 16, 14, 19, 28)
uniformProbabilities <- c(0.20, 0.20, 0.20, 0.20, 0.20)

# visualisation of the data (in general should be done before the test)
expectedCounts <- uniformProbabilities * sum(observedCounts)

# Create a dataframe in long format
df <- data.frame(
  Day = rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 2),
  Counts = c(observedCounts, expectedCounts),
  Type = rep(c("Observed", "Expected"), each = length(observedCounts))
) %>% mutate(Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")))

# visualise the observed vs expected data 
ggplot(df, aes(x = factor(Day), y = Counts, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day", y = "Counts", title = "Observed vs Expected Counts") +
  theme_minimal()

# run the chi test
chisq.test(x = observedCounts, p = uniformProbabilities)

## Can also be used to test whether two categorical variables are independent, here we test whether voting (yes/no) and gender are related from one of the ESS surveys

# load ESS data: (similar examples as for the dplyr tutorial)
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv") # replace with relevant dataset for your project

##### ------- data cleaning ------- ######

ESS9_CH_proj <- ESS9_CH %>% 
  select(gndr, vote)

ESS9_CH_proj %>% plot()

# plot(ESS9_CH_proj)

ESS9_CH_filt <- ESS9_CH_proj %>% 
  filter(gndr < 9) %>% 
  filter(vote < 3)

plot(ESS9_CH_filt)

# test whether there are NAs
which(is.na(ESS9_CH_filt$gndr))

ESS9_CH_filt$gndrCHR <- ifelse(ESS9_CH_filt$gndr == 1, "Male", "Female")
ESS9_CH_filt$voteCHR <- ifelse(ESS9_CH_filt$vote == 1, "Yes", "No")

table(ESS9_CH_filt$gndrCHR, ESS9_CH_filt$voteCHR) 

chisq.test(table(ESS9_CH_filt$gndrCHR, ESS9_CH_filt$voteCHR)) # it is significant

chisq.test(table(ESS9_CH_filt$gndrCHR, ESS9_CH_filt$voteCHR))$residuals
# look at residuals (deviations from uniform distribution)

## Finally, we use a Kendall Tau as a better measure of correlation for variables that are categorical ordinal, such as some questions from our ess data

ESS9_CH_proj <- ESS9_CH %>% 
  select(psppipla, polintr) # "Political system allows people to have influence on politics" & "How interested in politics"

ESS9_CH_proj %>% plot()

# plot(ESS9_CH_proj)

ESS9_CH_filt <- ESS9_CH_proj %>% 
  filter(psppipla < 5) %>% # 1 to 5
  filter(polintr < 6) # 1 to 4

plot(ESS9_CH_filt)

cor.test(ESS9_CH_filt$psppipla, ESS9_CH_filt$polintr, method = "kendall", use = "complete.obs") # non parametric (less assumptions, often more valid for survey data), rank-based estimate



