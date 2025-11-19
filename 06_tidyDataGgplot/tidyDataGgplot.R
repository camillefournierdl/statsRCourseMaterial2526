library(tidyverse) # includes ggplot2 and dplyr, among others

set.seed(123)

# https://r4ds.hadley.nz/data-visualize.html # good ggplot tutorial


### using ggplot

iris

# >> Is there a relation between the length & the width of the iris Sepal?

# make the simplest plot (data + aesthetics)
# add complexity with a color aesthetics
# add a smooth line using geom_smooth()

ggplot(data = , aes())+
  geom_...()

ggplot(
  iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species)
)+
  geom_point()+
  geom_smooth(method= "lm")

versicolorIris <- iris %>% filter(Species == "versicolor")

ggplot(
  versicolorIris
)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width))+
  geom_smooth(aes(x = Sepal.Length, y = Sepal.Width),
              method= "lm", color = "green")

#+
  # geom_smooth(method = "lm")

# >> Are the length of the Petal & Sepal related to each other, in the iris dataset?

# You can use the slides here, the tutorial linked here and in the beginning of the code
# https://r4ds.hadley.nz/data-visualize.html, or stack overflow



##
mtcars # is this a long-format dataset?

# >> build the plot from the slides

ggplot(data = mtcars) +
  geom_point(mapping = aes(x = wt, y = mpg,
                           colour = disp,
                           size = hp))+
  labs(x = "Weight", y = "Miles per Gallon", color = "Displacement", size = "Horse Power",
       title = "Car efficiency based on weight, split by manual and automatic")+
  facet_wrap(~am)+
  theme_bw()


### using dplyr

# rename a variable
irisModified <- iris %>% 
  rename(petalLength = Petal.Length)

## tip! you can use Ctrl+Shift+M to print a pipe

# create a column
irisModified <- iris %>% 
  mutate(ratioPetalSepalLength = Petal.Length/Sepal.Length)

# select a column
irisSimplified <- iris %>% 
  select(Species, Sepal.Length, Sepal.Width)

# filter rows
irisSetosa <- iris %>% 
  filter(Species == "setosa")

# calculate summary statistics for different groups
irisSummary <- iris %>% 
  group_by(Species) %>% # grouping variable, summaries will be calculated for every level of that variables (works with multiple variables)
  summarize(mean_sepalLength = mean(Sepal.Length, na.rm=T),
            sd_sepalLength = sd(Sepal.Length, na.rm=T))

# you can combine commands together
irisModified <- iris %>% 
  mutate(ratioPetalSepalLength = Petal.Length/Sepal.Length) %>% 
  filter(Species == "setosa") %>% 
  summarize(mean_ratioLength = mean(ratioPetalSepalLength, na.rm=T)) %>% 
  pull(mean_ratioLength) # pull extracts the values in a column and prints them
  
# merge datasets together (actually from base, but is more intuitive than the dplyr options I.M.O)
# example, do not run
dfMerged <- merge(df1, df2, by = "columnid", all.x = T, all.y = T) # this would be a full merge, what's the default behavior? it is full because we keep all rows in x (df1) and y (df2)

# now we merge colors to the irisSummary data
dfCol <- data.frame(species = c("setosa", "versicolor", "virginica", "other"),
                 color = c("purple", "yellow", "green", "grey"))

# here, the name of the Species column is not written with the same capitalization
# a solution is to use dplyr to rename the column in dfCol 
# see how we do the rename pipe inside the merge function, which means we do not need to create a new object, dfCol is not modified in the memory

irisMerged <- merge(irisSummary, dfCol %>% 
                      rename(Species = species), # see how we rename first
                    by = "Species", all.x = T, all.y = T) # also full merge

irisMerged

irisMerged2 <- merge(irisSummary, dfCol %>% 
                      rename(Species = species),
                    by = "Species", all.x = T, all.y = F) # left merge (more common, default behavior)

irisMerged2

# make sure you understand the difference between irisMerged and irisMerged2

# >> let's select a few variables of interest for your project, and visualize their distribution (single variable, 2 variables and their measures of association)

theme_set(theme_minimal()) # set default theme for the session

# load ESS data:
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv") # replace with relevant dataset for your project

# select columns 
ESS9_CH_proj <- ESS9_CH %>% 
  select(prtclgch, trstprt, aesfdrk) #  prtclgch - Which party feel closer to, Switzerland --  trstprt - Trust in political parties --  aesfdrk - Feeling of safety of walking alone in local area after dark 

ESS9_CH_proj %>% plot()

# rename columns
ESS9_CH_proj <- ESS9_CH_proj %>% 
  rename(party = prtclgch,
         trustPol = trstprt,
         safety = aesfdrk)

# visualize one variable (and removing non-applicable values)
ESS9_CH_proj %>% 
  ggplot(aes(x = party))+
  geom_bar()

ESS9_CH_proj %>% 
  filter(party < 17) %>%
  ggplot(aes(x = party))+
  geom_bar()

ESS9_CH_proj %>% 
  ggplot(aes(x = trustPol))+
  geom_bar()

ESS9_CH_proj %>% 
  filter(trustPol <= 10) %>% 
  ggplot(aes(x = trustPol))+
  geom_bar()+
  labs(x = "Trust in political parties",
       y = "Number of respondents")

ESS9_CH_proj %>% 
  ggplot(aes(x = safety))+
  geom_bar()

# associations between variables
ESS9_CH_proj %>% 
  ggplot(aes(x = safety, y = trustPol, group = safety))+
  geom_boxplot()

# here, could decide to create a new 'filtered' dataframe, or to use the pipes every time: e.g.

ESS9_CH_filt <- ESS9_CH_proj %>% 
  filter(trustPol <= 10) %>% 
  filter(party < 17) %>% 
  filter(safety <= 4)

ESS9_CH_filt %>% 
  ggplot(aes(x = safety, y = trustPol, group = safety))+
  geom_boxplot()

# (or do the following for every plot)
# ESS9_CH_proj %>% 
#   filter(trustPol <= 10) %>% 
#   filter(party < 17) %>% 
#   filter(safety <= 4) %>% 
#   ggplot(aes(x = safety, y = trustPol, group = safety))+
#   geom_boxplot()

ESS9_CH_filt %>% 
  group_by(safety) %>% 
  summarize(avgTrustPol = mean(trustPol, na.rm =T),
            sampleSize = n())

cor(ESS9_CH_filt$trustPol, ESS9_CH_filt$safety)
cor.test(ESS9_CH_filt$trustPol, ESS9_CH_filt$safety)

# lots of other functions in tidyverse, for example:
# - to transform a wide dataset into long: pivot_longer() from tidyr
# - to order a dataset based on a variable: arrange() 
# - to select the first n rows: slice()
# - to extract a vector from the column (instead of using $column, can use `df %>% pull(column)` )

lm(trustPol~safety, data = ESS9_CH_filt) %>% summary()

lm1 <- lm(trustPol~safety, data = ESS9_CH_filt)
plot(lm1) # (need to press enter multiple times in the console to visualize different plots)

resid(lm1)




