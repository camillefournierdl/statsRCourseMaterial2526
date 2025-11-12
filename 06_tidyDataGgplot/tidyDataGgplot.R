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

# >> Are the length of the Petal & Sepal related to each other, in the iris dataset?

# You can use the slides here, the tutorial linked here and in the beginning of the code
# https://r4ds.hadley.nz/data-visualize.html, or stack overflow



##
mtcars # is this a long-format dataset?

# >> build the plot from the slides



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

# calculate summary statistics
irisSummary <- iris %>% 
  group_by(Species) %>% 
  summarize(mean_sepalLength = mean(Sepal.Length, na.rm=T),
            sd_sepalLength = sd(Sepal.Length, na.rm=T))

# you can combine commands together
irisModified <- iris %>% 
  mutate(ratioPetalSepalLength = Petal.Length/Sepal.Length) %>% 
  filter(Species == "setosa") %>% 
  summarize(mean_ratioLength = mean(ratioPetalSepalLength, na.rm=T)) %>% 
  pull(mean_ratioLength) # pull extracts the values in a column and prints them
  

# merge datasets together
# example, do not run
dfMerged <- merge(df1, df2, by = "columnid", all.x = T, all.y = T) # this would be a full merge, what's the default behavior?

# now we merge colors to the irisSummary data
dfCol <- data.frame(species = c("setosa", "versicolor", "virginica", "other"),
                 color = c("purple", "yellow", "green", "grey"))

# here, the name of the Species column is not written with the same capitalization
# a solution is to use dplyr to rename the column in dfCol 

irisMerged <- merge(irisSummary, dfCol %>% 
                      rename(Species = species), # see how we rename first
                    by = "Species", all.x = T, all.y = T) # also full merge

irisMerged

irisMerged2 <- merge(irisSummary, dfCol %>% 
                      rename(Species = species),
                    by = "Species", all.x = T, all.y = F) # left merge (more common, default behavior)

irisMerged2

# make sure you understand the difference between irisMerged and irisMerged2


#> use it all together, think about how to make the plots described in the slides

band_members
band_instruments

# create bandsInstrumentsMerged

# >> together, use geom_bar to create the plot in the slides

# >> again, another plot, this time do it yourself

# using merge(),
# filter(),
# ggplot()+
#   geom_bar()



