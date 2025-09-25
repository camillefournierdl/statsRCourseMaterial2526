# Create a new project, you can name it dataAnalysisPolicy2025

# Talk about the User interface, what do we see?

# Focus on the console, the environment, and the files panels

# Try to type in commands in console

# 1+1 5/6, whatever you like, can try date()

# make sure the console is in the right working directory: getwd()

# if not, setwd("path") -> can also navigate using the files UI and use the More -> Set as working Directory tool

# create a folder using dir.create(), you can name it 01_introClass

# in there, create a new R script (New File, R Script). You can name it 01_script.R

# Now we see a new panel in RStudio. That's your script. 

# start typing some commands, like 1+1, date(), etc. If you press enter, it doesn't run. That's because you're writing a script
1+1

# We mainly write code in here, that's basically a text editor. Why?

# Who is familiar with Jupyter notebooks? RMarkdown might be a good file type for you to code with if you're more familiar with that.

# These R scripts are usually used in ways that are slightly different to your regular python scripts. 

# We can run them from any terminal, but we usually like to run them line by line.

# Write 3 commands, and run them one by one using Ctrl+Enter (or <command>+Enter)

# Now select the whole script using Ctrl+A, and press Ctrl+Enter to run the whole script

# you can also select two lines and run them, try it.

# You can find more information about what a function does using ?functionName (opens in Help panel)--- you can also press F1 with your text cursor inside the function

# You can also use that panel to search for a function.

### any questions? 2 min break to make sure everyone is on the same page

# Then we'll talk about the environment. For now it should say it's empty.

# In R, the convention to store information in objects is the following:

object <- "information"

nowsDate <- date()

nowsDate
object

# there are characters, you can also store numbers

summed <- 5+856
summed # equivalent to print(summed)

# introducing vectors

ages <- c(21, 25, 24)
names <- c("Leonie", "Pisa", "Henry")

# operations on vectors

ages/2

names/2 # showing what an error looks like

# storing these in a dataframe

classStudents <- data.frame(age = ages, name = names)

classStudents

# can call a single column using $, that's very similar to then having a vector

# we can also inspect in the environment using the arrow, or clicking on the object name. Can also use Ctrl+leftClick on the object name in the script

# let's try to load a dataset from our computer

read.csv("01_introClass/data/ESS10_CHData.csv")

# now save it into an object

datasetSurvey <- read.csv("01_introClass/data/ESS10_CHData.csv")
datasetSurvey

# looks complicated
head(datasetSurvey, 1)
colnames(datasetSurvey)

# can inspect in the UI

# here we load the iris dataset, a dataset that's installed with R
dataIris <- iris

# try to get more information about this dataset (help, use the help function! Or F1.. or google it)

# find the names of the columns and what they mean. The type of every column -> try to find more information about what a factor is 
head(dataIris)

summary(dataIris)

help(iris)

# from the help 
pairs(iris[1:4], col = iris$Species)
legend(0.5, 1, levels(iris$Species), fill = 1:3, bty = "n",
       horiz = TRUE, xjust = 0.5, yjust = 0, xpd = TRUE)


# create a new column in iris that would be the ratio of sepal length to petal length (A = B+C)
# remember the $ operator
dataIris$ratioSP <- dataIris$Sepal.Length/dataIris$Petal.Length

# maybe talk about errors, how gpt can help you understand why you get an error, but be careful!!!!!
# also show how to use stack overflow

# bonus: equivalent using tidyverse (dplyr)
library(tidyverse)

dataIris <- dataIris %>% 
  mutate(ratioSP = Sepal.Length/Petal.Length)


