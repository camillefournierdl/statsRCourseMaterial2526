library(tidyverse)
library(sjPlot) # used to plot interactions
library(sjmisc) # used to plot interactions

theme_set(theme_minimal()) # set default theme for the session

# load ESS data: (similar examples as for the dplyr tutorial)
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv") # replace with relevant dataset for your project

# select columns 
ESS9_CH_proj <- ESS9_CH %>% 
  select(agea, trstprt, aesfdrk, gndr, marsts) # we use similar examples to the tutorial for dplyr, adding gender and marital status

ESS9_CH_proj %>% plot()

plot(ESS9_CH_proj)

# rename columns
ESS9_CH_proj <- ESS9_CH_proj %>% 
  rename(age = agea,
         trustPol = trstprt,
         safety = aesfdrk,
         gender = gndr) # 

ESS9_CH_filt <- ESS9_CH_proj %>% 
  filter(trustPol <= 10) %>% 
  filter(age < 100) %>% 
  filter(safety <= 4) %>% 
  filter(gender < 9) # check how each variable is operationalized

plot(ESS9_CH_filt)

# here, I would do more descriptives between your variables of interest, but here are examples of linear models in R:

lm(safety~trustPol, data = ESS9_CH_filt) # doesn't give you much information

lm(safety~trustPol, data = ESS9_CH_filt) %>% summary() # is what you're expected to know how to interpret
# check in the codebook how the safety variable is operationalized (less is actually more safe)

# we find that safety should probably reversed to help with interpretation
ESS9_CH_filt <- ESS9_CH_filt %>% 
  mutate(safety = -safety)

lm1 <- lm(safety~trustPol, data = ESS9_CH_filt) # another way of getting the summary (this time we store a variable lm1)
summary(lm1)

# plot(lm1) # need to press enter multiple times to show different plots

lmAge <- lm(safety~age, data = ESS9_CH_filt)
summary(lmAge)

# plot(lmAge) # need to press enter multiple times to show different plots

lmGender<- lm(safety~gender, data = ESS9_CH_filt)
summary(lmGender) # how to interpret the output for gender? -> check codebook for operationalization

lmComplex<- lm(safety~age+gender+trustPol, data = ESS9_CH_filt)
summary(lmComplex) #

# now we do the same with an interaction between age and gender:

lmInteraction <- lm(safety~age*gender+trustPol, data = ESS9_CH_filt) # star is the equivalent of both + and :
lmInteraction <- lm(safety~age+gender+age:gender+trustPol, data = ESS9_CH_filt)

summary(lmInteraction)

plot_model(lmInteraction, type = "pred", terms = c("gender", "age"))

# and between age and trustPol, to test

lmInteraction2 <- lm(safety~age+gender+age:gender+trustPol+age:trustPol, data = ESS9_CH_filt)
summary(lmInteraction2)

plot_model(lmInteraction2, type = "pred", terms = c("trustPol", "age"))

# more on predicted probabilities next week
# + could experiment with the marsts variable to play with dummies (currently it would be considered a continuous variable in a lm)
# would need to re-code a column as a factor with different levels, e.g. using ifelse() or mutate()
