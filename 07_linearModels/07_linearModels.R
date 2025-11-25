library(tidyverse)

theme_set(theme_minimal()) # set default theme for the session

# load ESS data: (similar examples as for the dplyr tutorial)
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv") # replace with relevant dataset for your project

# select columns 
ESS9_CH_proj <- ESS9_CH %>% 
  select(agea, trstprt, aesfdrk, gndr) #  trstprt - Trust in political parties --  aesfdrk - Feeling of safety of walking alone in local area after dark 

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

## predicted probabilities:

# library(emmeans) to delete, not really what I want
# 
# emm <- emmeans(lmComplex, ~ age, at = list(age = seq(20, 80, by = 10)))
# plot(emm)
# 
# emm <- emmeans(
#   lmComplex,
#   ~ age,
#   at = list(
#     age = seq(20, 80, by = 10),
#     gender = 1,
#     trustPol = mean(ESS9_CH_filt$trustPol)
#   )
# )
# 
# plot(emm)
# 
# emFemale <- emmeans(
#   lmComplex,
#   ~ age,
#   at = list(
#     age = seq(20, 80, by = 10),
#     gender = 2,
#     trustPol = mean(ESS9_CH_filt$trustPol)
#   )
# )
# 
# plot(emFemale)

# now we do the same with an interaction between age and gender:

lmInteraction <- lm(safety~age*gender+trustPol, data = ESS9_CH_filt) # star is the equivalent of both + and :
lmInteraction <- lm(safety~age+gender+age:gender+trustPol, data = ESS9_CH_filt)

summary(lmInteraction)

# predicted probabilities

