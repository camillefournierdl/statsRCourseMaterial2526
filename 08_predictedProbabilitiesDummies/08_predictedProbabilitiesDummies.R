##### ------- setup ------- ######

library(tidyverse)
library(emmeans) # tukey test

theme_set(theme_minimal()) # set default theme for the session

# load ESS data: (similar examples as for the dplyr tutorial)
ESS9_CH <- read.csv("ESSData/ESS9_CH.csv") # replace with relevant dataset for your project

##### ------- data cleaning ------- ######

ESS9_CH_proj <- ESS9_CH %>% 
  select(agea, cptppola, hinctnta, eisced, gndr, nwspol, ipudrst) # new example, we look at what impacts confidence in participating in politics

ESS9_CH_proj %>% plot()

# plot(ESS9_CH_proj)

ESS9_CH_filt <- ESS9_CH_proj %>% 
  filter(cptppola <= 5) %>% 
  filter(agea < 100) %>% 
  filter(hinctnta <= 10) %>% 
  filter(eisced <= 7) %>% 
  filter(gndr < 9) %>% 
  filter(nwspol < 7777) %>% 
  filter(ipudrst <= 6) %>% # check how each variable is operationalized
  mutate(ipudrst = 7-ipudrst)

plot(ESS9_CH_filt)

table(ESS9_CH_filt$eisced) # no 0s so we can use this

# rename columns

ESS9_CH_filt <- ESS9_CH_filt %>% 
  rename(age = agea,
         confidencePolitics = cptppola,
         income = hinctnta,
         education = eisced,
         gender = gndr, # 1 = male, 2 = female, we could recode it with mutate as a categorical variable
         newsPol = nwspol,
         impUnd = ipudrst) 

##### ------- descriptives ------- ######
# some descriptives e.g. 
ESS9_CH_filt %>% 
  ggplot(aes(x = age, y = confidencePolitics, group = confidencePolitics))+
  geom_boxplot()

ESS9_CH_filt %>% 
  ggplot(aes(x = newsPol, y = confidencePolitics, group = confidencePolitics))+
  geom_boxplot()

ESS9_CH_filt %>% 
  ggplot(aes(x = gender, y = confidencePolitics, group = gender))+
  geom_boxplot()

# ESS9_CH_filt %>% 
#   ggplot(aes(x = impUnd, y = confidencePolitics, group = impUnd))+
#   geom_boxplot()
# 
# table(ESS9_CH_filt$impUnd)

ESS9_CH_filt %>% 
  ggplot(aes(x = education, y = confidencePolitics, group = education))+
  geom_boxplot()

# too many categories! I want to simplify this

##### ------- recoding into dummy ------- ######
# I often use ifelse statements:
ESS9_CH_filt$edu_f <- ifelse(ESS9_CH_filt$education %in% c(1,2), "secondary", "other") # simple example

ESS9_CH_filt$edu_f <- ifelse(ESS9_CH_filt$education %in% c(1,2), "1 lower secondary", 
                             ifelse(ESS9_CH_filt$education %in% c(3,4), "2 upper secondary",
                                    ifelse(ESS9_CH_filt$education %in% c(5,6,7), "3 higher educ", NA))) # to recode the whole variable

# see how edu_f is not a numeric anymore
ESS9_CH_filt$edu_f
str(ESS9_CH_filt)
class(ESS9_CH_filt$edu_f)

ESS9_CH_filt %>% 
  ggplot(aes(x = edu_f, y = confidencePolitics, group = edu_f))+
  geom_boxplot()

##### ------- example categorical variable and anova ------- ######
example1 <- lm(confidencePolitics ~ age + income + education + gender + newsPol, data = ESS9_CH_filt) 
summary(example1)

example2 <- lm(confidencePolitics ~ age + income + edu_f + gender + newsPol, data = ESS9_CH_filt) 
summary(example2) # what's the difference? Now we see how each level of education impacts the confidence,
# it makes the interpretation easier than if you had to think about what a 'one unit' increase on the scale of this variable does

anova(example2) # this tells us that education has an effect but we can't tell in what direction

# to get a precise understanding, we can use a pairwise tukey test:
emm_example2 <- emmeans(example2, ~ edu_f)          # estimated marginal means
pairs(emm_example2, adjust = "tukey")                # Tukey pairwise tests



# example of interaction plot (reminder from last week)
# library(sjPlot) # used to plot interactions
# library(sjmisc) # used to plot interactions

# example3 <- lm(confidencePolitics ~ age + income + edu_f + gender + newsPol + income:gender, data = ESS9_CH_filt) 
# summary(example3) 
# 
# plot_model(example3, type = "pred", terms = c("income", "gender"))

##### ------- second example where the categorical variable is not continuous (more fit for anova / tukey test) ------- ######
# now we just saw how to transform a continuous variable into a dummy, but what if you have a category variable that is not 'ordered'?
# we select different ESS instances (different times / different countries):

##### ------- setup ------- ######

ESS9_CH <- read.csv("ESSData/ESS9_CH.csv")

ESS9_CH <- ESS9_CH %>% 
  select(agea, cptppola, hinctnta, eisced, gndr, nwspol, ipudrst) %>% 
  mutate(survey = "ch9")

ESS10_CH <- read.csv("ESSData/ESS10_CHData.csv") 

ESS10_CH <- ESS10_CH %>% 
  select(agea, cptppola, hinctnta, eisced, gndr, nwspol, ipudrst) %>% 
  mutate(survey = "ch10")

ESS9_DE <- read.csv("ESSData/ESS9_DE.csv")

ESS9_DE <- ESS9_DE %>% 
  select(agea, cptppola, hinctnta, eisced, gndr, nwspol, ipudrst) %>% 
  mutate(survey = "de9")

ESS8_DE <- read.csv("ESSData/ESS8_DE.csv")

ESS8_DE <- ESS8_DE %>% 
  select(agea, cptppola, hinctnta, eisced, gndr, nwspol, ipudrst) %>% 
  mutate(survey = "de8")

fullDataset <- rbind(ESS9_CH, ESS10_CH, ESS9_DE, ESS8_DE) # this binds datasets by row

# then we filter
fullDataset_fil <- fullDataset %>% 
  filter(cptppola <= 5) %>% 
  filter(agea < 100) %>% 
  filter(hinctnta <= 10) %>% 
  filter(eisced <= 7) %>% 
  filter(gndr < 9) %>% 
  filter(nwspol < 7777) %>% 
  filter(ipudrst <= 6) %>% # check how each variable is operationalized
  mutate(ipudrst = 7-ipudrst)

fullDataset_fil <- fullDataset_fil %>% 
  rename(age = agea,
         confidencePolitics = cptppola,
         income = hinctnta,
         education = eisced,
         gender = gndr, # 1 = male, 2 = female, we could recode it with mutate as a categorical variable
         newsPol = nwspol,
         impUnd = ipudrst) 

# usually you would run some descriptives
fullDataset_fil %>% 
  ggplot(aes(x = survey, y = confidencePolitics, group = survey))+
  geom_boxplot()

fullDataset_fil %>% 
  ggplot(aes(x = confidencePolitics, fill = survey, y = after_stat(prop)))+
  geom_bar(position = "dodge")

fullDataset_fil %>% 
  ggplot(aes(x = age, y = confidencePolitics, group = confidencePolitics))+
  geom_boxplot()+
  facet_wrap(~survey)

##### ------- example (for non ordered) categorical variable and anova ------- ######

example5 <- lm(confidencePolitics ~ age + income + education + gender + newsPol + survey, data = fullDataset_fil) 
anova(example5) # this tells us the category survey has an effect significant effect on the confidence to participate in politics, but we don't know how

summary(example5) # this compares every category to the reference category and assesses significance.

# to get a precise understanding, we can use a pairwise tukey test:
emm_survey <- emmeans(example5, ~ survey)          # estimated marginal means
pairs(emm_survey, adjust = "tukey")                # Tukey pairwise tests



