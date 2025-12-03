# In this last session, we look at group comparisons using anova and tukey tests, dummy variables, predicted values, and using stargazer to compare model specifications.
# You can look for the specific parts that are relevant for your project using the different sections of the code

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

##### ------- export regression tables with stargazer------- ######

library(stargazer)
##Multple Regression - base model
base <- lm(confidencePolitics ~ newsPol, data = ESS9_CH_filt) 
summary(base)

####Multple Regression with Age transformation
ESS9_CH_filt$age2<-ESS9_CH_filt$age*ESS9_CH_filt$age

age_2 <- lm(confidencePolitics ~ newsPol + age + age2, data = ESS9_CH_filt) 
summary(age_2)

####Multple Regression with controls
#same as above
full <- lm(confidencePolitics ~ newsPol + age + age2 + income + gender, data = ESS9_CH_filt) 
summary(full)

###Full model with Education
#same as above but with age
full_educ <- lm(confidencePolitics ~ newsPol + age + age2 + income + gender + education, data = ESS9_CH_filt) 
summary(full_educ)

##Export to HTML
stargazer(base, age_2, full, full_educ,
          title="Stats I Multiple Regression Example",
          dep.var.labels="Confidence in politics",
          omit.stat=c("LL","ser","f"),
          align=TRUE,
          digits=2,
          covariate.labels=c("Political News", "Age", "Age$^{2}$","Income", "Gender",  "Education"),
          keep.stat=c("aic", "bic","rsq", "n","adj.rsq"),
          type="text", 
          out="08_predictedValuesDummies/multi.txt")

#Add in AIC and BIC
base$AIC<-AIC(base)
age_2$AIC<-AIC(age_2)
full$AIC<-AIC(full)
full_educ$AIC<-AIC(full_educ)

base$BIC<-BIC(base)
age_2$BIC<-BIC(age_2)
full$BIC<-BIC(full)
full_educ$BIC<-BIC(full_educ)

stargazer(base, age_2, full, full_educ,
          title="Stats I Multiple Regression Example",
          dep.var.labels="Confidence in politics",
          omit.stat=c("LL","ser","f"),
          align=TRUE,
          digits=2,
          covariate.labels=c("Political News", "Age", "Age$^{2}$","Income", "Gender",  "Education"),
          keep.stat=c("aic", "bic","rsq", "n","adj.rsq"),  #add in AIC BIC
          type="text",
          out="08_predictedValuesDummies/multi_modelfit.txt")


##Display CIs instead of SEs
stargazer(base, age_2, full, full_educ,
          title="Stats I Multiple Regression Example with CIs",
          dep.var.labels="Confidence in politics",
          omit.stat=c("LL","ser","f"),
          align=TRUE,
          digits=2,
          covariate.labels=c("Political News", "Age", "Age$^{2}$","Income", "Gender",  "Education"),
          keep.stat=c("aic", "bic","rsq", "n","adj.rsq"), 
          ci=TRUE, ci.level=0.95, #add in CIs
          type="text",
          out="08_predictedValuesDummies/multi_CIs.txt")

##Display CIs instead of SEs
stargazer(base, age_2, full, full_educ,
          title="Stats I Multiple Regression Example with CIs",
          dep.var.labels="Confidence in politics",
          omit.stat=c("LL","ser","f"),
          align=TRUE,
          digits=2,
          covariate.labels=c("Political News", "Age", "Age$^{2}$","Income", "Gender",  "Education"),
          keep.stat=c("aic", "bic","rsq", "n","adj.rsq"), 
          ci=TRUE, ci.level=0.95, #add in CIs
          type="html", #change to html
          out="08_predictedValuesDummies/multi_CIs.html")


##### ------- predicted values ------- ######

# for one categorical variable
emm_example2 <- emmeans(example2, ~ edu_f)

emm_df <- as.data.frame(emm_example2)

ggplot(emm_df, aes(x = edu_f, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
  labs(y = "Adjusted mean confidence",
       x = "Education level")

# for one variable
emm_example2 <- emmeans(example2, ~ newsPol)

emm_df <- as.data.frame(emm_example2)

ggplot(emm_df, aes(x = newsPol, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = .1) +
  labs(y = "Adjusted mean confidence",
       x = "Education level")


# for more variables, you can draw scenarios, here I show an example where we vary gender and news consumption

# we define a baseline and then edit the scenarios. You should choose a *meaningful* baseline level for each variable (e.g., 0, median, or mean)
baseline <- data.frame(
  age    = mean(ESS9_CH_filt$age, na.rm = TRUE),
  income = mean(ESS9_CH_filt$income, na.rm = TRUE),
  edu_f  = "2 upper secondary",                 # pick an appropriate reference level, here, no interactions so it will be the same effect
  gender = 1, # could also test for 2, or look at the interaction between the 2!
  newsPol = mean(ESS9_CH_filt$newsPol, na.rm = TRUE) # will fill in different scenarios
)

# here we use quantiles to draw the scenarios, it would normally make sense to look at the distribution of the variable of interest and decide based on this
news_low  <- quantile(ESS9_CH_filt$newsPol, 0.05, na.rm = TRUE)
news_high <- quantile(ESS9_CH_filt$newsPol, 0.95, na.rm = TRUE)

scenarios <- rbind(
  transform(baseline, gender = 1, newsPol = news_low,  scenario = "Low news use - Male"),
  transform(baseline, gender = 1, newsPol = news_high, scenario = "High news use - Male"),
  transform(baseline, gender = 2, newsPol = news_low, scenario = "Low news use - Female"),
  transform(baseline, gender = 2, newsPol = news_high, scenario = "High news use - Female")
)

example3 <- lm(confidencePolitics ~ age + income + edu_f + gender * newsPol, data = ESS9_CH_filt) 
summary(example3)

pred <- cbind(
  scenarios,
  predict(example3, newdata = scenarios, interval = "confidence")
)

ggplot(pred, aes(x = scenario, y = fit)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .15) +
  labs(x = NULL, y = "Predicted confidence in participating in politics")

##### ------- using stargazer to compare models ------- ######


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



