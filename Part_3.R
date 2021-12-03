# Read new dataset -> result extrapolation
library(gridExtra) # for grid.arrange
library(psych) # for describe
library(tidyverse) # for dplyr and ggplot2
library(dplyr) 

data_sample_7 = read.csv("https://tinyurl.com/b385chpu") 
data_sample_8 = read.csv("https://tinyurl.com/4f8thztv") 

# First study both dataset
View(data_sample_7)
View(data_sample_8)
str(data_sample_7)
str(data_sample_8) #see which are characters and should be factors;
# which are: ID and sex.
# I create a new data_sample in case sth went wrong:
data_sample_7.1 <- data_sample_7 %>% 
  mutate(ID = factor(ID),
         sex = factor(sex),
         hospital = factor(hospital))
data_sample_8.1 <- data_sample_8 %>% 
  mutate(ID = factor(ID),
         sex = factor(sex),
         hospital = factor(hospital))
# Check if that worked:
class(data_sample_7.1$sex) #factor!
levels(data_sample_7.1$sex) #problem -> there is a "woman" level that should be female: change that

data_sample_7.2 <- data_sample_7.1 %>% 
  mutate(sex = recode(sex, "woman" = "female")) 
levels(data_sample_7.2$sex)

class(data_sample_8.1$sex) #factor! 
levels(data_sample_8.1$sex) # To remember: Female > Male (alphabetical order of reference)
# Data validation within test parameters
# 1. Overall pain (Pain)
data_sample_7.2 %>%
  select(pain) %>% 
  range() #perfect: between 0-10
data_sample_8.1 %>%
  select(pain) %>% 
  range() #perfect: between 0-10

# 2. State Trait Anxiety Inventory (STAI_trait)
data_sample_7.2 %>%
  select(STAI_trait) %>% 
  range() #perfect: between 20-80
data_sample_8.1 %>%
  select(STAI_trait) %>% 
  range() #perfect: between 20-80

# 3. Pain Catastrophizing Scale (Pain_cat)
data_sample_7.2 %>%
  select(pain_cat) %>% 
  range() #all values between 0 and 52! perfect
data_sample_8.1 %>%
  select(pain_cat) %>% 
  range() #all values between 0 and 52! perfect
# 4. Cortisol_serum and 5. Cortisol_saliva::
## we dn't have a range for that, but all the numbers seem to follow an constant scale from 2 to 8
# 6. Mindful Attention Awareness Scale (mindfullness)
data_sample_7.2 %>%
  select(mindfulness) %>% 
  range() #all values between 1 and 6! perfect
data_sample_8.1 %>%
  select(mindfulness) %>% 
  range() #all values between 1 and 6! perfect

## Descriptive analysis
data_sample_7.2 %>% # overview of the variables
  summary() #error: one household_income is negative (-) for (+)

# It's more probable for coding error, as there is no test for 
# household_income
data_sample_7.3 <- data_sample_7.2 %>% 
  mutate(household_income = replace(household_income, household_income < 0, 7884))
str(data_sample_7.3)
data_sample_7.3 %>% # overview of the variables
  summary()
data_sample_7.3 %>% # all vb between -1, 1
  select(-ID, -sex, -hospital) %>% 
  describe()
str(data_sample_7.3)

# Now the same for the 2nd data set
data_sample_8.1 %>% # overview of the variables
  summary()
data_sample_8.1 %>% # overview of the variables
  select(-ID, -sex, -hospital) %>% 
  describe()
str(data_sample_8.1)
View(data_sample_8.1)

## descriptive tables
# for sample 3 = 7.3
# Summarized tables
library(table1)
# Table 1: frequencies / demographics
table1::label(data_sample_7.3$sex) <- "Gender"
table1::label(data_sample_7.3$age) <- "Age"
table1::label(data_sample_7.3$weight) <- "Weight (kg)" 
table1::label(data_sample_7.3$IQ) <- "IQ test"
table1::label(data_sample_7.3$household_income) <- "Household Income (USD)"

table1::table1(~sex + age + weight + IQ + household_income, data = data_sample_7.3)

## Table 2: psych measurements
table1::label(data_sample_7.3$pain) <- "Overall pain"
table1::label(data_sample_7.3$pain_cat) <- "Pain Catastrophizing Scale "
table1::label(data_sample_7.3$STAI_trait) <- "State Trait Anxiety Inventory"
table1::label(data_sample_7.3$cortisol_serum) <- "Cortisol (serum)"
table1::label(data_sample_7.3$cortisol_saliva) <- "Cortisol (saliva)"
table1::label(data_sample_7.3$mindfulness) <-"Mindful Attention Awareness Scale"
table1::table1(~pain + pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_7.3)

# Also check differences among other variables according to overall pain (individual differences hypothesis)
table1::table1(~pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness | pain, data = data_sample_7.3)

# for sample 4 = 8.1
# Summarized tables
# Table 1: frequencies / demographics
table1::label(data_sample_8.1$sex) <- "Gender"
table1::label(data_sample_8.1$age) <- "Age"
table1::label(data_sample_8.1$weight) <- "Weight (kg)" 
table1::label(data_sample_8.1$IQ) <- "IQ test"
table1::label(data_sample_8.1$household_income) <- "Household Income (USD)"

table1::table1(~sex + age + weight + IQ + household_income, data = data_sample_8.1)

## Table 2: psych measurements
table1::label(data_sample_8.1$pain) <- "Overall pain"
table1::label(data_sample_8.1$pain_cat) <- "Pain Catastrophizing Scale "
table1::label(data_sample_8.1$STAI_trait) <- "State Trait Anxiety Inventory"
table1::label(data_sample_8.1$cortisol_serum) <- "Cortisol (serum)"
table1::label(data_sample_8.1$cortisol_saliva) <- "Cortisol (saliva)"
table1::label(data_sample_8.1$mindfulness) <-"Mindful Attention Awareness Scale"
table1::table1(~pain + pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_8.1)

# Also check differences among other variables according to overall pain (individual differences hypothesis)
table1::table1(~pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness | pain, data = data_sample_8.1)


# Build a linear mixed model on n data file 3 = data_sample_7.3
install.packages("cAIC4")
install.packages("r2glmm")
install.packages("lme4")
install.packages("Matrix")
install.packages("lmerTest")
install.packages("MuMIn")
library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM

# Function to extract standardized beta coefficients from linear mixed model:
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

# Exploring clustering in the data [data_sample_7.3]
# With all the 'predictive variables'
# plots  to see the directions
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = age) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = age, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F, fullrange = TRUE)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = STAI_trait) + geom_point(aes(color = hospital),
   size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = pain_cat) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>% #yes similar directions, random intercept mixed effects
  ggplot() + aes(y = pain, x = pain_cat, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE) + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = cortisol_saliva) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>%
  ggplot() + aes(y = pain, x = mindfulness) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
# Others:
data_sample_7.3 %>% # no relationship at all
  ggplot() + aes(y = pain, x = IQ) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>% # a bit of inverse relationship maybe
  ggplot() + aes(y = pain, x = weight) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)
data_sample_7.3 %>% # no relationship at all
  ggplot() + aes(y = pain, x = household_income) + geom_point(aes(color = hospital),
  size = 4) + geom_smooth(method = "lm", se = F)

## The model: random intercept + foxed slope
model5 = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + 
              mindfulness + (1 | hospital), data = data_sample_7.3)
model5
summary(model5)
stdCoef.merMod(model5)
confint(model5)

# R² for our Model 5
install.packages("r2glmm") # -> for r2beta(): marginal R²
library(r2glmm)
install.packages("MuMIn") # -> r.squaredGLMM(): estimate of both the marginal and the conditional R squared 
library(MuMIn)

?r2beta
r2beta(model5, method = "nsj", data = data_sample_7.3)
r.squaredGLMM(model5)

# See model predictions in the data sample 4 = 8.1
View(data_sample_8.1)
library(tidyverse) # for tidy format
library(lme4)
?predict.merMod 
# calculate predicted values
pred_test_2 <- predict(model5, data_sample_8.1, allow.new.levels = TRUE)
pred_test_2
# now we calculate the sum of squared residuals
RSS_test_2 = sum((data_sample_8.1[, "pain"] - pred_test_2)^2)
RSS_test_2

TSS = sum((data_sample_8.1$pain - predict(model5))^2)
TSS

R2 = 1 - (RSS_test_2/TSS)
R2

# New model: model 6, same as 5 but with random slope + only most 
# important predictor of model 5
## The model: random intercept + random slope
model6 = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                     data = data_sample_7.3)

model6
summary(model6)
?isSingular
stdCoef.merMod(model6)
confint(model6)

r2beta(model6, method = "nsj", data = data_sample_7.3)
r.squaredGLMM(model6)

# Visualization
# model 6
data_sample_7.3_slope  %>% 
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm", se = F, fullrange = TRUE) + 
  facet_wrap(~hospital, ncol = 2)

# fit indexes
sum(residuals(model5)^2)
sum(residuals(model6)^2)

#cAIC
install.packages("cAIC4")
library(cAIC4)
cAIC(model5)
cAIC(model6)

# final graph
final_plot <- data_sample_7.3_slope %>% 
  ggplot() + 
  aes(y=pain, x=cortisol_serum, color=hospital) + 
  geom_point(size = 3) + 
  geom_smooth(method = "lm", se = F, fullrange = TRUE)
final_plot

library(ggplot2)
