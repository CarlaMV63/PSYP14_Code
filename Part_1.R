# Dataset to asses trait and state psychological measures on pain
library(gridExtra) # for grid.arrange
library(psych) # for describe
library(tidyverse) # for dplyr and ggplot2
library(dplyr) 

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv") 

# See the dataset
View(data_sample_1)
str(data_sample_1) #see which are characters and should be factors;
# which are: ID and sex.
# I create a new data_sample in case sth went wrong:
data_sample_2 <- data_sample_1 %>% 
  mutate(ID = factor(ID),
         sex = factor(sex))
# Check if that worked:
class(data_sample_2$ID) #factor!
levels(data_sample_2$ID)
class(data_sample_2$sex) #factor! 
levels(data_sample_2$sex) # To remember: Female > Male (alphabetical order of reference)

# Data validation within test parameters
# 1. Overall pain (Pain)
data_sample_2 %>%
  select(pain) %>% 
  range() #ups! there is a 55, and it should be between 0-10, let's replace it
data_sample_3 <- data_sample_2 %>% 
  mutate(pain = replace(pain, pain > 10, NA))
data_sample_3$pain #perfect! Let's check the rest of the scales ranges
# 2. State Trait Anxiety Inventory (STAI_trait)
data_sample_3 %>%
  select(STAI_trait) %>% 
  range() #ups! There is a 4.2, and the minimum num should be 20, replace: 
data_sample_4 <- data_sample_3 %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait < 20, NA))
View(data_sample_4) #perfect!
# 3. Pain Catastrophizing Scale (Pain_cat)
data_sample_4 %>%
  select(pain_cat) %>% 
  range() #all values between 0 and 52! perfect
# 4. Cortisol_serum and 5. Cortisol_saliva::
 ## we dn't have a range for that, but all the numbers seem to follow an constant scale from 2 to 8
# 6. Mindful Attention Awareness Scale (mindfullness)
data_sample_4 %>%
  select(mindfulness) %>% 
  range() #all values between 1 and 6! perfect

## Descriptive analysis
data_sample_4 %>% # overview of the variables
  summary()
# check mean and sd of each test and measurement too
data_sample_4 %>% # check skew and kurtosis -> see normality of each variable (-1, 1)
  select(-ID, -sex) %>% 
  describe() #all the variables seem to behave normally (-1, 1)

## Summarized table
install.packages("table1")
library(table1)
# Table 1: frequencies / demographics
table1::label(data_sample_4$sex) <- "Gender"
table1::label(data_sample_4$age) <- "Age"
table1::label(data_sample_4$weight) <- "Weight (kg)" 
table1::label(data_sample_4$IQ) <- "IQ test"
table1::label(data_sample_4$household_income) <- "Household Income (USD)"

table1::table1(~sex + age + weight + IQ + household_income, data = data_sample_4)

## Table 2: psych measurements
table1::label(data_sample_4$pain) <- "Overall pain"
table1::label(data_sample_4$pain_cat) <- "Pain Catastrophizing Scale "
table1::label(data_sample_4$STAI_trait) <- "State Trait Anxiety Inventory"
table1::label(data_sample_4$cortisol_serum) <- "Cortisol (serum)"
table1::label(data_sample_4$cortisol_saliva) <- "Cortisol (saliva)"
table1::label(data_sample_4$mindfulness) <-"Mindful Attention Awareness Scale"

table1::table1(~pain + pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_4)
# Also check differences among other variables accordind to overall pain (individual differences hypothesis)
table1::table1(~pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness | pain, data = data_sample_4)

## Visual overview (ggplot)
## Demographics: with pain
#Gender/sex
Gender_pain_graph <- data_sample_4 %>%
  ggplot() +
    aes(x = pain, fill = sex) +
    geom_histogram() #seems like a normal distribution of the pain across gender
#Age
Age_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = age, y = pain) +
    geom_jitter() +
    geom_smooth(method = "lm") #seems no relationship (flat)
#Weight
Weight_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = weight, y = pain) +
    geom_jitter() +
    geom_smooth(method = "lm") #it only shows that mean weight between 60-80
#IQ
IQ_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = IQ, y = pain) +
    geom_jitter() +
    geom_smooth(method = "lm") #seems no relationship (flat)
#Income
Income_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = household_income, y = pain) +
    geom_jitter() +
    geom_smooth(method = "lm") #seems no relationship (flat)

## Tests parameters: relationships between the ordinal v pain with the rest of the scales
# 1. State Trait Anxiety Inventory (STAI_trait)
STAI_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = STAI_trait, y = pain) +
  geom_count() + 
  geom_smooth(method = "lm") #possible tendency of + anxiety as + pain (positive relationship)
# 2. Pain Catastrophizing Scale (Pain_cat)
Pain_cat_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = pain_cat, y = pain) +
  geom_count() + 
  geom_smooth(method = "lm") #pain seems a coherent construct as it converge between both scales
# 3. Cortisol_serum
Cortisol_serum_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = cortisol_serum, y = pain) +
  geom_count() + 
  geom_smooth(method = "lm") #it seems that at more pain experienced, more cortisol in serum was found (positive relationship)
# 4. Cortisol_saliva
Cortisol_saliva_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = cortisol_saliva, y = pain) +
  geom_count() + 
  geom_smooth(method = "lm") #it seems that at more pain experienced, more cortisol in saliva was found (positive relationship)
# 5. Mindful Attention Awareness Scale (mindfullness)
Mindfulness_pain_graph <- data_sample_4 %>%
  ggplot() +
  aes(x = mindfulness, y = pain) +
  geom_count() + 
  geom_smooth(method = "lm") #interesting: it seems that there is a negative relationship between the variables;
                             #so at more mindfulness, less pain.

## MODELS building
# Model 1
model1 <- lm(pain ~ age + sex, data = data_sample_4_imputed1)
model1

# Model 2
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_4_imputed1)
model2

# Model 3
model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_4_imputed1)
model3

# Before making any interpretations, check:
# Model diagnostics - assumptions of linear regression
install.packages("sandwich")
install.packages("boot")
install.packages("lmboot")
library(psych) # for describe
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy code

# 1. Outliers or extreme cases
# Cook's distance measurements
## MODEL 1
model1 %>%
  plot(which = 5) 
model1 %>%
  plot(which = 4)

#Individual cases exploration: 8, 23, 47
data_sample_4[8, ]
data_sample_4[23, ]
data_sample_4[47, ]

## MODEL 2
model2 %>%
  plot(which = 5)
model2 %>%
  plot(which = 4) #all very far from 1

#Individual cases exploration: 47, 74, 86
data_sample_4[74, ]
data_sample_4[86, ]

## MODEL 3
model3 %>%
  plot(which = 5)
model3 %>%
  plot(which = 4) #all very far from 1

#Individual cases exploration: 47, 65, 86
data_sample_4[65, ]

#Final Cook's coefficient (4/N < 1 is problematic) = 
4/160 # = 0.025 <- not problematic

## Let's check normality and see if u need to make bootstraping before eliminating any outlier (47)


# 2. Normality of the residuals only (error terms) on linear regression
# Model 1
# 1) QQ Plot 
model1 %>%
  plot(which = 2)
# 2) Histogram of the residuals 
residuals_model1 = enframe(residuals(model1))
residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()
# 3) Skewness and Kurtosis 
describe(residuals(model1)) #between -1 and 1

### It follows the normal distribution!

# Model 2
model2 %>%
  plot(which = 2)
# 2) Histogram of the residuals 
residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()
# 3) Skewness and Kurtosis 
describe(residuals(model2)) #between -1 and 1

### It follows the normal distribution!

# Model 3
# 1) QQ Plot 
model3 %>%
  plot(which = 2)
# 2) Histogram of the residuals 
residuals_model3 = enframe(residuals(model3))
residuals_model3 %>%
  ggplot() + aes(x = value) + geom_histogram()
# 3) Skewness and Kurtosis 
describe(residuals(model3)) #between -1 and 1

### It follows the normal distribution!

# 3. Linearity
install.packages("car")
library(car)
# Model 1
model1 %>%
  residualPlots() #a bit curved but not p significant (0.1)
??residualPlots   
# Model 2
model2 %>%
  residualPlots() 
# Model 3
model3 %>%
  residualPlots()

# Is this big enough to do overfitting? ANd not having variability

# 4. Homoscedasticity
# Model 1
model1 %>%
  plot(which = 3)
model1 %>%
  ncvTest() # NCV test
model1 %>%
  bptest() # Breush-Pagan test    ## homoscedasticity assumed!
# Model 2
model2 %>%
  plot(which = 3)
model2 %>%
  ncvTest() # NCV test
model2 %>%
  bptest() # Breush-Pagan test    ## homoscedasticity assumed!
# Model 3
model3 %>%
  plot(which = 3)
model3 %>%
  ncvTest() # NCV test
model3 %>%
  bptest() # Breush-Pagan test    ## homoscedasticity assumed!

# 5. No multicollinearity (VIF > 3 is a problem)
# Model 1
model1 %>%
  vif()
# Model 2
model2 %>%
  vif() #problem in cortisol factors, both > 3
# Analyse the 2 variables
cor.test(data_sample_4_imputed1$cortisol_serum, data_sample_4_imputed1$cortisol_saliva)
data_sample_4_imputed1 %>% 
  select(cortisol_serum) %>% 
  range()
data_sample_4_imputed1 %>% 
  select(cortisol_saliva) %>% 
  range()
# Model 3
model3 %>%
  vif() 

install.packages("apaTables")
library(apaTables)
library(lm.beta)
## MODEL comparison
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
# Model 1
model1
#coefficients
coef_table(model1)
summary(model1)
confint(model1)
lm.beta(model1)
#Calculate residual errors
RSS_model1 <- deviance(model1)
RSS_model1 
AIC(model1)

# Model 2
model2
#coefficients
coef_table(model2)
summary(model2)
confint(model2)
lm.beta(model2)
#Calculate residual errors
RSS_model2 <- deviance(model2)
RSS_model2 
AIC(model2)

# Model 3
model3
#coefficients
coef_table(model3)
summary(model3) 
confint(model3)
lm.beta(model3)
#Calculate residual errors
RSS_model3 <- deviance(model3)
RSS_model3 
AIC(model3)

#REGRESSION TABLE
library(apaTables)
#1
basic.reg <- lm(pain ~ age + sex, data = data_sample_4)
apa.reg.table(basic.reg, filename = "Reg.table1", table.number = 3)
#2
basic.reg <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_4)
apa.reg.table(basic.reg, filename = "Reg.table2", table.number = 4)
#3
basic.reg 
apa.reg.table(basic.reg, filename = "Reg.table3", table.number = 5)

#Final comparisons: model 1 - model 3
library(tidyverse) # for tidy format
# Rˆ2
summary(model1)$adj.r.squared ## [1] 0.07231481
summary(model3)$adj.r.squared ## [1] 0.504055
# Anova
anova(model1, model3)
# Problem: datasets do not fit (due the NA values)

#Solution: imputation with mice
install.packages("mice")
install.packages("VIM")
install.packages("lattice")
library(mice)
library(VIM)
library(lattice)

# https://bookdown.org/egarpor/PM-UC3M/app-nas.html
#understand the missing value pattern
md.pattern(data_sample_4)
#plot the missing values
data_sample_4_miss = aggr(data_sample_4, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(data_sample_4), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#create a new dataset_imputed
data_sample_4_imputed <- data_sample_4
#What methods were used for imputing
mice_imputes$method
# Input data using the sample mean
data_sample_4_imputed1 <- complete(mice(data = data_sample_4_imputed, m = 1, method = "mean"))
View(data_sample_4_imputed1)

# data implementation in the models -> 
# Model 1
model1 <- lm(pain ~ age + sex, data = data_sample_4_imputed1)
# Model 3
model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_4_imputed1)

# Review coef tables with less error

# Anova
anova(model1, model2, model3)
anova(model1, model3)

