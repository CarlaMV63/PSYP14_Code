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

#Solution to missing values repteated (NA): imputation with mice
library(mice)
library(VIM)
library(lattice)

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

# MODELS TO COMPARE
# Her model
Her_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + IQ + weight + household_income, data = data_sample_4_imputed1)
Her_model
# Our final Model: Model 3
model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_4_imputed1)

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
#Her model
Her_model %>%
  plot(which = 5) 
Her_model %>%
  plot(which = 4)

#Individual cases exploration: 47, 85, 86
data_sample_4[47, ]
data_sample_4[85, ]
data_sample_4[86, ]

#Final Cook's coefficient (4/N < 1 is problematic) = 
4/160 # = 0.025 <- not problematic

# 2. Normality of the residuals only (error terms) on linear regression
# Her model
# 1) QQ Plot 
Her_model %>%
  plot(which = 2)
# 2) Histogram of the residuals 
residuals_Her_model = enframe(residuals(Her_model))
residuals_Her_model %>%
  ggplot() + aes(x = value) + geom_histogram()
# 3) Skewness and Kurtosis 
describe(residuals(Her_model)) #between -1 and 1

## It follows the normal distribution!

# 3. Linearity
install.packages("car")
library(car)
# Her_model
Her_model %>%
  residualPlots() #not p-significant, thus it is linear

# 4. Homoscedasticity
# Her_model
Her_model %>%
  plot(which = 3)
Her_model %>%
  ncvTest() # NCV test
Her_model %>%
  bptest() # Breush-Pagan test    ## homoscedasticity assumed!

# 5. No multicollinearity (VIF > 3 is a problem)
# Her_model
Her_model %>%
  vif()

# Result-based models
# Backward regression
?step
Her_model_back <- step(Her_model, direction = "backward")
Model3_back <- step(model3, direction = "backward")

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

# Model comparison: Her_model vs. Her_model_back vs. Model3_back
Her_model
summary(Her_model)
coef_table(Her_model)
AIC(Her_model)

Her_model_back
summary(Her_model_back)
coef_table(Her_model_back)
AIC(Her_model_back)

Model3_back
summary(Model3_back)
coef_table(Model3_back)
AIC(Model3_back)

anova(model3, Her_model) #same
anova(Her_model, Her_model_back) #no improvement
anova(model3, Model3_back) #no improvement
anova(Her_model_back, Model3_back)
anova(Her_model_back, model3)

# Model test in new data
# New data:
library(tidyverse)
data_sample_5 <- read.csv("https://tinyurl.com/87v6emky")
# Explore the new data:
View(data_sample_5)
str(data_sample_5) #see which are characters and should be factors;
# which are: ID and sex.
# I create a new data_sample in case sth went wrong:
data_sample_6 <- data_sample_5 %>% 
  mutate(ID = factor(ID),
         sex = factor(sex))
# Check if that worked:
class(data_sample_6$ID) #factor!
levels(data_sample_6$ID)
class(data_sample_6$sex) #factor! 
levels(data_sample_6$sex) # To remember: Female > Male (alphabetical order of reference)

# Data validation within test parameters
# 1. Overall pain (Pain)
data_sample_6 %>%
  select(pain) %>% 
  range() #perfect: all between 1-10
# 2. State Trait Anxiety Inventory (STAI_trait)
data_sample_6 %>%
  select(STAI_trait) %>% 
  range() #perfect: all between 20-80 
# 3. Pain Catastrophizing Scale (Pain_cat)
data_sample_6 %>%
  select(pain_cat) %>% 
  range() #perfect: all values between 0 and 52
# 4. Cortisol_serum and 5. Cortisol_saliva::
## we don't have a range for that, but all the numbers seem to follow an constant scale from 2 to 8
# 6. Mindful Attention Awareness Scale (mindfullness)
data_sample_6 %>%
  select(mindfulness) %>% 
  range() #all values between 1 and 6! perfect

## Descriptive analysis
data_sample_6 %>% # overview of the variables
  summary()
# check mean and sd of each test and measurement too
data_sample_6 %>% # check skew and kurtosis -> see normality of each variable (-1, 1)
  select(-ID, -sex) %>% 
  describe() #all the variables seem to behave normally (-1, 1)

## Summarized tables
install.packages("table1")
library(table1)
# Table 1: frequencies / demographics
table1::label(data_sample_6$sex) <- "Gender"
table1::label(data_sample_6$age) <- "Age"
table1::label(data_sample_6$weight) <- "Weight (kg)" 
table1::label(data_sample_6$IQ) <- "IQ test"
table1::label(data_sample_6$household_income) <- "Household Income (USD)"

table1::table1(~sex + age + weight + IQ + household_income, data = data_sample_6)

## Table 2: psych measurements
table1::label(data_sample_6$pain) <- "Overall pain"
table1::label(data_sample_6$pain_cat) <- "Pain Catastrophizing Scale "
table1::label(data_sample_6$STAI_trait) <- "State Trait Anxiety Inventory"
table1::label(data_sample_6$cortisol_serum) <- "Cortisol (serum)"
table1::label(data_sample_6$cortisol_saliva) <- "Cortisol (saliva)"
table1::label(data_sample_6$mindfulness) <-"Mindful Attention Awareness Scale"
table1::table1(~pain + pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_6)

# Also check differences among other variables according to overall pain (individual differences hypothesis)
table1::table1(~pain_cat + STAI_trait + cortisol_serum + cortisol_saliva + mindfulness | pain, data = data_sample_6)


# TEST PERFORMANCE
# calculate predicted values
# Her_model_back ND model3 (our final model: theory-based)
pred_test <- predict(model3, data_sample_6)
pred_test_back <- predict(Her_model_back, data_sample_6)
# now we calculate the sum of squared residuals
RSS_test = sum((data_sample_6[, "pain"] - pred_test)^2)
RSS_test_back = sum((data_sample_6[, "pain"] - pred_test_back)^2)

RSS_test #243.8798
RSS_test_back #249.7466
