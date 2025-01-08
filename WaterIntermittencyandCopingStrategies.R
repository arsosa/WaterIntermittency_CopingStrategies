################
#Code to produce Table 1: household characteristics
#We ran logistic regression models between household demographics and coping strategies

####Libraries
library(ggplot2) #For plots
library(ggpubr) #For plots
library(RColorBrewer) #For plots
library(reshape2) #For plots
library(sjPlot) #For plots

library(plyr) #For data management
library(dplyr) #For data management
library(tidyr) #For data management
library(forcats) #reorder variables

library(car)
library(MASS) #for ordinal regression
library(VGAM) #for truncated regression

### Functions
twounit <- function  (model) {
  coefficients <- coef(model)
  exposure_variable_index <- which(names(coefficients) == "hours_noservice")
  coefficient_for_x <- coefficients[exposure_variable_index]
  pvalue<- coef(summary(model))[,'Pr(>|z|)'][2]
  adjusted_coefficient_for_2_unit_increase <- exp(coefficient_for_x * 3)
  adjusted_coefficient_ci<- confint(model3, 'hours_noservice', level=0.95)
  adjusted_coefficient_ci_exp <- exp(adjusted_coefficient_ci * 3)
  print (cbind(adjusted_coefficient_for_2_unit_increase, adjusted_coefficient_ci_exp, pvalue))
}

threeunit <- function  (model) {
  coefficients <- coef(model)
  exposure_variable_index <- which(names(coefficients) == "intermittency")
  coefficient_for_x <- coefficients[exposure_variable_index]
  pvalue<- coef(summary(model))[,'Pr(>|z|)'][2]
  adjusted_coefficient_for_2_unit_increase <- exp(coefficient_for_x * 3)
  adjusted_coefficient_ci<- confint(model4, 'intermittency', level=0.95)
  adjusted_coefficient_ci_exp <- exp(adjusted_coefficient_ci * 3)
  print (cbind(adjusted_coefficient_for_2_unit_increase, adjusted_coefficient_ci_exp, pvalue))
}

################
###Load dataset
setwd("/Users/andreasosamoreno/Downloads/Dissertation/Aim2/Paper/SubmissionPlosWater/SubmissionPlosWater2/Github")
data<-read.csv("CopingStrategyDataset.csv", stringsAsFactors = FALSE, sep=',') 
nrow(data) #1172

################
###Descriptive

borbon <- data[which(data$community_factor=="Borbon"),]
nrow(borbon) #815

maldonado <- data[which(data$community_factor=="Maldonado"),]
nrow(maldonado) #183

timbire <- data[which(data$community_factor=="Timbire"),]
nrow(timbire) #174

summary(data$days_noservice)
tapply(data$days_noservice, data$community_factor, summary)

summary(data$hours_noservice)
tapply(data$hours_noservice, data$community_factor, summary)

addmargins(table(data$drinking, data$community_factor))
prop.table(table(data$drinking))*100
prop.table(table(data$drinking, data$community_factor), margin=2)*100

addmargins(table(data$multiple, data$community_factor))
prop.table(table(data$multiple))*100
prop.table(table(data$multiple, data$community_factor), margin=2)*100

addmargins(table(data$share, data$community_factor))
prop.table(table(data$share))*100
prop.table(table(data$share, data$community_factor), margin=2)*100

addmargins(table(data$treat_domestic2, data$community_factor))
prop.table(table(data$treat_domestic2))*100
prop.table(table(data$treat_domestic2, data$community_factor), margin=2)*100

addmargins(table(data$cistern_elevated2, data$community_factor))
prop.table(table(data$cistern_elevated2))*100
prop.table(table(data$cistern_elevated2, data$community_factor), margin=2)*100

addmargins(table(data$storage, data$community_factor))
prop.table(table(data$storage))*100
prop.table(table(data$storage, data$community_factor), margin=2)*100


################
### Risk factor for coping strategies

#Bivariate risk factor analysis for coping strategies (repeat for #drinking, multiple, share, treat_domestic2, cistern_elevated2, storage
variables <- c("age","sex_factor", "race_factor", "marriage_factor", "housesize" , "property_factor",  "energy_factor","income","waterperson_factor") # Replace with your actual variable names
models <- list()
data$storage<-as.factor(data$storage)

# Loop through each variable and fit the model
for (var in variables) {
  formula <- as.formula(paste("storage~", var))
  model <- glm(formula, data = data, family = binomial)
 models[[var]] <- model
}

tab_model(models)

#Table S4. Multivariable risk factor analysis for coping strategies (repeat for #drinking, multiple, share, treat_domestic2, cistern_elevated2, storage)
model0 <- glm(storage~ race_factor + marriage_factor + housesize + income + waterperson_factor, data=data, family = binomial(link='logit'))
tab_model(model0)

################
### Table 3. Logistic regression model assessing the association between coping strategies and community (n=1172).

#Water storage
model1 <- glm(storage~community_factor, data=data, family = binomial(link='logit'))
tab_model(model1)

################
### Table 4. Multivariate logistic regression model assessing the association between coping strategies and water intermittency.  
#Models are adjusted for the community of residence and relevant sociodemographic variables. 

#Water storage
model2<- glm(storage~days_noservice + community_factor + housesize + income + waterperson_factor, data=data, family=binomial(link='logit')) #all year
tab_model(model2)

model3<- glm(storage~hours_noservice+ community_factor + housesize + income + waterperson_factor, data=data, family=binomial(link='logit')) #all year
tab_model(model3)
twounit(model3)

model4<- glm(storage~intermittency + community_factor + housesize + income + waterperson_factor, data=data, family=binomial(link='logit')) #all year
tab_model(model4)
threeunit(model4)
