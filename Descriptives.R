
################
#Code to produce Table 1: household characteristics
#We ran logistic regression models between household demographics and coping strategies

#Code to produce Table 2: assess the association between neighborhood and water intermittency
#We ran negative binomial models between neighborhoods of residence and intermittency

#Libraries
library(ggplot2) #For plots
library(ggpubr) #For plots
library(RColorBrewer) #For plots
library(reshape2) #For plots
library(sjPlot) #For plots

library(plyr) #For data management
library(dplyr) #For data management
library(tidyr) #For data management

library(car)
library(MASS) #for ordinal regression
library(VGAM) #for truncated regression


################
###Load data
setwd("/Users/andreasosamoreno/Downloads/Dissertation/Aim2/Paper/SubmissionPlosWater/SubmissionPlosWater2/Github")
data<-read.csv("CopingStrategyDataset.csv", stringsAsFactors = FALSE, sep=',') 
nrow(data) #1172

borbon <- data[which(data$community_factor=="Borbon"),]
nrow(borbon) #815

maldonado <- data[which(data$community_factor=="Maldonado"),]
nrow(maldonado) #183

timbire <- data[which(data$community_factor=="Timbire"),]
nrow(timbire) #174

################
###Analysis

# Table 1: Frequency Table
 #change to variables: age, sex_factor, race_factor, literacy_factor,
 #marriage_factor, ousesize, animalown, property_factor, energy_factor, income
 #waterperson_factor

frequencytable<- function ( factor, group){
  overall <- table(factor)
  overall_per<- prop.table(overall)*100
  bycomm <- table(factor, group)
  bycomm_per <- prop.table(table(factor, group), margin=2)*100
  print(overall)
  print(overall_per)
  print(bycomm)
  print(bycomm_per)
}

frequencytable(data$waterperson_factor, data$community_factor)

