#12/16/2020 
#This file is created for the paper titled
# "Consumer Willingness-to-Pay for Restaurant Surcharges to Reduce Carbon Emissions: Default and Information Effects"

# Setup:
rm(list = ls())
setwd("C:/Users/018792551.CAMPUS-DOMAIN/Dropbox/Green New Meal") # set working directory to folder where this script was opened, open from working directory
setwd("/Users/dedelong/Dropbox/Green New Meal") # set working directory to folder where this script was opened, open from working directory
library(dplyr)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Icens")

install.packages("DCchoice")
library(DCchoice)
library(lmtest)


#load data
load(file = "data.Rdata")

##############################################################################
# DBDC full model
# 
m1 <- R1 + R2 ~ age_num + female + hhsize + ethnicity + marriage + edu + income + dine_check + climate_info + remove + CFCI + CFCF + nep + 
  agency + agreeableness + openness +  neuroticism + extraversion + conscientiousness + 
  socialconservative + econconservative | log(bid1) + log(bid2)
mod1 <- dbchoice(m1, data = data)
mod1.sum <- summary(mod1)
mod1.sum
# WTP with Confidence Intervals (adjusted truncated mean and median value)
krCI(mod1) # Krinsky & Robb simulation

#Robustness check: Remove personality 

m2 <- R1 + R2 ~ age_num + female + hhsize + ethnicity + marriage + edu + income + dine_check + climate_info + remove + CFCI + CFCF + nep + 
  socialconservative + econconservative | log(bid1) + log(bid2) | log(bid1) + log(bid2)
mod2 <- dbchoice(m2, data = data)
mod2.sum <- summary(mod2)
mod2.sum

#DBDC restricted model: Remove environmental preferences, time preferences, personality, social views and economic views 

m3 <- R1 + R2 ~ age_num + female + hhsize + ethnicity + marriage + edu + income + dine_check + climate_info + remove  | log(bid1) + log(bid2)
mod3 <- dbchoice(m3, data = data)
mod3.sum <- summary(mod3)
mod3.sum
krCI(mod3)



##############################################################################
# Single-bounded dichotomous chocie (SBDC) full model

m4 <- R1 ~ age_num + female + hhsize + ethnicity + marriage + edu + income + dine_check + climate_info + remove + CFCI + CFCF + nep + 
  agency + agreeableness + openness +  neuroticism + extraversion + conscientiousness + 
  socialconservative + econconservative | log(bid1)
mod4 <- sbchoice(m4, data = data)
mod4.sum <- summary(mod4)
mod4.sum
krCI(mod4)

#SBDC restricted model without nep, personality, tp, social and economic views
m5 <- R1 ~ age_num + female + hhsize + ethnicity + marriage + edu + income + dine_check + climate_info + remove | log(bid1)
mod5 <- sbchoice(m5, data = data)
mod5.sum <- summary(mod5)
mod5.sum
krCI(mod5)

##############################################################################
# Calculate WTP for default and information nudge groups - DBDC full model: 
# 1. when remove = 1, climate_infor=0
krCI(mod1, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3", dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="0", remove = "1"))

# 2. when remove = 0, climate_infor=0
krCI(mod1, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="0", remove = "0"))

# 3. remove = 1, climate_info=1
krCI(mod1, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="1", remove = "1"))

# 4. remove = 0, climate_info=1
krCI(mod1, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="1", remove = "0"))

##############################################################################
# Calculate WTP for default and information nudge groups - DBDC restricted model wo nep, tp, personalities, social views, and econ views:
# 1. when remove = 1, climate_infor=0
krCI(mod3, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3", dine_check = "2", climate_info="0", remove = "1"))

# 2. remove = 1, climate_info=1
krCI(mod3, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="1", remove = "1"))

# 3. when remove = 0, climate_infor=0
krCI(mod3, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="0", remove = "0"))


# 4. remove = 0, climate_info=1
krCI(mod3, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="1", remove = "0"))

############################################################################################################################################################
# SBDC full model -  SBDC full model
# 1. when remove = 1, climate_infor=0
krCI(mod4, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3", dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="0", remove = "1"))

# 2. remove = 1, climate_info=1
krCI(mod4, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="1", remove = "1"))

# 3. when remove = 0, climate_infor=0
krCI(mod4, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="0", remove = "0"))

# 4. remove = 0, climate_info=1
krCI(mod4, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", CFCI = median(data$CFCI), CFCF=median(data$CFCF), 
                                   nep=median(data$nep), agency = median(data$agency), agreeableness= median(data$agreeableness), 
                                   openness = median(data$openness), neuroticism = median(data$neuroticism), extraversion = median(data$extraversion), 
                                   conscientiousness=median(data$conscientiousness), socialconservative = "4", 
                                   econconservative =  "4" , climate_info="1", remove = "0"))

##############################################################################
# Calculate WTP for default and information nudge groups - SBDC restricted model wo nep, tp, personalities, social views, and econ views:
# 1. when remove = 1, climate_infor=0
krCI(mod5, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3", dine_check = "2", climate_info="0", remove = "1"))

# 2. remove = 1, climate_info=1
krCI(mod5, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="1", remove = "1"))

# 3. when remove = 0, climate_infor=0
krCI(mod5, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="0", remove = "0"))


# 4. remove = 0, climate_info=1
krCI(mod5, individual = data.frame(age_num = median(data$age_num), female = "1", hhsize = median(data$hhsize), ethnicity = "1", marriage =  "2", 
                                   edu =  "4" , income =  "3",  dine_check = "2", climate_info="1", remove = "0"))
